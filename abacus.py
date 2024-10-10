"""Core double-entry accounting objects.

This module contains classes for:

  - chart of accounts (Chart)
  - general ledger (Ledger)
  - accounting  entry (Entry)
  - reports (TrialBalance, IncomeStatement, BalanceSheet)

Accounting workflow:

1. create chart of accounts and set retained earnings account
2. create ledger from chart with opening balances
3. post entries to ledger
4. show trial balance at any time
5. show proxy income statement if necessary 
6. close ledger at accounting period end and make income statement
7. make post-close entries and make balance sheet
8. save permanent account balances for next period  

Accounting conventions:

- regular accounts of five types (asset, liability, capital, income, expense)
- contra accounts to regular accounts are possible (eg depreciation, discounts, etc.)
- intermediate income summary account used for net income calculation

Assumptions and simplifications (some may be relaxed in future versions): 

- one currency
- one level of accounts, no subaccounts
- account names must be globally unique
- chart always has retained earnigns account
- chart always has income summary account
- no other comprehensive income account (OCIA) 
- no journals, entries are posted to ledger directly
- an entry can touch any accounts
- entry amount can be positive or negative
- account balance cannot go negative
- net earnings are income less expenses, no gross profit or earnings before tax calculated    
- period end closing will transfer net earnings to retained earnings
- no cash flow statement
- no statement of changes in equity
- no date or transaction metadata recorded
"""

import decimal
from abc import ABC, abstractmethod
from collections import UserDict
from dataclasses import dataclass, field
from enum import Enum
from typing import Iterator, Sequence, Type

from pydantic import BaseModel, ConfigDict


class AbacusError(Exception):
    """Custom error for the abacus project."""


def error(message: str, data):
    return AbacusError([message, data])


AccountName = str
Amount = decimal.Decimal
Pair = tuple[AccountName, AccountName]


class T5(Enum):
    """Five types of accounts."""

    Asset = "asset"
    Liability = "liability"
    Capital = "capital"
    Income = "income"
    Expense = "expense"

    @property
    def t_account(self):
        if self in (T5.Asset, T5.Expense):
            return DebitAccount
        return CreditAccount


class Chart(BaseModel):
    """Serializable chart of accounts."""

    model_config = ConfigDict(extra="forbid")

    retained_earnings: str
    assets: list[str] = []
    capital: list[str] = []
    liabilities: list[str] = []
    income: list[str] = []
    expenses: list[str] = []
    contra_accounts: dict[str, list[str]] = {}
    names: dict[str, str] = {}

    def __post_init__(self):
        self.assert_unique()
        self.dry_run()

    @property
    def accounts(self):
        return (
            self.assets
            + self.capital
            + self.liabilities
            + self.income
            + self.expenses
            + [self.retained_earnings]
            + sum(self.contra_accounts.values(), [])  # rather unusual
        )

    def assert_unique(self):
        """Check if all account names are unique."""
        if len(self.to_dict()) < len(self.accounts):
            # FIXME: tell what account names are not unique in error message
            raise AbacusError("Account names are not unique.")

    def dry_run(self):
        """Verify chart by making an empty ledger and try closing it."""
        self.to_ledger().close(chart=self)
        return self

    def to_dict(self) -> "ChartDict":
        chart_dict = ChartDict()
        for t, attr in (
            (T5.Asset, "assets"),
            (T5.Liability, "liabilities"),
            (T5.Capital, "capital"),
            (T5.Income, "income"),
            (T5.Expense, "expenses"),
        ):
            for account_name in getattr(self, attr):
                chart_dict.set(t, account_name)
        chart_dict.set(T5.Capital, self.retained_earnings)
        # all regular accounts are now added, adding contra accounts
        for account_name, contra_names in self.contra_accounts.items():
            for contra_name in contra_names:
                chart_dict.offset(account_name, contra_name)
        return chart_dict

    def offset(self, account_name: str, contra_name: str):
        self.contra_accounts.setdefault(account_name, list()).append(contra_name)
        return self

    def name(self, account_name: str, title: str):
        self.names[account_name] = title
        return self

    @property
    def closing_pairs(self):
        return list(self.to_dict().closing_pairs(self.retained_earnings))

    def to_ledger(self):
        return self.to_dict().to_ledger()


class Definition(ABC):
    pass


@dataclass
class Regular(Definition):
    """Regular account, holds information about account type."""

    t: T5


@dataclass
class Contra(Definition):
    """Contra account, refers to an existing regular account."""

    name: str


class ChartDict(UserDict[str, Definition]):
    """Dictionary of accounts with their definitions.
    A useful intermediate data structure between Chart and Ledger.
    """

    def set(self, t: T5, account_name: str):
        """Add regular account."""
        self[account_name] = Regular(t)
        return self

    def offset(self, account_name: str, contra_account_name: str):
        """Add contra account."""
        if account_name not in self:
            raise AbacusError(f"Account not found in chart: {account_name}")
        self[contra_account_name] = Contra(account_name)
        return self

    def get_constructor(self, account_name: str) -> Type["TAccount"]:
        """Return T-account class constructor for a given account name."""
        match self[account_name]:
            case Regular(t):
                return t.t_account
            case Contra(name):
                return self.get_constructor(name).reverse()
        raise KeyError(account_name)

    def to_ledger(self) -> "Ledger":
        """Create ledger."""
        return Ledger(
            {
                account_name: self.get_constructor(account_name)()
                for account_name in self.keys()
            }
        )

    def _closing_pairs_by_type(
        self, t: T5, retained_earnings_account: str
    ) -> Iterator[Pair]:
        # Close contra income and contra expense accounts.
        for name in self.by_type(t):
            for contra_name in self.find_contra_accounts(name):
                yield contra_name, name

        # Close income and expense accounts to retained earnings account.
        for name in self.by_type(t):
            yield name, retained_earnings_account

    def closing_pairs(self, retained_earnings_account: str) -> Iterator[Pair]:
        """Yield closing pairs for accounting period end."""
        yield from self._closing_pairs_by_type(T5.Income, retained_earnings_account)
        yield from self._closing_pairs_by_type(T5.Expense, retained_earnings_account)

    def by_type(self, t: T5) -> list[AccountName]:
        """Return account names for a given account type."""
        return [name for name, _t in self.items() if _t == Regular(t)]

    def find_contra_accounts(self, name: AccountName) -> list[AccountName]:
        """Find contra accounts for a given account name."""
        return [
            contra_name for contra_name, _name in self.items() if _name == Contra(name)
        ]


@dataclass
class SingleEntry(ABC):
    """Base class for a single entry changes either a debit or a credit side of an account."""

    name: AccountName
    amount: Amount


class DebitEntry(SingleEntry):
    """An entry that increases the debit side of an account."""


class CreditEntry(SingleEntry):
    """An entry that increases the credit side of an account."""


@dataclass
class Entry:
    title: str
    debits: list[tuple[AccountName, Amount]] = field(default_factory=list)
    credits: list[tuple[AccountName, Amount]] = field(default_factory=list)

    def __iter__(self) -> Iterator[SingleEntry]:
        for name, amount in self.debits:
            yield DebitEntry(name, amount)
        for name, amount in self.credits:
            yield CreditEntry(name, amount)

    def dr(self, account_name, amount):
        """Add debit part to entry."""
        self.debits.append((account_name, amount))
        return self

    def cr(self, account_name, amount):
        """Add credit part to entry."""
        self.credits.append((account_name, amount))
        return self

    def validate_balance(self):
        """Raise error if sum of debits and sum credits are not equal."""
        if not self.is_balanced():
            raise error("Sum of debits does not equal to sum of credits", self)
        return self

    def is_balanced(self) -> bool:
        """Return True if sum of debits equals to sum credits."""

        def sums(xs):
            return sum(amount for _, amount in xs)

        return sums(self.debits) == sums(self.credits)


def double_entry(
    title: str, debit: AccountName, credit: AccountName, amount: Amount | int | float
) -> Entry:
    """Create double entry with one debit and one credit entry."""
    amount = Amount(amount)
    return Entry(title).dr(debit, amount).cr(credit, amount)


@dataclass
class TAccount(ABC):
    """Base class for T-account that holds amounts on the left and right sides.

    Parent class for:
      - DebitAccount
      - CreditAccount
    """

    left: Amount = Amount(0)
    right: Amount = Amount(0)

    @classmethod
    def reverse(self):
        pass

    def copy(self):
        return self.__class__(left=self.left, right=self.right)

    def debit(self, amount: Amount):
        """Add amount to debit side of T-account."""
        self.left += amount

    def credit(self, amount: Amount):
        """Add amount to credit side of T-account."""
        self.right += amount

    @property
    @abstractmethod
    def balance(self) -> Amount:
        pass

    @abstractmethod
    def closing_entry(self, from_: AccountName, to_: AccountName) -> "Entry":
        pass


class DebitAccount(TAccount):

    @classmethod
    def reverse(cls):
        return CreditAccount

    @property
    def balance(self) -> Amount:
        return self.left - self.right

    def credit(self, amount: Amount):
        if amount > self.balance:
            raise AbacusError(
                f"Account balance is {self.balance}, cannot credit {amount}."
            )
        self.right += amount

    def closing_entry(
        self, from_: AccountName, to_: AccountName, title: str
    ) -> "Entry":
        return double_entry(title, to_, from_, self.balance)

    @property
    def tuple(self):
        return self.balance, Amount(0)


class CreditAccount(TAccount):

    @classmethod
    def reverse(cls):
        return DebitAccount

    @property
    def balance(self) -> Amount:
        return self.right - self.left

    def debit(self, amount: Amount):
        if amount > self.balance:
            raise AbacusError(
                f"Account balance is {self.balance}, cannot debit {amount}."
            )
        self.left += amount

    def closing_entry(
        self, from_: AccountName, to_: AccountName, title: str
    ) -> "Entry":
        return double_entry(title, from_, to_, self.balance)

    @property
    def tuple(self):
        return Amount(0), self.balance


class Ledger(UserDict[AccountName, TAccount]):

    def post_single(self, single_entry: SingleEntry):
        """Post single entry to ledger. Will raise `KeyError` if account name is not found."""
        match single_entry:
            case DebitEntry(name, amount):
                self.data[name].debit(Amount(amount))
            case CreditEntry(name, amount):
                self.data[name].credit(Amount(amount))

    def post(self, entry: Entry):
        """Post a stream of single entries to ledger."""
        not_found = []
        cannot_post = []
        for single_entry in iter(entry):
            try:
                self.post_single(single_entry)
            except KeyError as e:
                not_found.append((e, single_entry))
            except AbacusError as e:
                cannot_post.append((e, single_entry))
        if not_found:
            raise error("Accounts do not exist", not_found)
        if cannot_post:
            raise error(
                "Posting will make account balance negative",
                cannot_post,
            )

    def post_many(self, entries: Sequence[Entry]):
        """Post several streams of entries to ledger."""
        for entry in entries:
            self.post(entry)

    @property
    def trial_balance(self):
        """Create trial balance from ledger."""
        return TrialBalance({name: taccount.tuple for name, taccount in self.items()})

    @property
    def balances(self) -> dict[AccountName, Amount]:
        """Return account balances."""
        return {name: account.balance for name, account in self.items()}

    #     def net_balance(self, name: AccountName, contra_names: list[AccountName]) -> Amount:
    #         """Calculate net balance of an account by substracting the balances of its contra accounts."""
    #         return self[name].balance - sum(
    #             self[contra_name].balance for contra_name in contra_names
    #         )

    def close_by_pairs(self, pairs: Sequence[Pair], entry_title: str) -> list[Entry]:
        """Close ledger by using closing pairs of accounts."""
        closing_entries = []
        for from_, to_ in pairs:
            entry = self.data[from_].closing_entry(from_, to_, entry_title)
            closing_entries.append(entry)
            self.post(entry)
            del self.data[from_]
        return closing_entries

    def close(self, chart: Chart, entry_title="Closing entry") -> list[Entry]:
        """Close ledger at accounting period end."""
        return self.close_by_pairs(chart.closing_pairs, entry_title)


class TrialBalance(UserDict[str, tuple[Amount, Amount]]):
    """Trial balance contains account names and balances."""


# class Report(BaseModel):
#     """Base class for financial reports."""


# class IncomeStatement(Report):
#     income: dict[AccountName, Amount]
#     expenses: dict[AccountName, Amount]

#     @classmethod
#     def new(cls, ledger: Ledger, chart: Chart):
#         """Create income statement from ledger and chart."""
#         fill = chart.net_balances_factory(ledger)
#         return cls(income=fill(T5.Income), expenses=fill(T5.Expense))

#     @property
#     def net_earnings(self):
#         """Calculate net earnings as income less expenses."""
#         return sum(self.income.values()) - sum(self.expenses.values())


# class BalanceSheet(Report):
#     assets: dict[AccountName, Amount]
#     capital: dict[AccountName, Amount]
#     liabilities: dict[AccountName, Amount]

#     @classmethod
#     def new(cls, ledger: Ledger, chart: Chart):
#         """Create balance sheet from ledger and chart.
#         Account will balances will be shown net of contra account balances."""
#         fill = chart.net_balances_factory(ledger)
#         return cls(
#             assets=fill(T5.Asset),
#             capital=fill(T5.Capital),
#             liabilities=fill(T5.Liability),
#         )
