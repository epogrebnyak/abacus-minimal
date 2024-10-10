"""Core double-entry accounting objects.

This module contains classes for:

  - chart of accounts (Chart)
  - general ledger (Ledger)
  - accounting entry (DoubleEntry, Entry)
  - reports (TrialBalance, IncomeStatement, BalanceSheet)
  - data exchange formats (BalancesDict)

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

Assumptions and simplifications (some may be relaxed in future versions): 

- one currency
- one level of accounts, no subaccounts
- account names must be globally unique (eg cannot have two "other" accounts)
- chart always has retained earnigns account
- other comprehensive income account (OCIA) not calculated 
- no journals, entries are posted to ledger directly
- an entry can touch any accounts
- entry amount can be positive or negative
- account balance cannot go negative
- net earnings are income less expenses, no gross profit or earnings before tax calculated    
- period end closing will transfer net earnings to retained earnings
- no cash flow statement
- no statement of changes in equity
- no date or any transaction metadata recorded
"""

import decimal
from abc import ABC, abstractmethod
from collections import UserDict
from dataclasses import dataclass, field
from enum import Enum

# some serialisation
from pathlib import Path
from typing import Dict, Iterator, Sequence, Type

from pydantic import BaseModel, ConfigDict, RootModel


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
        """All accounts in this chart including the duplicates."""
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
        """Raise error if any duplicate account names are found."""
        if len(self.to_dict()) < len(self.accounts):
            # FIXME: tell what account names are not unique in the error message
            raise AbacusError("Account names are not unique.")

    def dry_run(self):
        """Verify chart by making an empty ledger and try closing it."""
        self.to_ledger().close(chart=self)
        return self

    def to_dict(self) -> "ChartDict":
        """Create chart dictionary with unique account names."""
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
        """Add contra account to chart."""
        self.contra_accounts.setdefault(account_name, list()).append(contra_name)
        return self

    def name(self, account_name: str, title: str):
        """Add descriptive account title."""
        self.names[account_name] = title
        return self

    @property
    def closing_pairs(self) -> list[Pair]:
        """Return list of tuples that allows to close ledger at period end."""
        return list(self.to_dict().closing_pairs(self.retained_earnings))

    def open(self, opening_balances: dict[AccountName, Amount] | None = None):
        """Create ledger with some opening balances."""
        ledger = self.to_dict().to_ledger()
        if opening_balances:
            entry = make_opening_entry(self.to_dict(), opening_balances)
            ledger.post(entry)
        return ledger


def make_opening_entry(
    chart_dict: "ChartDict",
    opening_balances: dict[AccountName, Amount],
    title="Opening entry",
) -> "Entry":
    """Create and validate opening entry."""
    entry = Entry(title)
    for account_name, amount in opening_balances.items():
        if chart_dict.is_debit_account(account_name):
            entry.dr(account_name, amount)
        elif account_name in chart_dict.keys():
            entry.cr(account_name, amount)
    return entry.validate()


@dataclass
class Regular:
    """Regular account, holds information about account type."""

    t: T5


@dataclass
class Contra:
    """Contra account, refers to an existing regular account."""

    name: str


class ChartDict(UserDict[str, Regular | Contra]):
    """Dictionary of accounts with their definitions.
    A useful intermediate data structure between Chart and Ledger
    that ensures account names are unique.
    """

    def is_debit_account(self, account_name: AccountName) -> bool:
        """Return True if account is a debit account."""
        match self[account_name]:
            case Regular(t):
                return t in (T5.Asset, T5.Expense)
            case Contra(name):
                return not self.is_debit_account(name)
            case _:
                raise AbacusError()

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
        if account_name not in self.keys():
            raise KeyError(account_name)
        if self.is_debit_account(account_name):
            return DebitAccount
        return CreditAccount

    def to_ledger(self) -> "Ledger":
        """Create ledger."""
        return Ledger(
            {
                account_name: self.get_constructor(account_name)()
                for account_name in self.keys()
            }
        )

    def _close_contra_accounts(self, t: T5) -> Iterator[Pair]:
        """Close contra accounts, used for income or expense accounts."""
        for name in self.by_type(t):
            for contra_name in self.find_contra_accounts(name):
                yield contra_name, name

    def _close_to_retained_earnings(
        self, t: T5, retained_earnings_account: str
    ) -> Iterator[Pair]:
        """Close income and expense accounts to retained earnings account."""
        for name in self.by_type(t):
            yield name, retained_earnings_account

    def closing_pairs(self, retained_earnings_account: str) -> Iterator[Pair]:
        """Yield closing pairs for accounting period end."""
        for t in (T5.Income, T5.Expense):
            yield from self._close_contra_accounts(t)
            yield from self._close_to_retained_earnings(t, retained_earnings_account)

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
        self.debits.append((account_name, Amount(amount)))
        return self

    def cr(self, account_name, amount):
        """Add credit part to entry."""
        self.credits.append((account_name, Amount(amount)))
        return self

    def validate(self):
        """Raise error if sum of debits and sum credits are not equal."""
        if not self.is_balanced():
            raise error("Sum of debits does not equal to sum of credits", self)
        return self

    def is_balanced(self) -> bool:
        """Return True if sum of debits equals to sum credits."""

        def sums(xs):
            return sum(amount for _, amount in xs)

        return sums(self.debits) == sums(self.credits)


@dataclass
class DoubleEntry:
    """Create double entry with one debit and one credit entry."""

    title: str
    debit: AccountName
    credit: AccountName
    amount: Amount | int | float

    @property
    def entry(self) -> "Entry":
        return (
            Entry(self.title).dr(self.debit, self.amount).cr(self.credit, self.amount)
        )

    def __iter__(self):
        return iter(self.entry)


@dataclass
class TAccount(ABC):
    """Base class for T-account that holds amounts on the left and right sides.

    Parent class for:
      - DebitAccount
      - CreditAccount
    """

    left: Amount = Amount(0)
    right: Amount = Amount(0)

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

    def closing_entry(self, pair: Pair, title: str) -> "Entry":
        frm, to = pair
        if isinstance(self, DebitAccount):
            dr, cr = to, frm  # debit destination account
        elif isinstance(self, CreditAccount):
            dr, cr = frm, to  # credit destination account
        return DoubleEntry(title, dr, cr, self.balance).entry


class DebitAccount(TAccount):
    @property
    def balance(self) -> Amount:
        return self.left - self.right

    def credit(self, amount: Amount):
        if amount > self.balance:
            raise AbacusError(
                f"Account balance is {self.balance}, cannot credit {amount}."
            )
        self.right += amount

    @property
    def tuple(self):
        return self.balance, Amount(0)


class CreditAccount(TAccount):
    @property
    def balance(self) -> Amount:
        return self.right - self.left

    def debit(self, amount: Amount):
        if amount > self.balance:
            raise AbacusError(
                f"Account balance is {self.balance}, cannot debit {amount}."
            )
        self.left += amount

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
                "Posting should not make account balance negative",
                cannot_post,
            )

    def post_many(self, entries: Sequence[Entry]):
        """Post several streams of entries to ledger."""
        for entry in entries:
            self.post(entry)

    @property
    def trial_balance(self):
        """Create trial balance from ledger."""
        return TrialBalance({name: t_account.tuple for name, t_account in self.items()})

    @property
    def balances(self) -> dict[AccountName, Amount]:
        """Return account balances."""
        return {name: account.balance for name, account in self.items()}

    def net_balance(self, name: AccountName, contra_names: list[AccountName]) -> Amount:
        """Calculate net balance of an account by substracting the balances of its contra accounts."""
        return self[name].balance - sum(
            self[contra_name].balance for contra_name in contra_names
        )

    def close_by_pairs(self, pairs: Sequence[Pair], entry_title: str) -> list[Entry]:
        """Close ledger by using closing pairs of accounts."""
        closing_entries = []
        for pair in pairs:
            from_ = pair[0]
            entry = self.data[from_].closing_entry(pair, entry_title)
            closing_entries.append(entry)
            self.post(entry)
            del self.data[from_]
        return closing_entries

    def close(self, chart: Chart, entry_title="Closing entry") -> list[Entry]:
        """Close ledger at accounting period end."""
        return self.close_by_pairs(chart.closing_pairs, entry_title)

    def balance_sheet(self, chart: Chart):
        return BalanceSheet.new(chart, self)

    def income_statement(self, chart: Chart):
        return IncomeStatement.new(chart, self)


class TrialBalance(UserDict[str, tuple[Amount, Amount]]):
    """Trial balance contains account names and balances."""


def net_balances_factory(chart: Chart, ledger: Ledger):
    cd = chart.to_dict()

    def fill(t: T5):
        return {
            name: ledger.net_balance(name, cd.find_contra_accounts(name))
            for name in cd.by_type(t)
        }

    return fill


class Report(BaseModel):
    """Base class for financial reports."""


class IncomeStatement(Report):
    income: dict[AccountName, Amount]
    expenses: dict[AccountName, Amount]

    @classmethod
    def new(
        cls,
        chart: Chart,
        ledger: Ledger,
    ):
        """Create income statement from ledger and chart."""
        fill = net_balances_factory(chart, ledger)
        return cls(income=fill(T5.Income), expenses=fill(T5.Expense))

    @property
    def net_earnings(self):
        """Calculate net earnings as income less expenses."""
        return sum(self.income.values()) - sum(self.expenses.values())


class BalanceSheet(Report):
    assets: dict[AccountName, Amount]
    capital: dict[AccountName, Amount]
    liabilities: dict[AccountName, Amount]

    @classmethod
    def new(cls, chart: Chart, ledger: Ledger):
        """Create balance sheet from ledger and chart.
        Account will balances will be shown net of contra account balances."""
        fill = net_balances_factory(chart, ledger)
        return cls(
            assets=fill(T5.Asset),
            capital=fill(T5.Capital),
            liabilities=fill(T5.Liability),
        )


class BalancesDict(RootModel[Dict[str, Amount]]):
    def save(self, filename):
        Path(filename).write_text(self.model_dump_json())

    @classmethod
    def load(cls, filename):
        return cls.model_validate_json(Path(filename).read_text())
