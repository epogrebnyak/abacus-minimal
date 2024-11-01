"""Core accounting objects.

This module contains classes for:

  - chart of accounts (ChartDict),
  - general ledger (Ledger),
  - accounting entry (Posting),
  - summaries (TrialBalance) and financial reports (IncomeStatement, BalanceSheet).

Accounting workflow:

1. create chart of accounts and set current and retained earnings accounts
2. create ledger from chart with opening balances
3. post entries to ledger
4. show trial balance at any time
5. show proxy income statement and balance sheet if necessary
6. close ledger at accounting period end and make income statement
7. make post-close entries and make balance sheet
8. save permanent account balances for next period

Accounting conventions:

- regular accounts of five types (asset, liability, capital, income, expense),
- contra accounts to regular accounts are possible (eg depreciation, discounts, etc.).

Assumptions and simplifications (some may be relaxed in future versions):

- one currency
- one reporting period
- one level of accounts, no sub-accounts
- account names must be globally unique (eg cannot have two accounts named "other")
- chart always has current account and retained earnings account
- no account durations (current vs non-current)
- other comprehensive income account (OCIA) not calculated
- no journals, entries are posted to ledger directly
- an entry can touch any accounts
- entry amount can be positive, negative or even zero
- net earnings are income less expenses, no gross profit or earnings before tax calculated
- period end closing will transfer current earnings to retained earnings
- no cash flow statement
- no statement of changes in equity
- no date or any transaction metadata recorded
"""

import decimal
from abc import ABC, abstractmethod
from collections import UserDict
from dataclasses import dataclass
from enum import Enum
from typing import Iterator, Sequence, Type


class AbacusError(Exception):
    """Custom error for the abacus project."""


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


@dataclass
class Regular:
    """Regular account, holds information about account type."""

    t: T5


@dataclass
class Contra:
    """Contra account, refers to an existing regular account."""

    name: str


# suggestion: may shut down __setitem__ for this class
class ChartDict(UserDict[str, Regular | Contra]):
    """Dictionary of accounts with their definitions.
    This is an intermediate data structure between Chart and Ledger
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
                raise AbacusError()  # mypy wants it

    def set(self, t: T5, account_name: str):
        """Add regular account."""
        self.data[account_name] = Regular(t)
        return self

    def offset(self, account_name: str, contra_account_name: str):
        """Add contra account."""
        if account_name not in self.keys():
            raise AbacusError(f"Account not found in chart: {account_name}")
        self.data[contra_account_name] = Contra(account_name)
        return self

    def t_account_class(self, account_name: str) -> Type["TAccount"]:
        """Return T-account class for a given account name."""
        return DebitAccount if self.is_debit_account(account_name) else CreditAccount

    def to_ledger(self) -> "Ledger":
        """Create ledger."""
        return Ledger(
            {
                account_name: self.t_account_class(account_name)()
                for account_name in self.keys()
            }
        )

    def opening_entry(self, opening_balances: dict) -> "Posting":
        """Create opening entry."""
        entry: Posting = []
        for account_name, amount in opening_balances.items():
            if self.is_debit_account(account_name):
                entry.append(Debit(account_name, amount))
            elif account_name in self.keys():
                entry.append(Credit(account_name, amount))
            else:
                raise AbacusError(f"Account not found in chart: {account_name}")
        raise_if_not_balanced(entry)
        return entry

    def closing_pairs(self, earnings_account: AccountName) -> Iterator[Pair]:
        """Yield closing pairs for accounting period end.
        The closing pairs can poitn to the current earnings or retained earnings account.
        """

        def close(account_name: AccountName):
            for contra_name in self.find_contra_accounts(account_name):
                yield contra_name, account_name
            yield account_name, earnings_account

        for t in (T5.Income, T5.Expense):
            for account_name in self.by_type(t):
                yield from close(account_name)

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


class Debit(SingleEntry):
    """An entry that increases the debit side of an account."""


class Credit(SingleEntry):
    """An entry that increases the credit side of an account."""


Posting = list[Debit | Credit]


def sum_of(posting: Posting, cls) -> Posting:
    """Return debit or credit amount sum."""
    return sum(entry.amount for entry in posting if isinstance(entry, cls))


def is_balanced(posting: Posting) -> bool:
    """Return True if posting is balanced."""
    return sum_of(posting, Debit) == sum_of(posting, Credit)


def raise_if_not_balanced(posting: Posting):
    """Raise error if posting is not balanced."""
    if not is_balanced(posting):
        raise AbacusError(f"Posting is not balanced: {posting}")


def double(debit: str, credit: str, amount: Amount) -> Posting:
    """Create double entry."""
    return [Debit(debit, amount), Credit(credit, amount)]


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

    def apply(self, single_entry: SingleEntry):
        """Apply single entry to T-account."""
        if isinstance(single_entry, Debit):
            self.debit(single_entry.amount)
        elif isinstance(single_entry, Credit):
            self.credit(single_entry.amount)

    @abstractmethod
    def transfer(self, frm: AccountName, to: AccountName) -> Posting:
        pass

    @property
    @abstractmethod
    def balance(self) -> Amount:
        pass


class DebitAccount(TAccount):
    @property
    def balance(self) -> Amount:
        return self.left - self.right

    def transfer(self, frm: AccountName, to: AccountName):
        return double(to, frm, self.balance)


class CreditAccount(TAccount):
    @property
    def balance(self) -> Amount:
        return self.right - self.left

    def transfer(self, frm: AccountName, to: AccountName):
        return double(frm, to, self.balance)


class Ledger(UserDict[AccountName, TAccount]):
    @classmethod
    def empty(cls, chart_dict: ChartDict):
        """Create empty ledger from chart dictionary."""
        return chart_dict.to_ledger()

    def post_single(self, single_entry: SingleEntry) -> None:
        """Post single entry to ledger. Will raise `KeyError` if account name is not found."""
        self.data[single_entry.name].apply(single_entry)

    def assert_key(self, key: AccountName) -> None:
        if key not in self.keys():
            raise AbacusError(f"Account not found in ledger: {key}")

    def post(self, entry: Posting) -> None:
        """Post entry to ledger."""
        raise_if_not_balanced(entry)
        for single_entry in entry:
            self.assert_key(single_entry.name)
        for single_entry in entry:
            self.post_single(single_entry)

    def is_closed(self, chart_dict: ChartDict) -> bool:
        for frm, _ in chart_dict.closing_pairs("_"):
            if frm in self.data.keys():
                return False
        return True

    @property
    def trial_balance(self):
        """Create trial balance from ledger."""

        def as_tuple(t_account):
            b = t_account.balance
            return (b, None) if isinstance(t_account, DebitAccount) else (None, b)

        return TrialBalance(
            {name: as_tuple(t_account) for name, t_account in self.items()}
        )

    @property
    def balances(self) -> dict[AccountName, Amount]:
        """Return account balances."""
        return {name: account.balance for name, account in self.items()}

    def net_balance(self, name: AccountName, contra_names: list[AccountName]) -> Amount:
        """Calculate net balance of an account by deducting the balances of its contra accounts."""
        return self[name].balance - sum(
            self[contra_name].balance for contra_name in contra_names
        )

    def close_one(self, frm: AccountName, to: AccountName) -> Posting:
        """Close account and move its balances to another account."""
        entry = self.data[frm].transfer(frm, to)
        self.post(entry)
        del self.data[frm]
        return entry

    def close(self, closing_pairs: Sequence[Pair]) -> list[Posting]:
        """Close ledger at accounting period end."""
        return [self.close_one(frm, to) for frm, to in closing_pairs]

    def balance_sheet(self, chart_dict: ChartDict):
        """Create balance sheet from ledger."""
        fill = net_balances_factory(chart_dict, self)
        return BalanceSheet(
            assets=fill(T5.Asset),
            capital=fill(T5.Capital),
            liabilities=fill(T5.Liability),
        )

    def income_statement(self, chart_dict: ChartDict):
        """Create income statement from ledger."""
        fill = net_balances_factory(chart_dict, self)
        return IncomeStatement(income=fill(T5.Income), expenses=fill(T5.Expense))


class TrialBalance(UserDict[str, tuple[Amount, Amount]]):
    """Trial balance contains account names and balances."""


def net_balances_factory(chart_dict: ChartDict, ledger: Ledger):
    """Create a function that calculates net balances
    for a given account type based on the provided chart and ledger.
    """

    def fill(t: T5):
        result = {}
        for name in chart_dict.by_type(t):
            try:
                contra_accounts = chart_dict.find_contra_accounts(name)
                result[name] = ledger.net_balance(name, contra_accounts)
            except KeyError:  # protect from current account not in ledger
                pass
        return result

    return fill


class Report:
    """Base class for financial reports."""


def sum_values(d: dict):
    return sum(d.values())


@dataclass
class IncomeStatement(Report):
    income: dict[AccountName, Amount]
    expenses: dict[AccountName, Amount]

    @property
    def net_earnings(self):
        """Calculate net earnings as income less expenses."""
        return sum_values(self.income) - sum_values(self.expenses)


@dataclass
class BalanceSheet(Report):
    assets: dict[AccountName, Amount]
    capital: dict[AccountName, Amount]
    liabilities: dict[AccountName, Amount]

    def is_balanced(self) -> bool:
        """Return True if assets equal liabilities plus capital."""
        return sum_values(self.assets) == (
            sum_values(self.capital) + sum_values(self.liabilities)
        )
