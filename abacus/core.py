"""Core double-entry accounting objects.

This module contains classes for:

  - chart of accounts (Chart)
  - general ledger (Ledger)
  - accounting entry (DoubleEntry, Entry)
  - account summaries (TrialBalance, BalancesDict), and
  - reports (IncomeStatement, BalanceSheet)

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
- account balance cannot go negative

Assumptions and simplifications (some may be relaxed in future versions):

- one currency
- one level of accounts, no sub-accounts
- account names must be globally unique (eg cannot have two "other" accounts)
- chart always has retained earnings account
- no account durations (current vs non-current)
- other comprehensive income account (OCIA) not calculated
- no journals, entries are posted to ledger directly
- an entry can touch any accounts
- entry amount can be positive or negative
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
from typing import Iterator, Sequence


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


class ChartDict(UserDict[str, Regular | Contra]):
    """Dictionary of accounts with their definitions.
    This ia a useful intermediate data structure between Chart and Ledger
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
        self[account_name] = Regular(t)
        return self

    def offset(self, account_name: str, contra_account_name: str):
        """Add contra account."""
        if account_name not in self.keys():
            raise AbacusError(f"Account not found in chart: {account_name}")
        self[contra_account_name] = Contra(account_name)
        return self

    def t_account(self, account_name: str) -> "TAccount":
        """Return T-account for a given account name."""
        return (
            DebitAccount() if self.is_debit_account(account_name) else CreditAccount()
        )

    def to_ledger(self) -> "Ledger":
        """Create ledger."""
        return Ledger(
            {account_name: self.t_account(account_name) for account_name in self.keys()}
        )

    def closing_pairs(self, retained_earnings_account: str) -> Iterator[Pair]:
        """Yield closing pairs for accounting period end."""

        def close_account(name: AccountName):
            for contra_name in self.find_contra_accounts(name):
                yield contra_name, name
            yield name, retained_earnings_account

        for t in (T5.Income, T5.Expense):
            for account_name in self.by_type(t):
                yield from close_account(account_name)

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


def sums(xs):
    return sum(amount for _, amount in xs)


@dataclass
class MultipleEntry:
    """A multiple entry is similar to a list of single entries
    that can be iterated over to post to ledger.

    For a valid entry the sum of debits equals the sum of credits.
    """

    title: str
    debits: list[tuple[AccountName, Amount]] = field(default_factory=list)
    credits: list[tuple[AccountName, Amount]] = field(default_factory=list)

    def __iter__(self) -> Iterator[SingleEntry]:
        """A multiple entry is behaves like a list of single entries
        that can be iterated when posting to ledger."""
        for name, amount in self.debits:
            yield DebitEntry(name, amount)
        for name, amount in self.credits:
            yield CreditEntry(name, amount)

    def validate(self):
        """Raise error if sum of debits and sum credits are not equal."""
        a = sums(self.debits)
        b = sums(self.credits)
        if a != b:
            raise AbacusError("Sum of debits {a} does not equal to sum of credits {b}.")
        return self


Numeric = int | float | Amount


@dataclass
class Entry(MultipleEntry):
    """An Entry class is a user interface for creating a double or multiple entry
    and also an opening or a closing entry.
    """

    is_closing: bool = False
    _current_amount: Amount | None = None

    def double(self, debit: str, credit: str, amount: Numeric):
        """Create double entry."""
        if self.debits or self.credits:
            raise AbacusError("Cannot create double entry.")
        return self.debit(debit, amount).credit(credit, amount)

    def amount(self, amount: Numeric):
        """Set amount for the entry."""
        self._current_amount = Amount(amount)
        return self

    def _get_amount(self, amount: Numeric | None = None) -> Amount:
        """Use provided amount, default amount or raise error if no suffient data."""
        if amount is None:
            if self._current_amount:
                return self._current_amount
            else:
                raise AbacusError("Amount is not set.")
        return Amount(amount)

    def debit(self, account_name: str, amount: Numeric | None = None):
        """Add debit part to entry."""
        self.debits.append((account_name, self._get_amount(amount)))
        return self

    def credit(self, account_name: str, amount: Numeric | None = None):
        """Add credit part to entry."""
        self.credits.append((account_name, self._get_amount(amount)))
        return self

    def opening(
        self, opening_balances: dict[AccountName, Amount], chart_dict: "ChartDict"
    ):
        """Create opening entry."""
        for account_name, amount in opening_balances.items():
            if chart_dict.is_debit_account(account_name):
                self.debit(account_name, amount)
            elif account_name in chart_dict.keys():
                self.credit(account_name, amount)
            else:
                raise AbacusError(f"Account not found in chart: {account_name}")
        self.validate()
        return self

    def transfer(self, frm: AccountName, to: AccountName, t_account: "TAccount"):
        """Make entry to transfer account balances from one account to another."""
        if isinstance(t_account, DebitAccount):
            dr, cr = to, frm  # debit destination account
        elif isinstance(t_account, CreditAccount):
            dr, cr = frm, to  # credit destination account
        self.is_closing = True
        self.debit(dr, b := t_account.balance)
        self.credit(cr, b)
        return self


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

    def closing_entry(self, frm: AccountName, to: AccountName, title: str) -> "Entry":
        """Make closing entry to transfer balance to another account."""
        return Entry(title).transfer(frm, to, self)


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


class Ledger(UserDict[AccountName, TAccount]):
    @classmethod
    def empty(cls, chart_dict: ChartDict):
        """Create empty ledger from chart dictionary."""
        return chart_dict.to_ledger()

    @classmethod
    def open(
        cls,
        chart_dict: ChartDict,
        opening_balances: dict,
        opening_entry_title="Opening entry",
    ):
        """Create ledger using starting balances."""
        self = cls.empty(chart_dict)
        entry = Entry(opening_entry_title).opening(opening_balances, chart_dict)
        self.post(entry)
        return self

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
            raise AbacusError(f"Accounts do not exist: {not_found}")
        if cannot_post:
            raise AbacusError(
                f"Forbidden to make account balances negative: {cannot_post}"
            )

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

    def close_by_pairs(self, pairs: Sequence[Pair], entry_title: str) -> list[Entry]:
        """Close ledger by using closing pairs of accounts.
        This method changes the existing ledger."""
        closing_entries = []
        for frm, to in pairs:
            entry = self.data[frm].closing_entry(frm, to, entry_title)
            closing_entries.append(entry)
            self.post(entry)
            del self.data[frm]
        return closing_entries

    def close(
        self, closing_pairs: Sequence[Pair], entry_title="Closing entry"
    ) -> list[Entry]:
        """Close ledger at accounting period end."""
        return self.close_by_pairs(closing_pairs, entry_title)

    def balance_sheet(self, chart_dict: "ChartDict"):
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
    """Create a function that calculates net balances for a given account type
    based on the provided chart and ledger."""

    def fill(t: T5):
        return {
            name: ledger.net_balance(name, chart_dict.find_contra_accounts(name))
            for name in chart_dict.by_type(t)
        }

    return fill


class Report:
    """Base class for financial reports."""


@dataclass
class IncomeStatement(Report):
    income: dict[AccountName, Amount]
    expenses: dict[AccountName, Amount]

    @property
    def net_earnings(self):
        """Calculate net earnings as income less expenses."""
        return sum(self.income.values()) - sum(self.expenses.values())


@dataclass
class BalanceSheet(Report):
    assets: dict[AccountName, Amount]
    capital: dict[AccountName, Amount]
    liabilities: dict[AccountName, Amount]
