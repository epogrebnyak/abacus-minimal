"""Core double-entry accounting objects.

This module contains classes for:

  - chart of accounts (Chart)
  - general ledger (Ledger)
  - accounting entry (DoubleEntry, Entry)
  - summaries and reports (TrialBalance, BalancesDict, IncomeStatement, BalanceSheet)

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
from pathlib import Path
from typing import Dict, Iterator, Sequence

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


class SaveLoadMixin:
    """Class for loading and saving Pydantic models to files."""

    @classmethod
    def load(cls, filename: str | Path):
        return cls.model_validate_json(Path(filename).read_text())  # type: ignore

    def save(self, filename: str | Path):
        Path(filename).write_text(self.model_dump_json(indent=2))  # type: ignore


class Chart(BaseModel, SaveLoadMixin):
    """Chart of accounts."""

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
            + sum(self.contra_accounts.values(), [])
        )

    @property
    def duplicates(self):
        """Duplicate account names. Must be empty for valid chart."""
        names = self.accounts
        for name in self.to_dict().keys():
            names.remove(name)
        return names

    def assert_unique(self):
        """Raise error if any duplicate account names are found."""
        if names := self.duplicates:
            raise error("Account names are not unique", names)

    def dry_run(self):
        """Verify chart by making an empty ledger and try closing it."""
        self.to_ledger().close(chart=self)

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
        # all regular accounts are added, now adding contra accounts
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

    def open(
        self, opening_balances: dict[AccountName, Amount] | None = None
    ) -> "Ledger":
        """Create ledger with opening balances."""
        ledger = self.to_dict().to_ledger()
        if opening_balances:
            entry = make_opening_entry(opening_balances, self.to_dict())
            ledger.post(entry)
        return ledger


def make_opening_entry(
    opening_balances: dict[AccountName, Amount],
    chart_dict: "ChartDict",
    title="Opening entry",
) -> "Entry":
    """Create and validate opening entry."""
    entry = Entry(title)
    for account_name, amount in opening_balances.items():
        if chart_dict.is_debit_account(account_name):
            entry.debit(account_name, amount)
        elif account_name in chart_dict.keys():
            entry.credit(account_name, amount)
        else:
            raise AbacusError(f"Account not found in chart: {account_name}")
    entry.validate()
    return entry


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
    is_closing: bool = False
    amount: Amount | int | float | None = None

    def __iter__(self) -> Iterator[SingleEntry]:
        for name, amount in self.debits:
            yield DebitEntry(name, amount)
        for name, amount in self.credits:
            yield CreditEntry(name, amount)

    def debit(self, account_name, amount=None):
        """Add debit part to entry."""
        if amount is None:
            amount = self.amount
        self.debits.append((account_name, Amount(amount)))
        return self

    def credit(self, account_name, amount=None):
        """Add credit part to entry."""
        if amount is None:
            amount = self.amount
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
        """Make closing entry to transfer balance to another account."""
        frm, to = pair
        if isinstance(self, DebitAccount):
            dr, cr = to, frm  # debit destination account
        elif isinstance(self, CreditAccount):
            dr, cr = frm, to  # credit destination account
        return (
            Entry(title, is_closing=True)
            .debit(dr, self.balance)
            .credit(cr, self.balance)
        )


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
                "Forbidden to make account balance negative",
                cannot_post,
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
        for pair in pairs:
            frm, _ = pair
            entry = self.data[frm].closing_entry(pair, entry_title)
            closing_entries.append(entry)
            self.post(entry)
            del self.data[frm]
        return closing_entries

    def close(self, chart: Chart, entry_title="Closing entry") -> list[Entry]:
        """Close ledger at accounting period end."""
        return self.close_by_pairs(chart.closing_pairs, entry_title)

    def balance_sheet(self, chart: Chart):
        """Create balance sheet from ledger."""
        fill = net_balances_factory(chart, self)
        return BalanceSheet(
            assets=fill(T5.Asset),
            capital=fill(T5.Capital),
            liabilities=fill(T5.Liability),
        )

    def income_statement(self, chart: Chart):
        """Create income statement from ledger."""
        fill = net_balances_factory(chart, self)
        return IncomeStatement(income=fill(T5.Income), expenses=fill(T5.Expense))


class TrialBalance(UserDict[str, tuple[Amount, Amount]]):
    """Trial balance contains account names and balances."""


def net_balances_factory(chart: Chart, ledger: Ledger):
    chart_dict = chart.to_dict()

    def fill(t: T5):
        return {
            name: ledger.net_balance(name, chart_dict.find_contra_accounts(name))
            for name in chart_dict.by_type(t)
        }

    return fill


class Report(BaseModel):
    """Base class for financial reports."""


class IncomeStatement(Report):
    income: dict[AccountName, Amount]
    expenses: dict[AccountName, Amount]

    @property
    def net_earnings(self):
        """Calculate net earnings as income less expenses."""
        return sum(self.income.values()) - sum(self.expenses.values())


class BalanceSheet(Report):
    assets: dict[AccountName, Amount]
    capital: dict[AccountName, Amount]
    liabilities: dict[AccountName, Amount]


class BalancesDict(RootModel[Dict[str, Amount]], SaveLoadMixin):
    pass


class EntryStore(BaseModel, SaveLoadMixin):
    entries: list[Entry] = []


@dataclass
class PathFinder:
    directory: str
    _chart: str = "chart.json"
    _store: str = "store.json"
    _balances: str = "balances.json"

    @property
    def base(self) -> Path:
        return Path(self.directory)

    @property
    def chart(self) -> Path:
        return self.base / self._chart

    @property
    def store(self) -> Path:
        return self.base / self._store

    @property
    def balances(self) -> Path:
        return self.base / self._balances

    def get_chart(self):
        return Chart.load(self.chart)

    def get_store(self):
        return EntryStore.load(self.store)

    def get_balances(self) -> dict[AccountName, Amount]:
        return BalancesDict.load(self.balances).root


class Book:
    def __init__(self, chart: Chart, opening_balances: dict | None = None):
        self.chart = chart
        if opening_balances is None:
            opening_balances = {}
        self.ledger = chart.open(opening_balances)
        self.store = EntryStore()

    def save_chart(self, directory: str):
        self.chart.save(PathFinder(directory).chart)

    def save_store(self, directory: str):
        self.store.save(PathFinder(directory).store)

    def save_balances(self, directory: str):
        BalancesDict(self.ledger.balances).save(PathFinder(directory).balances)

    @classmethod
    def load(cls, directory: str):
        """Load chart and starting balances from directory."""
        path = PathFinder(directory)
        try:
            chart = Chart.load(path.chart)
        except FileNotFoundError:
            raise AbacusError(f"Chart file not found: {path.chart}")
        opening_balances = path.get_balances() if path.balances.exists() else {}
        return cls(chart, opening_balances)

    def open(self, starting_balances=None):
        if not starting_balances:
            starting_balances = {}
        self.ledger = self.chart.open(starting_balances)

    def save(self, directory: str):
        self.save_chart(directory)
        self.save_store(directory)
        self.save_balances(directory)

    def post(self, entry: Entry):
        self.ledger.post(entry)
        self.store.entries.append(entry)

    def post_many(self, entries: Sequence[Entry]):
        for entry in entries:
            self.post(entry)

    def close(self):
        entries = self.ledger.close(self.chart)
        self.store.entries.extend(entries)

    @property
    def income_statement(self):
        return self.ledger.income_statement(self.chart)

    @property
    def balance_sheet(self):
        return self.ledger.balance_sheet(self.chart)
