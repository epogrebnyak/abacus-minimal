"""An accounting ledger that equates to a sequence of events.

The main class is `Ledger`. You can modify the state of ledger by applying operations to it.

The basic ('primitive') operations are:
- `Add` and `Offset` to add regular and contra accounts,
- `Debit` and `Credit` to change account balances,
- `PeriodEnd` to mark accounting period end,
- `Drop` to deactivate an empty account.

The compound operations are:
- `Account` to specify an account together with contra accounts,
- `Initial` to open ledger with initial balances,
- `Double` to make a double entry,
- `Multiple` to make a multiple entry,
- `Transfer` to move account balance to another account,
- `Close` for closing accounts at period end.

Each compound operation consists of a sequence of basic operations.

`Ledger.history` holds a sequence of events that where applied to the ledger.
You can re-run the events on empty ledger and will arrive to the same state of ledger.
"""

from abc import ABC, abstractmethod
from collections import UserDict
from copy import deepcopy
from dataclasses import dataclass, field
from decimal import Decimal
from typing import Iterable, Literal

import simplejson as json  # type: ignore
from pydantic import BaseModel

from .base import T5, AbacusError, Closing, Numeric, Operation, SaveLoadMixin
from .chart import Account, Add, Asset, Drop, Equity, Expense, Income, Liability, Offset
from .entry import Credit, Debit, Double, Multiple, Posting, Unbalanced


@dataclass
class Initial(Posting):
    """Open ledger with initial balances."""

    balances: dict[str, Numeric]
    tag: Literal["initial"] = "initial"

    def to_entry(self, chart: "ChartDict") -> Multiple:
        """Convert event to valid multiple entry using chart."""
        entry = Unbalanced()
        for account, amount in self.balances.items():
            AbacusError.must_exist(chart, account)
            pair = account, Decimal(amount)
            if chart.is_debit_account(account):
                entry.debits.append(pair)
            else:
                entry.credits.append(pair)
        return entry.to_multiple()


@dataclass
class Transfer(Closing):
    """Transfer account balance to another account."""

    from_account: str
    to_account: str
    tag: Literal["transfer"] = "transfer"


@dataclass
class PeriodEnd(Closing):
    """Mark end of accounting period and save the copy of ledger
    to be used for the income statement.
    """

    tag: Literal["period_end"] = "period_end"


@dataclass
class Close(Closing):
    """Close ledger to earnings account."""

    earnings_account: str
    tag: Literal["close"] = "close"


@dataclass
class TAccount(ABC):
    balance: Decimal = Decimal(0)

    @abstractmethod
    def debit(self, amount: int | float | Decimal):
        pass

    def credit(self, amount: int | float | Decimal):
        self.debit(-amount)

    def is_empty(self) -> bool:
        return self.balance == Decimal(0)

    @abstractmethod
    def transfer(self, from_account: str, to_account: str) -> Double:
        pass


class DebitAccount(TAccount):
    def debit(self, amount: int | float | Decimal):
        self.balance += Decimal(amount)

    def transfer(self, from_account: str, to_account: str) -> Double:
        return Double(to_account, from_account, self.balance)


class CreditAccount(TAccount):
    def debit(self, amount: int | float | Decimal):
        self.balance -= Decimal(amount)

    def transfer(self, from_account: str, to_account: str) -> Double:
        return Double(from_account, to_account, self.balance)

@dataclass
class Contra:
    """Contra account, refers to an existing regular account."""

    name: str

class ChartDict(UserDict[str, T5 | Contra]):
    """A representation of chart of accounts that ensures account names are unique.""" 

    def is_debit_account(self, name) -> bool:
        """Return True if *name* is debit-normal account."""
        AbacusError.must_exist(self, name)
        match self[name]:
            case Contra(name):
                return not self.is_debit_account(name)
            case t:
                return t in {T5.Asset, T5.Expense}

    def by_type(self, t: T5) -> list[str]:
        """List regular accounts of a given type."""
        return [name for name, account_type in self.data.items() if account_type == t]

    def find_contra_accounts(self, regular_account_name: str) -> list[str]:
        """List contra accounts for an given regular account."""
        return [
            contra_name
            for contra_name, parent in self.data.items()
            if parent == Contra(regular_account_name)
        ]

def close_contra_accounts(chart: ChartDict, t: T5) -> Iterable[tuple[str, str]]:
    """Yield pairs of account names for closing contra accounts."""
    for account_name in chart.by_type(t):
        for contra_name in chart.find_contra_accounts(account_name):
            yield (contra_name, account_name)


def closing_pairs(chart: ChartDict, earnings_account: str) -> Iterable[tuple[str, str]]:
    """Yield pairs of account names that will need to transfer when closing."""
    for t in (T5.Income, T5.Expense):
        yield from close_contra_accounts(chart, t)
        for account_name in chart.by_type(t):
            yield (account_name, earnings_account)


Primitive = Add | Offset | Debit | Credit | PeriodEnd | Drop
AccountType = Asset | Equity | Liability | Income | Expense
EntryType = Double | Multiple | Initial
ClosingType = Transfer | Close
# TODO: may disqualify primitives from actions or allow a list of primitives in actions
Action = Primitive | AccountType | EntryType | ClosingType
# Action2 = list[Primitive] | AccountType | EntryType | ClosingType

@dataclass
class Event:
    action: Action
    primitives: list[Primitive]
    note: str | None


class History(BaseModel, SaveLoadMixin):
    events: list[Event] = field(default_factory=list)

    def append(
        self, action: Action, primitives: list[Primitive], note: str | None = None
    ):
        event = Event(action, primitives, note)
        self.events.append(event)

    @property
    def primitives(self) -> Iterable[Primitive]:
        for event in self.events:
            yield from event.primitives

    @property
    def actions(self) -> Iterable:
        for event in self.events:
            yield event.action

    @property
    def accounts(self) -> Iterable[Account]:
        for action in self.actions:
            if isinstance(action, Account):
                yield action

    def to_ledger(self) -> "Ledger":
        """Re-create ledger from history of actions."""
        return Ledger.from_list(self.actions)


@dataclass
class Ledger:
    accounts: dict[str, TAccount] = field(default_factory=dict)
    chart: ChartDict = field(default_factory=ChartDict)
    history: History = field(default_factory=History)
    accounts_before_close: dict[str, TAccount] | None = None

    def is_closed(self) -> bool:
        return self.accounts_before_close is not None

    @classmethod
    def from_accounts(cls, accounts: Iterable[Account]):
        return cls.from_list(accounts)  # type: ignore

    @classmethod
    def from_list(cls, actions: Iterable[Action]):
        return cls().apply_many(actions)

    @property
    def balances(self):
        return ReportDict(
            {name: account.balance for name, account in self.accounts.items()}
        )

    def _create_account(self, name):
        if self.chart.is_debit_account(name):
            self.accounts[name] = DebitAccount()
        else:
            self.accounts[name] = CreditAccount()

    def run_iterable(self, iterable) -> list[Primitive]:
        """Get primitives from an iterable action and use them to change ledger."""
        primitives: list[Primitive] = []
        for operation in iter(iterable):
            primitives += self.run(operation)
        return primitives

    def run(self, action: Operation) -> list[Primitive]:
        """Apply action and return list of primitives."""
        if isinstance(action, (Account, Double, Multiple)):
            return self.run_iterable(action)
        match action:
            case Add(name, t):
                AbacusError.must_not_exist(self.chart, name)
                self.chart[name] = t
                self._create_account(name)
                return [action]
            case Offset(parent, name):
                if parent not in self.chart:
                    raise AbacusError(f"Account {parent} must exist.")
                AbacusError.must_not_exist(self.chart, name)
                self.chart[name] = Contra(parent)
                self._create_account(name)
                return [action]
            case Drop(name):
                if not self.accounts[name].is_empty():
                    raise AbacusError(f"Account {name} is not empty.")                    
                    # FIMXE: must also check contra accounts are empty
                del self.accounts[name]
                return [action]
            case Debit(account, amount):
                AbacusError.must_exist(self.accounts, account)
                self.accounts[account].debit(amount)
                return [action]
            case Credit(account, amount):
                AbacusError.must_exist(self.accounts, account)
                self.accounts[account].credit(amount)
                return [action]
            case Initial(_):
                initial_entry = action.to_entry(self.chart)
                return self.run(initial_entry)
            case Transfer(from_account, to_account):
                transfer_entry = self.transfer_entry(from_account, to_account)
                return self.run(transfer_entry)
            case PeriodEnd():
                self.accounts_before_close = deepcopy(self.accounts)
                return [action]
            case Close(earnings_account):
                actions = self.close_ledger_items(earnings_account)
                return self.run_iterable(actions)
            case _:
                raise AbacusError(f"Unknown {action}")

    def transfer_entry(self, from_account, to_account):           
        return self.accounts[from_account].transfer(from_account, to_account)

    def close_ledger_items(self, earnings_account: str) -> Iterable[Operation]:
        yield PeriodEnd()
        for from_account, to_account in closing_pairs(self.chart, earnings_account):
            yield Transfer(from_account, to_account)
            yield Drop(from_account)

    def close(self, earnings_account: str):
        self.apply(Close(earnings_account))
        return self

    def apply(self, action: Action, note: str | None = None):
        operations = self.run(action)
        self.history.append(action, operations, note)
        return self

    def apply_many(self, actions: Iterable[Action], note: str | None = None):
        for action in actions:
            self.apply(action, note)
        return self

    def income_statement(self) -> "IncomeStatement":
        if self.is_closed():
            return IncomeStatement.new(self.accounts_before_close, self.chart)
        else:
            return IncomeStatement.new(self.accounts, self.chart)

    def proxy(self, proxy_earnings_account: str) -> "Ledger":
        """Create a shallow ledger copy and close to proxy accumulation account."""
        return (
            Ledger(accounts=deepcopy(self.accounts), chart=deepcopy(self.chart))
            .apply(Equity(proxy_earnings_account))
            .close(proxy_earnings_account)
        )

    def balance_sheet(self, proxy_earnings: str = "current_earnings") -> "BalanceSheet":
        ledger = self if self.is_closed() else self.proxy(proxy_earnings)
        return BalanceSheet.new(ledger.accounts, ledger.chart)


@dataclass
class Reporter:
    accounts: dict[str, TAccount]
    chart: ChartDict

    def net_balance(self, name: str) -> Decimal:
        """Return account balance minus associated contra account balances."""
        contra_account_balances = [
            self.accounts[contra_name].balance
            for contra_name in self.chart.find_contra_accounts(name)
        ]
        return Decimal(self.accounts[name].balance - sum(contra_account_balances))

    def fill(self, t: T5) -> "ReportDict":
        """Return net balances for a given account type."""
        result = ReportDict()
        for name in self.chart.by_type(t):
            result[name] = self.net_balance(name)
        return result


class ReportDict(UserDict[str, Decimal], SaveLoadMixin):
    @property
    def total(self):
        return Decimal(sum(self.data.values()))

    def model_dump_json(self, indent: int = 2, warnings: bool = False):
        return json.dumps(self.data, indent=indent)

    @classmethod
    def model_validate_json(cls, text: str):
        return cls(json.loads(text))


class Report:
    """Base class for financial reports."""


@dataclass
class IncomeStatement(Report):
    income: ReportDict
    expenses: ReportDict

    @classmethod
    def new(cls, balances, chart):
        reporter = Reporter(balances, chart)
        return cls(income=reporter.fill(T5.Income), expenses=reporter.fill(T5.Expense))

    @property
    def net_earnings(self):
        """Calculate net earnings as income less expenses."""
        return self.income.total - self.expenses.total


@dataclass
class BalanceSheet(Report):
    assets: ReportDict
    equity: ReportDict
    liabilities: ReportDict

    @classmethod
    def new(cls, balances, chart):
        reporter = Reporter(balances, chart)
        return cls(
            assets=reporter.fill(T5.Asset),
            equity=reporter.fill(T5.Equity),
            liabilities=reporter.fill(T5.Liability),
        )

    def is_balanced(self) -> bool:
        """Return True if assets equal liabilities plus equity."""
        return self.assets.total == (self.equity.total + self.liabilities.total)
