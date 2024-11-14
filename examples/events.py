"""An accounting ledger that equates to a sequence of events.

The main class is `Ledger`. You can modify the state of ledger by applying operations to it.

The basic operations are:
- `Add` and `Offset` to add regular and contra accounts,
- `Debit` and `Credit` to change account balances,
- `PeriodEnd` to mark accounting period end,
- `Drop` to deactivate an empty account.

The compound operations are:
- `Account` to specify an account together with contra accounts,
- `Initial` to open ledger with initial balances,
- `Double` to make a double entry transaction,
- `Multiple` to make a multiple entry transaction,
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
from enum import Enum
from typing import Iterable, Iterator, Literal, Sequence

from mixin import SaveLoadMixin
from pydantic import BaseModel

Numeric = int | float | Decimal


class AbacusError(Exception):
    pass


class T5(Enum):
    Asset = "asset"
    Liability = "liability"
    Equity = "equity"
    Income = "income"
    Expense = "expense"

    def __repr__(self):
        return self.value.capitalize()


class Operation(object):
    """A unit of change of the ledger state.

    Types of operations:
    - Account, Charting: change chart of accounts,
    - Posting: change account balances,
    - Closing: compound period end operation on a ledger.
    """


class Charting(Operation):
    """Change chart of accounts."""


class Posting(Operation):
    """Change account balances."""


class Closing(Operation):
    """Close accounts at period end."""


@dataclass
class Add(Charting):
    """Add account."""

    name: str
    t: T5
    tag: Literal["add"] = "add"


@dataclass
class Offset(Charting):
    """Add contra account."""

    parent: str
    name: str
    tag: Literal["offset"] = "offset"


@dataclass
class Drop(Charting):
    """Drop account if the account and its contra accounts have zero balances."""

    name: str
    tag: Literal["drop"] = "drop"


@dataclass
class Account(ABC, Operation):
    name: str
    contra_accounts: list[str] = field(default_factory=list)
    title: str | None = None
    
    @property
    @abstractmethod
    def tag(self):
        pass

    @property
    def t(self) -> T5:
        return T5(self.tag)

    def __iter__(self) -> Iterator[Add | Offset]:
        yield Add(self.name, self.t)
        for contra_name in self.contra_accounts:
            yield Offset(self.name, contra_name)


@dataclass
class Asset(Account):
    t = T5.Asset
    tag: Literal["asset"] = "asset"


@dataclass
class Equity(Account):
    t = T5.Equity
    tag: Literal["equity"] = "equity"


@dataclass
class Liability(Account):
    t = T5.Liability
    tag: Literal["liability"] = "liability"


@dataclass
class Income(Account):
    t = T5.Income
    tag: Literal["income"] = "income"


@dataclass
class Expense(Account):
    t = T5.Expense
    tag: Literal["expense"] = "expense"


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
class Debit(Posting):
    """Increase debit-normal accounts, decrease credit-normal accounts."""

    account: str
    amount: int | float | Decimal
    tag: Literal["debit"] = "debit"


@dataclass
class Credit(Posting):
    """Increase credit-normal accounts, decrease debit-normal accounts."""

    account: str
    amount: int | float | Decimal
    tag: Literal["credit"] = "credit"


@dataclass
class Double(Posting):
    """Double-entry transaction."""

    debit: str
    credit: str
    amount: int | float | Decimal


@dataclass
class Unbalanced(Posting):
    """Multiple entry that may not be balanced by debits and credits."""

    debits: list[tuple[str, Numeric]] = field(default_factory=list)
    credits: list[tuple[str, Numeric]] = field(default_factory=list)

    def __iter__(self):
        for account, amount in self.debits:
            yield Debit(account, amount)
        for account, amount in self.credits:
            yield Credit(account, amount)

    @staticmethod
    def sums(xs):
        return sum(x for (_, x) in xs)

    def is_balanced(self):
        return self.sums(self.debits) == self.sums(self.credits)

    def to_multiple(self):
        return Multiple(debits=self.debits, credits=self.credits)


@dataclass
class Multiple(Unbalanced):
    """Multiple entry that is balanced by debits and credits."""

    tag: Literal["multiple"] = "multiple"

    def __post_init__(self):
        self.validate()

    def validate(self):
        ds = self.sums(self.debits)
        cs = self.sums(self.credits)
        if ds != cs:
            raise AbacusError(
                f"Debits {ds} and credits {cs} are not balanced for {self}."
            )
        return self


@dataclass
class Initial(Posting):
    """Open ledger with initial balances."""

    balances: dict[str, Numeric]

    def to_multiple(self, chart: "ChartDict") -> Multiple:
        entry = Unbalanced()
        for account, amount in self.balances.items():
            if chart.is_debit_account(account):
                entry.debits.append((account, Decimal(amount)))
            else:
                entry.credits.append((account, Decimal(amount)))
        return entry.to_multiple()


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


def must_not_exist(collection, name):
    if name in collection:
        raise AbacusError(f"Account {name} already exists.")


def must_exist(collection, name):
    if name not in collection:
        raise AbacusError(f"Account {name} not found.")


class ChartDict(UserDict[str, T5 | Contra]):
    def is_debit_account(self, name) -> bool:
        if name not in self:
            raise AbacusError(f"Account {name} not found.")
        match self[name]:
            case Contra(name):
                return not self.is_debit_account(name)
            case t:
                return t in {T5.Asset, T5.Expense}

    def by_type(self, t: T5) -> list[str]:
        return [name for name, account_type in self.data.items() if account_type == t]

    def find_contra_accounts(self, name: str) -> list[str]:
        return [
            contra_name
            for contra_name, parent in self.data.items()
            if parent == Contra(name)
        ]

    def close_contra_accounts(self, t: T5) -> Iterable[tuple[str, str]]:
        for account in self.by_type(t):
            for contra in self.find_contra_accounts(account):
                yield (contra, account)

    def close_type(self, t: T5, earnings_account: str) -> Iterable[tuple[str, str]]:
        for account in self.by_type(t):
            yield (account, earnings_account)

    def close(self, earnings_account: str) -> Iterable[tuple[str, str]]:
        for t in (T5.Income, T5.Expense):
            yield from self.close_contra_accounts(t)
            yield from self.close_type(t, earnings_account)

# may disqualify primitives from actions or allow a list of primitives 
Primitive = Add | Offset | Debit | Credit | PeriodEnd | Drop
AccountType =  Asset | Equity | Liability | Income | Expense
EntryType = Double | Multiple | Initial
ClosingType = Transfer | Close
Action = Primitive | AccountType | EntryType | ClosingType

@dataclass
class Event:
    action: Action
    primitives: list[Primitive]
    note: str | None


class History(BaseModel, SaveLoadMixin):
    events: list[Event] = field(default_factory=list)

    def __iter__(self):
        yield from self.events

    def append(
        self, action: "Action", primitives: list[Primitive], note: str | None = None
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

    def to_ledger(self):
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
        return cls.from_list(accounts) # type: ignore

    @classmethod
    def from_list(cls, actions: Iterable[Action]):
        return cls().apply_many(actions)

    @property
    def balances(self):
        return {name: account.balance for name, account in self.accounts.items()}

    def create_account(self, name):
        if self.chart.is_debit_account(name):
            self.accounts[name] = DebitAccount()
        else:
            self.accounts[name] = CreditAccount()

    def apply_from(self, action: Account | Multiple) -> list[Primitive]:
        """Get primitives from account or multiple entry and use them to change ledger."""
        primitives: list[Primitive] = []
        for operation in iter(action):
            primitives += self._apply(operation)
        return primitives

    def _apply(self, action: Operation) -> list[Primitive]:
        if isinstance(action, Account):
            return self.apply_from(action)
        operations: list[Primitive] = []
        match action:
            case Add(name, t):
                must_not_exist(self.chart, name)
                self.chart[name] = t
                self.create_account(name)
                return [action]
            case Offset(parent, name):
                if parent not in self.chart:
                    raise AbacusError(f"Account {parent} must exist.")
                must_not_exist(self.chart, name)
                self.chart[name] = Contra(parent)
                self.create_account(name)
                return [action]
            case Drop(name):
                if not self.accounts[name].is_empty():
                    raise AbacusError(f"Account {name} is not empty.")
                    # also check contra accounts are empty
                del self.accounts[name]
                return [action]
            case Debit(account, amount):
                must_exist(self.accounts, account)
                self.accounts[account].debit(amount)
                return [action]
            case Credit(account, amount):
                must_exist(self.accounts, account)
                self.accounts[account].credit(amount)
                return [action]
            case Double(debit, credit, amount):
                entry = Multiple(debits=[(debit, amount)], credits=[(credit, amount)])
                return self._apply(entry)
            case Initial(_):
                return self._apply(action.to_multiple(self.chart))
            case Multiple(_, _):
                return self.apply_from(action)
            case Transfer(from_account, to_account):
                account_ = self.accounts[from_account]
                transfer_entry = account_.transfer(from_account, to_account)
                operations += self._apply(transfer_entry)
                return operations
            case PeriodEnd():
                self.accounts_before_close = deepcopy(self.accounts)
                return [action]
            case Close(earnings_account):
                for operation in self.close_ledger(earnings_account):
                    operations += self._apply(operation)
                return operations
            case _:
                raise AbacusError(f"Unknown {action}")

    def close_ledger(self, earnings_account: str) -> Iterable[Operation]:
        yield PeriodEnd()
        for from_account, to_account in self.chart.close(earnings_account):
            yield Transfer(from_account, to_account)
            yield Drop(from_account)

    def close(self, earnings_account: str):
        self.apply(Close(earnings_account))
        return self

    def apply(self, action: "Action", note: str | None = None):
        operations = self._apply(action)
        self.history.append(action, operations, note)
        return self

    def apply_many(self, actions: Iterable[Action]):
        for action in actions:
            self.apply(action, note=None)
        return self

    def income_statement(self) -> "IncomeStatement":
        if self.is_closed():
            return IncomeStatement.new(self.accounts_before_close, self.chart)
        else:
            return IncomeStatement.new(self.accounts, self.chart)

    def proxy(self, proxy_earnings_account: str) -> "Ledger":
        """Create a ledger copy to used before period close."""
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


class ReportDict(UserDict[str, Decimal]):
    @property
    def total(self):
        return Decimal(sum(self.data.values()))


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
        """Return True if assets equal liabilities plus capital."""
        return self.assets.total == (self.equity.total + self.liabilities.total)


# Create accounts
accounts = [
    Asset("cash"),
    Equity("equity"),
    Income("services", contra_accounts=["refunds", "voids"]),
    Liability("vat", title="VAT due to tax authorities"),
    Expense("salaries"),
    Equity("retained_earnings"),
]

entries = [
    # Start ledger with initial balances
    Initial({"cash": 10, "equity": 8, "retained_earnings": 2}),
    # Make transactions
    Multiple(debits=[("cash", 120)], credits=[("services", 100), ("vat", 20)]),
    Double("refunds", "cash", 15),
    Double("voids", "cash", 15),
    Double("salaries", "cash", 50),
    # Close period end
    Close("retained_earnings"),
]

ledger = Ledger.from_accounts(accounts).apply_many(entries)
print(ledger.chart)
print(ledger.balances)
assert len(ledger.history.events) == len(accounts) + len(entries)
assert ledger.balances == {
    "cash": 50,
    "equity": 8,
    "vat": 20,
    "retained_earnings": 22,
}
assert ledger.chart == {
    "cash": T5.Asset,
    "equity": T5.Equity,
    "services": T5.Income,
    "refunds": Contra(name="services"),
    "voids": Contra(name="services"),
    "salaries": T5.Expense,
    "retained_earnings": T5.Equity,
    "vat": T5.Liability,
}
ledger2 = ledger.history.to_ledger()
assert ledger2.balances == ledger.balances
if ledger.accounts_before_close is not None:
    assert len(ledger.accounts_before_close) > 3
assert ledger2.is_closed() is True
assert ledger2.income_statement().net_earnings == 20
assert ledger2.balance_sheet().is_balanced() is True
content = ledger.history.model_dump_json(indent=2)
history2 = History.model_validate_json(content)
for a, b in zip(history2, ledger.history):
    assert a == b
history2.save("history2.json", allow_overwrite=True)
history3 = History.load("history2.json")
for a, b in zip(history2, history3):
    assert a == b


e = Event(
    action=Add(name="cash", t=T5.Asset, tag="add"),
    primitives=[Add(name="cash", t=T5.Asset, tag="add")],
    note=None,
)
h = History(events=[e])
d = h.model_dump()
h2 = History.model_validate(d)
print(h2)
