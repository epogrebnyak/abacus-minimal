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
from pathlib import Path
from typing import ClassVar, Iterable, Iterator, Sequence

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
    """Indicate a complex operation on a ledger."""


@dataclass
class Add(Charting):
    """Add account."""

    name: str
    t: T5


@dataclass
class Offset(Charting):
    """Add contra account."""

    parent: str
    name: str


@dataclass
class Drop(Charting):
    """Drop account if the account and its contra accounts have zero balances."""

    name: str


@dataclass
class Account(ABC, Operation):
    name: str
    contra_accounts: list[str] = field(default_factory=list)
    title: str | None = None
    t: ClassVar[T5]

    def __iter__(self) -> Iterator[Add | Offset]:
        yield Add(self.name, self.t)
        for contra_name in self.contra_accounts:
            yield Offset(self.name, contra_name)


class Asset(Account):
    t = T5.Asset


class Equity(Account):
    t = T5.Equity


class Liability(Account):
    t = T5.Liability


class Income(Account):
    t = T5.Income


class Expense(Account):
    t = T5.Expense


@dataclass
class Transfer(Closing):
    """Transfer account balance to another account."""

    from_account: str
    to_account: str


@dataclass
class PeriodEnd(Closing):
    """Mark end of accounting period.

    At this point we shall save the state of ledger before closing it.
    The saved ledger will be used for the income statement.
    """


@dataclass
class Close(Closing):
    """Close ledger to earnings account."""

    earnings_account: str


@dataclass
class Debit(Posting):
    """Increase debit-normal accounts, decrease credit-normal accounts."""

    account: str
    amount: int | float | Decimal


@dataclass
class Credit(Posting):
    """Increase credit-normal accounts, decrease debit-normal accounts."""

    account: str
    amount: int | float | Decimal


@dataclass
class Double(Posting):
    """Double-entry transaction."""

    debit: str
    credit: str
    amount: int | float | Decimal

    def __iter__(self):
        yield Debit(self.debit, self.amount)
        yield Credit(self.credit, self.amount)


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

    def debit(self, account, amount):
        self.debits.append((account, amount))
        return self

    def credit(self, account, amount):
        self.credits.append((account, amount))
        return self

    @staticmethod
    def sums(xs):
        return sum(x for (_, x) in xs)

    def is_balanced(self):
        return self.sums(self.debits) == self.sums(self.credits)

    def to_multiple(self):
        return Multiple(debits=self.debits, credits=self.credits)


class Multiple(Unbalanced):
    """Multiple entry that is balanced by debits and credits."""

    def __post_init__(self):
        self.validate()

    def validate(self):
        if not self.is_balanced():
            self.raise_not_balanced()
        return self

    def raise_not_balanced(self):
        ds = self.sums(self.debits)
        cs = self.sums(self.credits)
        raise AbacusError(f"Debits {ds} and credits {cs} are not balanced for {self}.")


@dataclass
class Initial(Posting):
    """Open ledger with initial balances."""

    balances: list[tuple[str, Numeric]]


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


class DebitAccount(TAccount):
    def debit(self, amount: int | float | Decimal):
        self.balance += Decimal(amount)


class CreditAccount(TAccount):
    def debit(self, amount: int | float | Decimal):
        self.balance -= Decimal(amount)


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


Primitive = Add | Offset | Debit | Credit | PeriodEnd | Drop


@dataclass
class Event:
    action: Operation
    primitives: list[Primitive]
    note: str | None


@dataclass
class History:
    events: list[Event] = field(default_factory=list)

    def __iter__(self):
        yield from self.events

    def append(
        self, action: Operation, primitives: list[Primitive], note: str | None = None
    ):
        self.events.append(Event(action, primitives, note))

    @property
    def primitives(self) -> Iterable[Primitive]:
        for event in self.events:
            yield from event.primitives

    @property
    def actions(self) -> Iterable[Primitive]:
        for event in self.events:
            yield from event.primitives

    def save(self, path: str | Path):
        pass

    @classmethod
    def load(cls, path: str | Path):
        pass

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
    def from_accounts(cls, accounts: Iterable[Account]): # just shugar
        return cls.from_list(accounts)

    @classmethod
    def from_list(cls, actions: Iterable[Operation]):
        return cls().apply_many(actions)

    @property
    def balances(self):
        return {name: account.balance for name, account in self.accounts.items()}

    def is_debit_account(self, name) -> bool:
        match self.chart[name]:
            case Contra(name):
                return not self.is_debit_account(name)
            case t:
                return t in {T5.Asset, T5.Expense}

    def create_account(self, name):
        if self.is_debit_account(name):
            self.accounts[name] = DebitAccount()
        else:
            self.accounts[name] = CreditAccount()

    def transfer_entry(self, from_account, to_account) -> Double:
        balance = self.accounts[from_account].balance
        if self.is_debit_account(from_account):
            return Double(to_account, from_account, balance)
        else:
            return Double(from_account, to_account, balance)

    def initial_entry(self, balances: list[tuple[str, Numeric]]) -> Multiple:
        posting = Multiple()
        for account, amount in balances:
            if self.is_debit_account(account):
                posting.debit(account, amount)
            else:
                posting.credit(account, amount)
        if posting.is_balanced():
            return posting
        else:
            raise AbacusError(f"Unbalanced {balances}")

    def apply_from(self, action: Account | Multiple) -> list[Primitive]:
        """Get primitives from account or multiple entry and use them to change ledger."""
        primitives: list[Primitive] = []
        for operation in iter(action):
            primitives.extend(self._apply(operation))
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
                    # TODO: check contra accounts are empty too
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
            case Double(_, _, _):
                debit_entry, credit_entry = list(action)
                self._apply(debit_entry)
                self._apply(credit_entry)
                return [debit_entry, credit_entry]
            case Initial(balances):
                posting = self.initial_entry(balances)
                return self._apply(posting)
            case Multiple(_, _):
                return self.apply_from(action)
            case Transfer(from_account, to_account):
                transfer_entry = self.transfer_entry(from_account, to_account)
                operations.extend(self._apply(transfer_entry))
                return operations
            case PeriodEnd():
                self.accounts_before_close = deepcopy(self.accounts)
                return [action]
            case Close(earnings_account):
                for operation in self.close_ledger(earnings_account):
                    operations.extend(self._apply(operation))
                return operations
            case _:
                raise AbacusError(f"Unknown {action}")

    def close_ledger(self, earnings_account: str) -> Iterable[Operation]:
        yield PeriodEnd()
        for from_account, to_account in self.chart.close(earnings_account):
            yield Transfer(from_account, to_account)
            yield Drop(from_account)

    def apply(self, action: Operation, note: str | None = None):
        operations = self._apply(action)
        self.history.append(action, operations, note)
        return self
    
    def apply_many(self, actions: Sequence[Operation]):
        for action in actions:
            self.apply(action, note=None)
        return self


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
    Initial([("cash", 10), ("equity", 8), ("retained_earnings", 2)]),
    # Make transactions
    Multiple(debits=[("cash", 120)], credits=[("services", 100), ("vat", 20)]),
    Double("refunds", "cash", 15),
    Double("voids", "cash", 15),
    Double("salaries", "cash", 50),
    # Close period end
    Close("retained_earnings"),
]

ledger = Ledger.from_accounts(accounts).apply_many(entries)
for event in ledger.history:    
    print(event.action, "translates to:\n ", event.primitives)
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
assert ledger2.is_closed() is True  # should be true
