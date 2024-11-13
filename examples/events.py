"""An accounting ledger that equates to a sequence of events.

The main class is `Ledger`. You can modify the state of ledger by applying operations to it.

The basic operations are:
- `Add` and `Offset` to add regular and contra accounts,
- `Debit` and `Credit` to change account balances,
- `Drop` to deactivate empty account.

The compound operations are:
- `Account` to specify an account together with contra accounts,
- `Initial` to open ledger with initial balances,
- `Double` to make a double entry transaction,
- `Multiple` to make a multiple entry transaction,
- `Transfer` to transfer account balance to another account,
- `Close` to close ledger to earnings account at period end.

Each compound operation consists of a sequence of basic operations.

`Ledger.history` attribute holds a sequence of events that where applied to the ledger.
You can re-run the events on empty ledger and will get the same state of ledger.
"""

from abc import ABC, abstractmethod
from collections import UserDict
from copy import deepcopy
from dataclasses import dataclass, field
from decimal import Decimal
from enum import Enum
from typing import ClassVar, Iterable, Iterator, Sequence


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
    title: str | None = None
    contra_accounts: list[str] = field(default_factory=list)
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
    """Transfer account balance to another account and delete account."""

    from_account: str
    to_account: str


@dataclass
class Close(Closing):
    """Close ledger to earnings account. Saves state of ledger before close."""

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
    """Multiple entry this is not balanced by debit and credit."""

    debits: list[tuple[str, int | float | Decimal]] = field(default_factory=list)
    credits: list[tuple[str, int | float | Decimal]] = field(default_factory=list)

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
    """Multiple entry that is balanced by debit and credit."""

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


Numeric = int | float | Decimal


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


def must_not_exist(chart, name):
    if name in chart:
        raise AbacusError(f"Account {name} already exists.")

class ChartDict(UserDict[str, T5 | Contra]):
    def by_type(self, t: T5) -> list[str]:
        return [name for name, account_type in self.data.items() if account_type == t]

    def find_contra_accounts(self, name: str) -> list[str]:
        return [
            contra_name
            for contra_name, parent in self.data.items()
            if parent == Contra(name)
        ]

    def close_contra_accounts(self, t: T5) -> Iterable[Transfer]:
        for account in self.by_type(t):
            for contra in self.find_contra_accounts(account):
                yield Transfer(contra, account)

    def close_type(self, t: T5, earnings_account: str) -> Iterable[Transfer]:
        for account in self.by_type(t):
            yield Transfer(account, earnings_account)

    def close(self, earnings_account: str) -> Iterable[Transfer]:
        for t in (T5.Income, T5.Expense):
            yield from self.close_contra_accounts(t)
            yield from self.close_type(t, earnings_account)


Primitive = Add | Offset | Drop | Debit | Credit

@dataclass
class Event:
    primitives: list[Primitive]
    note: str | None = None


@dataclass
class Ledger:
    accounts: dict[str, TAccount] = field(default_factory=dict)
    chart: ChartDict = field(default_factory=ChartDict)
    history: list[Event] = field(default_factory=list)
    accounts_before_close: dict[str, TAccount] | None = None

    def is_closed(self) -> bool:
        return self.accounts_before_close is not None
    
    @classmethod
    def from_accounts(cls, accounts: Sequence[Account]):
        return cls.from_list(accounts)

    @classmethod
    def from_list(cls, actions: Sequence[Operation]):
        ledger = cls()
        for action in actions:
            ledger.apply(action)
        return ledger

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
        
    def apply_account(self, account: Account) -> list[Primitive]:
        operations = []
        for action in iter(account):
            operations.extend(self.apply_charting(action))
        return operations    

    def apply_charting(self, action: Charting) -> list[Primitive]:
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
                del self.accounts[name]
                return [action]
            case _:
                raise AbacusError(f"Unknown {action}")
    
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

    def apply_entry(self, entry: Posting) -> list[Primitive]:
        match entry:
            case Debit(account, amount):
                self.accounts[account].debit(amount)
                return [entry]
            case Credit(account, amount):
                self.accounts[account].credit(amount)
                return [entry]
            case Double(debit, credit, amount):
                self.accounts[debit].debit(amount)
                self.accounts[credit].credit(amount)
                return list(entry)
            case Initial(balances):
                posting = self.initial_entry(balances)
                return self.apply_entry(posting)
            case Multiple(_, _):
                for primitive in entry:
                    self.apply_entry(primitive)
                return list(entry)
            case _:
                raise AbacusError(f"Unknown {entry}")

    def apply_closing(self, action: Closing) -> list[Primitive]:
        operations: list[Primitive] = []
        match action:
            case Transfer(from_account, to_account):
                # transfer balance
                transfer_entry = self.transfer_entry(from_account, to_account)
                self.apply_entry(transfer_entry)
                operations.extend(transfer_entry)
                # drop account
                drop = Drop(from_account)
                self.apply_charting(drop)
                operations.append(drop)
                return operations
            case Close(earnings_account):
                operations = []
                self.accounts_before_close = deepcopy(self.accounts)
                for transfer in self.chart.close(earnings_account):
                    operations.extend(self.apply_closing(transfer))
                return operations
            case _:
                raise AbacusError(f"Unknown {action}")

    def apply(self, action: Operation, note: str | None=None) -> list[Primitive]:
        if isinstance(action, Account):
            operations = self.apply_account(action)
        elif isinstance(action, Charting):
            operations = self.apply_charting(action)
        elif isinstance(action, Posting):
            operations = self.apply_entry(action)
        elif isinstance(action, Closing):
            operations = self.apply_closing(action)
        else:
            raise AbacusError(f"Unknown {action}")
        self.history.append(Event(operations, note))
        return operations


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
    # Close accounts
    Close("retained_earnings"),
]

ledger = Ledger.from_accounts(accounts)
for e in entries:
    ledger.apply(e)
    print(e, "translates to", ledger.events[-1].primitives)
print(ledger.chart)
print(ledger.balances)
assert len(ledger.events) == len(accounts) + len(entries)
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
ledger2 = Ledger.from_list([p for event in ledger.events for p in event.primitives])
assert ledger2.balances == ledger.balances
if ledger.accounts_before_close is not None:
   assert len(ledger.accounts_before_close) > 3
