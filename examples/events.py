from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from decimal import Decimal
from enum import Enum
from typing import ClassVar


class T5(Enum):
    Asset = "asset"
    Liability = "liability"
    Equity = "equity"
    Income = "income"
    Expense = "expense"

    def __repr__(self):
        return self.value.capitalize()


class Action:
    pass


@dataclass
class Add(Action):
    """Add account."""

    name: str
    t: T5


@dataclass
class Offset(Action):
    """Add contra account."""

    parent: str
    name: str


@dataclass
class Drop(Action):
    """Drop account if it has zero balance."""

    name: str


@dataclass
class Transfer(Action):
    """Transfer account balance to another account."""

    from_account: str
    to_account: str


@dataclass
class Close(Action):
    """Close ledger to earnings account."""

    earnings_account: str


class Entry:
    pass


@dataclass
class Debit(Entry):
    """Increase debit-normal accounts, decrease credit-normal accounts."""

    account: str
    amount: int | float | Decimal


@dataclass
class Credit(Entry):
    """Increase credit-normal accounts, decrease debit-normal accounts."""

    account: str
    amount: int | float | Decimal


@dataclass
class Double(Entry):
    """Double-entry transaction."""

    debit: str
    credit: str
    amount: int | float | Decimal

    def __iter__(self):
        yield Debit(self.debit, self.amount)
        yield Credit(self.credit, self.amount)


@dataclass
class Multiple(Entry):
    """Multiple entry, balanced by debit and credit changes."""

    entries: list[Debit | Credit]

    def __post_init__(self):
        def sum_class(cls):
            return sum(x.amount for x in self.entries if isinstance(x, cls))

        if sum_class(Debit) != sum_class(Credit):
            raise AbacusError(f"Debits and credits are not balanced for {self.events}")

    def __iter__(self):
        yield from self.entries


@dataclass
class Contra:
    """Contra account, refers to an existing regular account."""

    name: str


class AbacusError(Exception):
    pass


def must_not_exist(chart, name):
    if name in chart:
        raise AbacusError(f"Account {name} already exists.")


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
class Account(ABC):
    name: str
    title: str | None = None
    contra_accounts: list[str] = field(default_factory=list)
    t: ClassVar[T5]

    def __iter__(self):
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
class Ledger:
    accounts: dict[str, TAccount] = field(default_factory=dict)
    chart: dict[str, T5 | Contra] = field(default_factory=dict)

    @property
    def balances(self):
        return {name: str(account.balance) for name, account in self.accounts.items()}

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

    def apply_many(self, incoming):
        events = list(incoming)
        for event in events:
            self.apply(event)
        return events

    def apply_entry(self, entry: Entry) -> list[Entry]:
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
            case Multiple(events):
                return self.apply_many(events)
            case _:
                raise AbacusError(f"Unknown event {entry}")

    def apply_action(self, action: Action) -> list[Entry]:
        match action:
            case Add(name, t):
                must_not_exist(self.chart, name)
                self.chart[name] = t
                self.create_account(name)
                return []
            case Offset(parent, name):
                if parent not in self.chart:
                    raise AbacusError(f"Account {parent} must exist.")
                must_not_exist(self.chart, name)
                self.chart[name] = Contra(parent)
                self.create_account(name)
                return []
            case Drop(name):
                if not self.accounts[name].is_empty():
                    raise AbacusError(f"Account {name} is not empty.")
                del self.accounts[name]
                return []
            case Transfer(from_account, to_account):
                transfer_entry = self.transfer_entry(from_account, to_account)
                events = [transfer_entry, Drop(from_account)]
                self.apply_many(events)
                return [transfer_entry]
            case _:
                raise AbacusError(f"Unknown event {action}")

    def apply(self, incoming: Entry | Action | Account) -> list[Entry]:
        if isinstance(incoming, Account):
            self.apply_many(incoming)
            return []
        elif isinstance(incoming, Entry):
            return self.apply_entry(incoming)
        elif isinstance(incoming, Action):
            return self.apply_action(incoming)
        else:
            raise AbacusError(f"Unknown type {incoming}")


events: list[Account | Action | Entry] = [
    # Create accounts
    Asset("cash"),
    Equity("equity"),
    Income("services", contra_accounts=["refunds", "voids"]),
    Liability("tax", title="VAT due to tax authorities"),
    Expense("salaries"),
    Equity("retained_earnings"),
    # Start ledger
    # TODO: Open(cash=10, equity=8, retained_earnings=2)
    Multiple([Debit("cash", 10), Credit("equity", 8), Credit("retained_earnings", 2)]),
    # Start transactions
    # TODO: demonstrate mmultiple entry
    # Multiple(debits=[("cash", 120)], credits=[("services", 100), ("tax_due", 20)]),
    Double("cash", "services", 75),
    Double("refunds", "cash", 10),
    Double("voids", "cash", 15),
    Double("salaries", "cash", 25),
    # Close accounts
    # TODO: Close(retained_earnings)
    Transfer("refunds", "services"),
    Transfer("voids", "services"),
    Transfer("services", "retained_earnings"),
    Transfer("salaries", "retained_earnings"),
]

# Run ledger
ledger = Ledger()
for event in events:
    print(event)
    ledger.apply(event)
print(ledger.balances)
print(ledger.chart)
assert ledger.balances == {
    "cash": "35",
    "equity": "8",
    "retained_earnings": "27",
    "tax": "0",
}
assert ledger.chart == {
    "cash": T5.Asset,
    "equity": T5.Equity,
    "services": T5.Income,
    "refunds": Contra(name="services"),
    "voids": Contra(name="services"),
    "salaries": T5.Expense,
    "retained_earnings": T5.Equity,
    "tax": T5.Liability,
}
