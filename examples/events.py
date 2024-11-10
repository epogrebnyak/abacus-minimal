from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from decimal import Decimal
from enum import Enum
from typing import ClassVar, Iterable


class T5(Enum):
    Asset = "asset"
    Liability = "liability"
    Equity = "equity"
    Income = "income"
    Expense = "expense"

    def __repr__(self):
        return self.value.capitalize()


class Primitive:
    """Indicate a basic operation on a ledger."""

    pass


class Charting:
    """Change chart of accounts."""

    pass


class Entry:
    """Change account balances."""

    pass


class Compound:
    """Indicate a complex operation on a ledger."""

    pass


@dataclass
class Add(Primitive, Charting):
    """Add account."""

    name: str
    t: T5


@dataclass
class Offset(Primitive, Charting):
    """Add contra account."""

    parent: str
    name: str


@dataclass
class Drop(Primitive, Charting):
    """Drop account if it has zero balance."""

    name: str


@dataclass
class Transfer(Compound):
    """Transfer account balance to another account."""

    from_account: str
    to_account: str


@dataclass
class Close(Compound):
    """Close ledger to earnings account."""

    earnings_account: str


@dataclass
class Debit(Entry, Primitive):
    """Increase debit-normal accounts, decrease credit-normal accounts."""

    account: str
    amount: int | float | Decimal


@dataclass
class Credit(Entry, Primitive):
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
    """Multiple entry, balanced by debit and credit."""

    debits: list[tuple[str, int | float | Decimal]] = field(default_factory=list)
    credits: list[tuple[str, int | float | Decimal]] = field(default_factory=list)

    def __post_init__(self):
        self

    def assert_balanced(self):
        ds = sum(x for (_, x) in self.debits)
        cs = sum(x for (_, x) in self.credits)
        if ds != cs:
            raise AbacusError(
                f"Debits {ds} and credits {cs} are not balanced for {self}."
            )

    def __iter__(self):
        for account, amount in self.debits:
            yield Debit(account, Decimal(amount))
        for account, amount in self.credits:
            yield Credit(account, Decimal(amount))


@dataclass
class Opening(Compound):
    """Open ledger with initial balances."""

    balances: dict[str, int | float | Decimal]


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
class Account(ABC, Charting):
    name: str
    title: str | None = None
    contra_accounts: list[str] = field(default_factory=list)
    t: ClassVar[T5]

    def __iter__(self) -> Iterable[Add | Offset]:
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

    def apply_charting(self, incoming: Charting) -> list[Primitive]:
        if isinstance(incoming, Account):
            return self.apply_many(incoming)
        match incoming:
            case Add(name, t):
                must_not_exist(self.chart, name)
                self.chart[name] = t
                self.create_account(name)
                return [incoming]
            case Offset(parent, name):
                if parent not in self.chart:
                    raise AbacusError(f"Account {parent} must exist.")
                must_not_exist(self.chart, name)
                self.chart[name] = Contra(parent)
                self.create_account(name)
                return [incoming]
            case Drop(name):
                if not self.accounts[name].is_empty():
                    raise AbacusError(f"Account {name} is not empty.")
                del self.accounts[name]
                return [incoming]
            case _:
                raise AbacusError(f"Unknown event {incoming}")

    def apply_entry(self, entry: Entry) -> list[Primitive]:
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
            case Multiple(_, _):
                for e in entry:
                    self.apply_entry(e)
                return list(entry)
            case _:
                raise AbacusError(f"Unknown event {entry}")

    def apply_compound(self, compound: Compound) -> list[Primitive]:
        match compound:
            case Opening(balances):
                raise NotImplementedError(balances)
            case Transfer(from_account, to_account):
                transfer_entry = self.transfer_entry(from_account, to_account)
                events = list(transfer_entry) + [Drop(from_account)]
                self.apply_many(events)
                return events
            case Close(earnings_account):
                raise NotImplementedError(earnings_account)
            case _:
                raise AbacusError(f"Unknown event {compound}")

    def apply_many(self, incoming) -> list[Primitive]:
        events = list(iter(incoming))
        for event in events:
            self.apply(event)
        return events

    def apply(self, incoming: Charting | Entry | Compound) -> list[Primitive]:
        if isinstance(incoming, Charting):
            return self.apply_charting(incoming)
        elif isinstance(incoming, Entry):
            return self.apply_entry(incoming)
        elif isinstance(incoming, Compound):
            return self.apply_compound(incoming)
        else:
            raise AbacusError(f"Unknown type {incoming}")


events: list[Account | Compound | Entry] = [
    # Create accounts
    Asset("cash"),
    Equity("equity"),
    Income("services", contra_accounts=["refunds", "voids"]),
    Liability("vat", title="VAT due to tax authorities"),
    Expense("salaries"),
    Equity("retained_earnings"),
    # Start ledger
    # TODO: Open(cash=10, equity=8, retained_earnings=2)
    Multiple(debits=[("cash", 10)], credits=[("equity", 8), ("retained_earnings", 2)]),
    # Start transactions
    # TODO: demonstrate multiple entry
    Multiple(debits=[("cash", 120)], credits=[("services", 100), ("vat", 20)]),
    Double("refunds", "cash", 20),
    Double("voids", "cash", 10),
    Double("salaries", "cash", 50),
    # Close accounts
    # TODO: Close(retained_earnings)
    Transfer("refunds", "services"),
    Transfer("voids", "services"),
    Transfer("services", "retained_earnings"),
    Transfer("salaries", "retained_earnings"),
]


m = Multiple(debits=[("cash", 10)], credits=[("equity", 8), ("retained_earnings", 2)])
print(list(m))

# Run ledger
ledger = Ledger()
for event in events:
    print(event)
    print(ledger.apply(event))
print(ledger.balances)
print(ledger.chart)
assert ledger.balances == {
    "cash": "50",
    "equity": "8",
    "vat": "20",
    "retained_earnings": "22",
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
