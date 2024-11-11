from abc import ABC, abstractmethod
from collections import UserDict
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


class Posting:
    """Change account balances."""

    pass


class Closing:
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
class Transfer(Closing):
    """Transfer account balance to another account."""

    from_account: str
    to_account: str


@dataclass
class Close(Closing):
    """Close ledger to earnings account."""

    earnings_account: str


@dataclass
class Debit(Posting, Primitive):
    """Increase debit-normal accounts, decrease credit-normal accounts."""

    account: str
    amount: int | float | Decimal


@dataclass
class Credit(Posting, Primitive):
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
    """Multiple entry, balanced by debit and credit."""

    debits: list[tuple[str, int | float | Decimal]] = field(default_factory=list)
    credits: list[tuple[str, int | float | Decimal]] = field(default_factory=list)

    def __iter__(self):
        for account, amount in self.debits:
            yield Debit(account, Decimal(amount))
        for account, amount in self.credits:
            yield Credit(account, Decimal(amount))

    def debit(self, account, amount):
        self.debits.append((account, amount))
        return self

    def credit(self, account, amount):
        self.credits.append((account, amount))
        return self

    def balances(self):
        ds = sum(x for (_, x) in self.debits)
        cs = sum(x for (_, x) in self.credits)
        return ds, cs

    def is_balanced(self):
        ds, cs = self.balances()
        return ds == cs

    def to_multiple(self):
        return Multiple(debits=self.debits, credits=self.credits)


class Multiple(Unbalanced):
    """Multiple entry, balanced by debit and credit."""

    def validate(self):
        ds, cs = self.balances()
        if ds != cs:
            raise AbacusError(
                f"Debits {ds} and credits {cs} are not balanced for {self}."
            )
        return self


Numeric = int | float | Decimal


@dataclass
class Initial(Posting):
    """Open ledger with initial balances."""

    balances: list[tuple[str, Numeric]]


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
class Event:
    primitives: list[Primitive]


class ChartDict(UserDict[str, T5 | Contra]):
    def by_type(self, t: T5) -> list[str]:
        return [name for name, account_type in self.data.items() if account_type == t]

    def find_contra_accounts(self, name: str) -> list[str]:
        return [
            contra_name
            for contra_name, parent in self.data.items()
            if parent == Contra(name)
        ]

    def closing_pairs(self, earnings_account: str) -> Iterable[Transfer]:
        for t in (T5.Income, T5.Expense):
            for account in self.by_type(t):
                for contra in self.find_contra_accounts(account):
                    yield Transfer(contra, account)
                yield Transfer(account, earnings_account)


@dataclass
class Ledger:
    accounts: dict[str, TAccount] = field(default_factory=dict)
    chart: ChartDict = field(default_factory=ChartDict)
    events: list[Event] = field(default_factory=list)

    @classmethod
    def from_list(cls, events: list[Charting | Posting | Closing]):
        ledger = cls()
        for event in events:
            ledger.apply(event)
        return ledger

    @property
    def balances(self):
        return {name: float(account.balance) for name, account in self.accounts.items()}

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
                ub = Unbalanced()
                for account, amount in balances:
                    if self.is_debit_account(account):
                        ub.debit(account, amount)
                    else:
                        ub.credit(account, amount)
                if ub.is_balanced():
                    return self.apply_entry(ub.to_multiple())
                else:
                    raise AbacusError(f"Unbalanced opening {balances}")
            case Multiple(_, _):
                for e in entry:
                    self.apply_entry(e)
                return list(entry)
            case _:
                raise AbacusError(f"Unknown event {entry}")

    def apply_compound(self, compound: Closing) -> list[Primitive]:
        match compound:
            case Transfer(from_account, to_account):
                transfer_entry = self.transfer_entry(from_account, to_account)
                events = list(transfer_entry) + [Drop(from_account)]
                self.apply_many(events)
                return events
            case Close(earnings_account):
                events = []
                for t in self.chart.closing_pairs(earnings_account):
                    events.extend(self.apply(t))
                return events
            case _:
                raise AbacusError(f"Unknown event {compound}")

    def apply_many(self, incoming) -> list[Primitive]:
        events = list(incoming)
        for event in events:
            self.apply(event)
        return events

    def apply(self, incoming: Charting | Posting | Closing) -> list[Primitive]:
        if isinstance(incoming, Charting):
            prims = self.apply_charting(incoming)
        elif isinstance(incoming, Posting):
            prims = self.apply_entry(incoming)
        elif isinstance(incoming, Closing):
            prims = self.apply_compound(incoming)
        else:
            raise AbacusError(f"Unknown type {incoming}")
        self.events.append(Event(prims))
        return prims


events: list[Account | Closing | Posting] = [
    # Create accounts
    Asset("cash"),
    Equity("equity"),
    Income("services", contra_accounts=["refunds", "voids"]),
    Liability("vat", title="VAT due to tax authorities"),
    Expense("salaries"),
    Equity("retained_earnings"),
    # Start ledger with initial balances
    Initial(balances=[("cash", 10), ("equity", 8), ("retained_earnings", 2)]),
    # Make transactions
    Multiple().debit("cash", 120).credit("services", 100).credit("vat", 20),
    Double("refunds", "cash", 20),
    Double("voids", "cash", 10),
    Double("salaries", "cash", 50),
    # Close accounts
    Close("retained_earnings"),
]

m = Multiple(debits=[("cash", 10)], credits=[("equity", 8), ("retained_earnings", 2)])
assert list(m) == [Debit(account='cash', amount=Decimal('10')), Credit(account='equity', amount=Decimal('8')), Credit(account='retained_earnings', amount=Decimal('2'))]

# Run ledger
ledger = Ledger.from_list(events)
print(ledger.balances)
print(ledger.chart)
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
