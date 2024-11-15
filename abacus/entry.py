from dataclasses import dataclass, field
from decimal import Decimal
from typing import Iterable, Literal

from .base import AbacusError, Numeric, Posting


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
class Double(Posting, Iterable[Debit | Credit]):
    """Double-entry transaction."""

    debit: str
    credit: str
    amount: int | float | Decimal
    tag: Literal["double"] = "double"

    def __iter__(self):
        yield Debit(self.debit, self.amount)
        yield Credit(self.credit, self.amount)


@dataclass
class Unbalanced(Posting, Iterable[Debit | Credit]):
    """Multiple entry that is not guaranteed to be balanced by debits and credits."""

    debits: list[tuple[str, Numeric]] = field(default_factory=list)
    credits: list[tuple[str, Numeric]] = field(default_factory=list)

    def __iter__(self):
        for account, amount in self.debits:
            yield Debit(account, Decimal(amount))
        for account, amount in self.credits:
            yield Credit(account, Decimal(amount))

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

    @classmethod
    def from_list(cls, singles):
        entry = Unbalanced()
        for s in singles:
            match s:
                case Debit(account, amount):
                    entry.debits.append((account, amount))
                case Credit(account, amount):
                    entry.credits.append((account, amount))
        return entry.to_multiple()

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
    tag: Literal["initial"] = "initial"


@dataclass
class Entry:
    title: str
    debits: list[tuple[str, Decimal]] = field(default_factory=list)
    credits: list[tuple[str, Decimal]] = field(default_factory=list)
    _amount: Decimal | None = None

    def amount(self, amount):
        """Set amount for the entry."""
        self._amount = Decimal(amount)
        return self

    def get_amount(self, amount: Numeric | None = None) -> Decimal:
        """Use provided amount, default amount or raise error if no data about amount."""
        if amount is None:
            if self._amount:
                return self._amount
            else:
                raise AbacusError("Amount is not set.")
        else:
            return Decimal(amount)

    def debit(self, account, amount=None):
        amount = self.get_amount(amount)
        self.debits.append((account, amount))
        return self

    def credit(self, account, amount=None):
        amount = self.get_amount(amount)
        self.credits.append((account, amount))
        return self

    def double(self, debit, credit, amount):
        self.debit(debit, amount)
        self.credit(credit, amount)
        return self

    def to_multiple(self):
        return Multiple(debits=self.debits, credits=self.credits)  # type: ignore

    def __iter__(self):
        return iter(self.to_multiple())
