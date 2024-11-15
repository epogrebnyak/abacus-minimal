"""Chart of accounts as Pydantic class."""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Iterable, Iterator, Literal

from pydantic import BaseModel, ConfigDict

from .base import T5, AbacusError, Charting, Operation, SaveLoadMixin


class Chart(BaseModel, SaveLoadMixin):
    """Chart of accounts."""

    model_config = ConfigDict(extra="forbid")

    retained_earnings: str
    current_earnings: str
    assets: list[str] = []
    equity: list[str] = []
    liabilities: list[str] = []
    income: list[str] = []
    expenses: list[str] = []
    contra_accounts: dict[str, list[str]] = {}
    names: dict[str, str] = {}

    @classmethod
    def default(cls):
        """Create chart where mandatory fields are filled."""
        return cls(
            retained_earnings="retained_earnings", current_earnings="current_earnings"
        )

    def offset(self, account_name: str, contra_name: str):
        """Add contra account to chart."""
        AbacusError.must_exist(self.accounts, account_name)
        self.contra_accounts.setdefault(account_name, list()).append(contra_name)
        return self

    def name(self, account_name: str, title: str):
        """Add descriptive account title."""
        self.names[account_name] = title
        return self

    def __post_init__(self):
        self.assert_account_names_are_unique()
        self.assert_contra_account_references_are_valid()

    def assert_account_names_are_unique(self):
        """Raise error if any duplicate account names are found."""
        if duplicates := self.duplicates:
            raise AbacusError(f"Account names are not unique: {duplicates}")

    def assert_contra_account_references_are_valid(self):
        """Raise error if any contra account reference is invalid."""
        regular_account_names = self.regular_names
        for account_names in self.contra_accounts.keys():
            for account_name in account_names:
                AbacusError.must_exist(regular_account_names, account_name)

    @property
    def matching(self):
        """Match attributes with account classes."""
        return (
            (Asset, "assets"),
            (Liability, "liabilities"),
            (Equity, "equity"),
            (Income, "income"),
            (Expense, "expenses"),
        )

    def __iter__(self) -> Iterator["Account"]:  # type: ignore
        """Yield accounts from this chart (all except *current_earnings*).
        Overrides `pydantic.BaseModel.__iter__`.
        """
        for cls, attr in self.matching:
            for account_name in getattr(self, attr):
                contra_names = self.contra_accounts.get(account_name, [])
                title = self.names.get(account_name, None)
                yield cls(account_name, contra_names, title)
        yield Equity(self.retained_earnings)

    @property
    def regular_names(self):
        """All regular account names."""
        return [add.name for add in self]

    @property
    def contra_names(self):
        """All contra account names."""
        return [
            name
            for contra_names in self.contra_accounts.values()
            for name in contra_names
        ]

    @property
    def accounts(self):
        """All accounts in this chart including the duplicates."""
        return self.regular_names + [self.current_earnings] + self.contra_names

    @property
    def duplicates(self):
        """Duplicate account names. Must be empty for valid chart."""
        names = self.accounts
        for name in set(names):
            names.remove(name)
        return names


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
class Account(ABC, Operation, Iterable[Add | Offset]):
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
    tag: Literal["asset"] = "asset"  # type: ignore


@dataclass
class Equity(Account):
    tag: Literal["equity"] = "equity"  # type: ignore


@dataclass
class Liability(Account):
    tag: Literal["liability"] = "liability"  # type: ignore


@dataclass
class Income(Account):
    tag: Literal["income"] = "income"  # type: ignore


@dataclass
class Expense(Account):
    tag: Literal["expense"] = "expense"  # type: ignore
