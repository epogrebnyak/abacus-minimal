"""Chart of accounts and events to modify it.

The primitive events are:

- `Add` and `Offset` to add regular and contra accounts,
- `Drop` to deactivate an empty account.

`Account` is a parent class
for `Asset`, `Equity`, `Liability`, `Income`, and `Expense`.
`Account` is a compound event
that translates to a sequence of `Add` and `Offset` events.

`BaseChart` contains account names and titles.
A list of `Account` objects is enough to define a `BaseChart` object.

`Earnings` class indicates current and retained earnings account names
that we need for closing the ledger at period end.

`Chart` is a serializable chart of accounts that:
-  can be saved and loaded from a flat and readable JSON,
-  has 4 validation methods for consistency checks,
-  contains `BaseChart` and `Earnings` objects.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Iterable, Iterator, Literal

from pydantic import BaseModel, ConfigDict

from .base import T5, AbacusError, Charting, Operation, SaveLoadMixin


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
    def headline(self) -> str:
        return f"{self.tag}:{self.name}"

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


class BaseChart(BaseModel):
    """Chart of accounts without earnings accounts."""

    model_config = ConfigDict(extra="forbid")

    assets: list[str] = []
    equity: list[str] = []
    liabilities: list[str] = []
    income: list[str] = []
    expenses: list[str] = []
    contra_accounts: dict[str, list[str]] = {}
    names: dict[str, str] = {}

    def extend(self, accounts: Iterable["Account"]):
        """Add a list of accounts to chart."""
        for account in accounts:
            self.add_account(account)
        return self

    def add_account(self, account: "Account"):
        """Add account to chart."""
        self.add_string(account.headline)
        for contra in account.contra_accounts:
            self.offset(account.name, contra)
        if account.title:
            self.name(account.name, account.title)
        return self

    def add_string(self, string: str):
        """Add account by slug like `asset:cash` or `a:cash`."""
        prefix, account_name = string.split(":")
        match prefix[0].lower():
            case "a":
                self.assets.append(account_name)
            case "l":
                self.liabilities.append(account_name)
            case "c":
                self.equity.append(account_name)
            case "e":
                match prefix[1].lower():
                    case "q":
                        self.equity.append(account_name)
                    case "x":
                        self.expenses.append(account_name)
                    case _:
                        raise AbacusError(f"Invalid prefix: {prefix}")
            case "i":
                self.income.append(account_name)
            case _:
                raise AbacusError(f"Invalid prefix: {prefix}")
        return self

    def offset(self, account_name: str, contra_name: str):
        """Add contra account to chart."""
        AbacusError.must_exist(self.account_names, account_name)
        if contra_name in self.contra_accounts:
            self.contra_accounts[account_name].append(contra_name)
        else:
            self.contra_accounts[account_name] = [contra_name]
        return self

    def name(self, account_name: str, title: str):
        """Add descriptive account title."""
        self.names[account_name] = title
        return self

    @property
    def matching(self):
        """Match attributes with account classes."""
        return (
            (Asset, "assets"),
            (Equity, "equity"),
            (Liability, "liabilities"),
            (Income, "income"),
            (Expense, "expenses"),
        )

    @property
    def accounts(self) -> list["Account"]:
        """Accounts from this chart."""
        accounts = []
        for cls, attribute in self.matching:
            for account_name in getattr(self, attribute):
                contra_names = self.contra_accounts.get(account_name, [])
                title = self.names.get(account_name, None)
                accounts.append(cls(account_name, contra_names, title))
        return accounts

    @property
    def regular_names(self):
        """All regular account names."""
        return [add.name for add in self.accounts]

    @property
    def contra_names(self) -> list[str]:
        """All contra account names."""
        return [
            name
            for contra_names in self.contra_accounts.values()
            for name in contra_names
        ]

    @property
    def account_names(self) -> list[str]:
        """All accounts in this chart including the duplicates."""
        return self.regular_names + self.contra_names

    @property
    def duplicates(self) -> list[str]:
        """Duplicate account names. Must be empty for valid chart."""
        names = self.account_names
        for name in set(names):
            names.remove(name)
        return names

    def assert_account_names_are_unique(self):
        """Raise error if duplicate account names are found."""
        if ds := self.duplicates:
            raise AbacusError(f"Account names are not unique: {ds}")

    def assert_contra_account_references_are_valid(self):
        """Raise error if any contra account reference is invalid."""
        regular_names = self.regular_names
        for parent_account in self.contra_accounts.keys():
            AbacusError.must_exist(regular_names, parent_account)

    def to_chart(self, current_earnings: str, retained_earnings: str) -> "Chart":
        """Convert to chart using current and retained earnings account names."""
        return Chart(
            **self.model_dump(),
            current_earnings=current_earnings,
            retained_earnings=retained_earnings,
        )


class Chart(BaseChart, SaveLoadMixin):
    """Serializable chart of accounts that is saved to plain JSON."""

    current_earnings: str
    retained_earnings: str

    @classmethod
    def new(
        cls,
        current_earnings: str,
        retained_earnings: str,
        accounts: Iterable["Account"],
    ):
        """Create chart from account classes."""
        self = cls(
            current_earnings=current_earnings, retained_earnings=retained_earnings
        )
        self.extend(accounts)
        return cls(**self.model_dump())

    def model_post_init(self, _):
        self.move_retained_earnings_to_chart()
        self.assert_current_earnings_not_in_chart()
        self.assert_account_names_are_unique()
        self.assert_contra_account_references_are_valid()

    def move_retained_earnings_to_chart(self):
        """Add retained earnings to equity account if not already there."""
        re = Equity(self.retained_earnings)
        if re.name not in self.equity:
            self.add_account(re)

    def assert_current_earnings_not_in_chart(self):
        """Raise error if current earnings is in chart."""
        AbacusError.must_not_exist(self.account_names, self.current_earnings)

    @property
    def base(self) -> BaseChart:
        """Base chart without earnings."""
        dump = self.model_dump()
        del dump["current_earnings"]
        del dump["retained_earnings"]
        return BaseChart(**dump)

    @property
    def earnings(self) -> "Earnings":
        """Earnings account names from this chart."""
        return Earnings(current=self.current_earnings, retained=self.retained_earnings)

    # @property
    # def qualified(self) -> "QualifiedChart":
    #     return QualifiedChart(earnings=self.earnings, base=self.base)


class Earnings(BaseModel):
    current: str
    retained: str

    def to_chart(self, accounts: Iterable["Account"]) -> Chart:
        return BaseChart().extend(accounts).to_chart(self.current, self.retained)


# @dataclass
# class QualifiedChart:
#     earnings: Earnings
#     base: BaseChart

#     def __post_init__(self):
#         self.to_chart()

#     def __iter__(self) -> Iterator["Account"]:
#         return iter(self.base.accounts)

#     def to_chart(self) -> Chart:
#         return self.earnings.to_chart(self.base.accounts)
