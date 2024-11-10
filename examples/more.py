# # which should I use: chart1, chart2, chart3
# chart1 = Chart(
#     retained_earnings="retained_earnings",
#     current_earnings="current_earnings",
#     assets=["cash", "inventory"],
#     capital=["equity"],
#     income=["sales"],
#     expenses=["salaries", "cogs"],
#     contra_accounts={"sales": ["refunds", "voids"]},
# )

# chart2 = Chart.from_string(
#     """
# asset:cash,inventory
# capital:equity
# income:sales^(refunds,voids)
# expense:salaries
# expense:cogs
# re:retained_earnings
# isa:current_earnings
# """
# )

# chart3 = Chart.from_accounts(
#     Asset("cash"),
#     Asset("inventory"),
#     Capital("equity"),
#     Income("sales", contra_accounts=["refunds", "voids"]),
#     Expense("salaries"),
#     Expense("cogs"),
#     RetainedEarnings("re"),
#     CurrentEarnings("isa"),
# )

from abc import ABC
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


@dataclass
class Regular:
    t: T5


@dataclass
class Contra:
    t: T5


@dataclass
class TAccount:
    type: Regular | Contra
    balance: Decimal = Decimal(0)

    def is_debit_account(self) -> bool:
        match self.type:
            case Regular(t):
                return t in (T5.Asset, T5.Expense)
            case Contra(t):
                return t in (T5.Capital, T5.Liability, T5.Income)

    def debit(self, amount: Decimal):
        if self.is_debit_account():
            self.balance += amount
        else:
            self.balance -= amount

    def credit(self, amount: Decimal):
        self.debit(-amount)

    def transfer(self, this, that):
        if self.is_debit_account():
            return double(this, that, self.balance)
        else:
            return double(this, that, self.balance)


@dataclass
class SingleEntry:
    account: str
    amount: Decimal


class Debit(SingleEntry):
    pass


class Credit(SingleEntry):
    pass


MultipleEntry = list[SingleEntry]


def double(debit, credit, amount) -> MultipleEntry:
    return [Debit(debit, amount), Credit(credit, amount)]


@dataclass
class Account(ABC):
    name: str
    contra_accounts: list[str] = field(default_factory=list)
    t: ClassVar[T5 | None] = None

    def offset(self, contra_name: str):
        self.contra_accounts.append(contra_name)
        return self


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


class Acc:
    t: T5
    balance: Decimal = Decimal(0)
    is_contra_account: bool = False


@dataclass
class Ledger:
    accounts: dict[str, TAccount]
    contra_accounts: dict[str, list[str]]
    chart_dict: dict[str, Regular | Contra] = field(default_factory=dict)

    @property
    def balances(self):
        return {name: account.balance for name, account in self.accounts.items()}

    @classmethod
    def from_accounts(cls, *accounts):
        accounts_dict = {}
        contra_names_dict = {}
        chart_dict = {}
        for account in accounts:
            accounts_dict[account.name] = TAccount(Regular(account.t))
            for contra_name in account.contra_accounts:
                accounts_dict[contra_name] = TAccount(Contra(account.t))
                contra_names_dict.setdefault(account.name, []).append(contra_name)
        return cls(accounts_dict, contra_names_dict, chart_dict)

    def net_balance(self, account_name: str):
        account = self.accounts[account_name]
        contra_names = self.contra_accounts.get(account_name, [])
        return account.balance - sum(
            self.accounts[name].balance for name in contra_names
        )

    def by_type(self, t: T5):
        return [
            name
            for name, account in self.accounts.items()
            if account.type == Regular(t)
        ]

    def closing_pairs(self, earnings_account: str):
        for t in (T5.Income, T5.Expense):
            for name in self.by_type(t):
                for contra_name in self.contra_accounts.get(name, []):
                    yield contra_name, name
                yield name, earnings_account

    def post(self, me: MultipleEntry):
        for entry in me:
            match entry:
                case Debit(name, amount):
                    self.accounts[name].debit(amount)
                case Credit(name, amount):
                    self.accounts[name].credit(amount)

    def close(self, earnings_account: str):
        for this, that in self.closing_pairs(earnings_account):
            entry = self.accounts[this].transfer(this, that)
            self.post(entry)
            del self.accounts[this]


ledger = Ledger.from_accounts(
    Asset("cash"),
    Asset("inventory"),
    Equity("equity"),
    Income("sales", contra_accounts=["refunds", "voids"]),
    Expense("salaries"),
    Expense("cogs"),
    Equity("retained_earnings"),
    Equity("current_earnings"),
)
print(ledger)
ledger.accounts["cash"].debit(Decimal("10"))
ledger.accounts["equity"].credit(Decimal("10"))
print(ledger.balances)
