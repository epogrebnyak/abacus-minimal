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

from dataclasses import dataclass
from decimal import Decimal
from enum import Enum


class T5(Enum):
    Asset = "asset"
    Liability = "liability"
    Equity = "equity"
    Income = "income"
    Expense = "expense"


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

from dataclasses import dataclass, field
from typing import ClassVar

@dataclass
class Account:
   name: str
   contra_accounts: list[str] = field(default_factory=list)
   t: ClassVar[T5 | None]  = None 

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
    accounts: dict[str, TAccount]
    contra_accounts: dict[str, list[str]]

    @property
    def balances(self):
        return {name: account.balance for name, account in self.accounts.items()}

    @classmethod
    def from_accounts(cls, *accounts):            
        accounts_dict = {}
        contra_names_dict = {}
        for account in accounts:
            accounts_dict[account.name] = TAccount(Regular(account.t))
            for contra_name in account.contra_accounts:
                accounts_dict[contra_name] = TAccount(Contra(account.t))
                contra_names_dict.setdefault(account.name,[]).append(contra_name)
        return cls(accounts_dict, contra_names_dict)
    
    def net_balance(self):
        pass


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
ledger.accounts["equity"].debit(Decimal("10"))
print(ledger.balances)
