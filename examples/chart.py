from events import (
    AbacusError,
    Asset,
    Equity,
    Expense,
    Income,
    Liability,
    must_exist,
)
from mixin import SaveLoadMixin
from pydantic import BaseModel, ConfigDict


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
        return cls(
            retained_earnings="retained_earnings", current_earnings="current_earnings"
        )

    def offset(self, account_name: str, contra_name: str):
        """Add contra account to chart."""
        must_exist(self.accounts, account_name)
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
        if dups := self.duplicates:
            raise AbacusError(f"Account names are not unique: {dups}")

    def assert_contra_account_references_are_valid(self):
        """Raise error if any contra account reference is invalid."""
        regular_account_names = self.regular_accounts
        for account_names in self.contra_accounts.keys():
            for account_name in account_names:
                if account_name not in regular_account_names:
                    must_exist(account_name)

    @property
    def matcher(self):
        return (
            (Asset, "assets"),
            (Liability, "liabilities"),
            (Equity, "equity"),
            (Income, "income"),
            (Expense, "expenses"),
        )
                        
    @property
    def account_directives(self):
        for cls, attr in self.matcher:
            for account_name in getattr(self, attr):
                contra_names = self.contra_accounts.get(account_name, [])
                title = self.names.get(account_name, None)
                yield cls(account_name, contra_names, title)
        yield Equity(self.retained_earnings)

    @property
    def regular_names(self):
        return [add.name for add in self.accounts_directives]

    @property
    def contra_names(self):
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
