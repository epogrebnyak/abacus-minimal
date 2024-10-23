from pathlib import Path

from pydantic import BaseModel, ConfigDict

from abacus.core import (
    T5,
    AbacusError,
    AccountName,
    Amount,
    ChartDict,
    Entry,
    Ledger,
    Pair,
)


class SaveLoadMixin:
    """Class for loading and saving Pydantic models to files."""

    @classmethod
    def load(cls, filename: str | Path):
        return cls.model_validate_json(Path(filename).read_text())  # type: ignore

    def save(self, filename: str | Path):
        Path(filename).write_text(self.model_dump_json(indent=2))  # type: ignore


class Chart(BaseModel, SaveLoadMixin):
    """Chart of accounts."""

    model_config = ConfigDict(extra="forbid")

    retained_earnings: str
    assets: list[str] = []
    capital: list[str] = []
    liabilities: list[str] = []
    income: list[str] = []
    expenses: list[str] = []
    contra_accounts: dict[str, list[str]] = {}
    names: dict[str, str] = {}

    def __post_init__(self):
        self.assert_unique()
        self.dry_run()

    @property
    def accounts(self):
        """All accounts in this chart including the duplicates."""
        return (
            self.assets
            + self.capital
            + self.liabilities
            + self.income
            + self.expenses
            + [self.retained_earnings]
            + sum(self.contra_accounts.values(), [])
        )

    @property
    def duplicates(self):
        """Duplicate account names. Must be empty for valid chart."""
        names = self.accounts
        for name in set(names):
            names.remove(name)
        return names

    def assert_unique(self):
        """Raise error if any duplicate account names are found."""
        if names := self.duplicates:
            raise AbacusError(f"Account names are not unique: {names}")

    def dry_run(self):
        """Verify chart by making an empty ledger and try closing it."""
        self.to_ledger().close(chart=self)

    def to_dict(self) -> ChartDict:
        """Create chart dictionary with unique account names."""
        chart_dict = ChartDict()
        for t, attr in (
            (T5.Asset, "assets"),
            (T5.Liability, "liabilities"),
            (T5.Capital, "capital"),
            (T5.Income, "income"),
            (T5.Expense, "expenses"),
        ):
            for account_name in getattr(self, attr):
                chart_dict.set(t, account_name)
        chart_dict.set(T5.Capital, self.retained_earnings)
        # all regular accounts are added, now adding contra accounts
        for account_name, contra_names in self.contra_accounts.items():
            for contra_name in contra_names:
                chart_dict.offset(account_name, contra_name)
        return chart_dict

    def offset(self, account_name: str, contra_name: str):
        """Add contra account to chart."""
        self.contra_accounts.setdefault(account_name, list()).append(contra_name)
        return self

    def name(self, account_name: str, title: str):
        """Add descriptive account title."""
        self.names[account_name] = title
        return self

    @property
    def closing_pairs(self) -> list[Pair]:
        """Return list of tuples that allows to close ledger at period end."""
        return list(self.to_dict().closing_pairs(self.retained_earnings))


