from pathlib import Path

from pydantic import BaseModel, ConfigDict

from abacus.core import T5, AbacusError, AccountName, ChartDict, Pair


class SaveLoadMixin:
    """Class for loading and saving pydantic models to files."""

    @classmethod
    def load(cls, filename: str | Path):
        return cls.model_validate_json(Path(filename).read_text())  # type: ignore

    def save(self, filename: str | Path):
        Path(filename).write_text(self.model_dump_json(indent=2))  # type: ignore


class Chart(BaseModel, SaveLoadMixin):
    """Chart of accounts."""

    model_config = ConfigDict(extra="forbid")

    retained_earnings: str
    current_earnings: str
    assets: list[str] = []
    capital: list[str] = []
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

    def __post_init__(self):
        self.assert_all_account_names_are_unique()
        self.assert_no_invalid_contra_account_references()

    @property
    def regular_accounts(self):
        return (
            self.assets + self.capital + self.liabilities + self.income + self.expenses
        ) + [self.retained_earnings, self.current_earnings]

    @property
    def accounts(self):
        """All accounts in this chart including the duplicates."""
        return self.regular_accounts + sum(self.contra_accounts.values(), [])

    @property
    def duplicates(self):
        """Duplicate account names. Must be empty for valid chart."""
        names = self.accounts
        for name in set(names):
            names.remove(name)
        return names

    def assert_all_account_names_are_unique(self):
        """Raise error if any duplicate account names are found."""
        if names := self.duplicates:
            raise AbacusError(f"Account names are not unique: {names}")

    def assert_no_invalid_contra_account_references(self):
        """Verify chart by making an empty ledger and try closing it."""
        regular_account_names = self.regular_accounts
        for account_name in self.contra_accounts.keys():
            if account_name not in regular_account_names:
                raise AbacusError(
                    f"Account name should exist before making a contra account: {account_name}"
                )

    def _regular_accounts(self):
        for t, attr in (
            (T5.Asset, "assets"),
            (T5.Liability, "liabilities"),
            (T5.Capital, "capital"),
            (T5.Income, "income"),
            (T5.Expense, "expenses"),
        ):
            for account_name in getattr(self, attr):
                yield t, account_name

    def _contra_accounts(self):
        for account_name, contra_names in self.contra_accounts.items():
            for contra_name in contra_names:
                yield account_name, contra_name

    @property
    def mapping(self) -> ChartDict:
        """Create chart dictionary with unique account names."""
        chart_dict = ChartDict()
        for t, account_name in self._regular_accounts():
            chart_dict.set(t, account_name)
        chart_dict.set(T5.Capital, self.retained_earnings)
        chart_dict.set(T5.Capital, self.current_earnings)
        for account_name, contra_name in self._contra_accounts():
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

    def make_closing_pairs(self, accumulation_account: AccountName) -> list[Pair]:
        """Return list of tuples that allows to close ledger."""
        return list(self.mapping.closing_pairs(accumulation_account))

    # @property
    # def closing_pairs(self) -> list["Pair"]:
    #     """Return list of tuples that allows to close ledger at period end."""
    #     return self.make_closing_pairs(self.retained_earnings)
