from pathlib import Path

from pydantic import BaseModel, ConfigDict

from events import T5, AbacusError, Add, Offset, must_exist


class SaveLoadMixin:
    """Class for loading and saving pydantic models to files."""

    @classmethod
    def load(cls, filename: str | Path):
        return cls.model_validate_json(Path(filename).read_text())  # type: ignore

    def save(self, filename: str | Path, allow_overwrite: bool = False):
        if not allow_overwrite and Path(filename).exists():
            raise FileExistsError(f"File already exists: {filename}")
        Path(filename).write_text(self.model_dump_json(indent=2))  # type: ignore


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

    def __post_init__(self):
        self.assert_all_account_names_are_unique()
        self.assert_no_invalid_contra_account_references()

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
        """Raise error if any contra account reference is invalid."""
        regular_account_names = self.regular_accounts
        for account_names in self.contra_accounts.keys():
            for account_name in account_names:
                if account_name not in regular_account_names:
                    must_exist(account_name)

    def _regular_accounts(self):
        for t, attr in (
            (T5.Asset, "assets"),
            (T5.Liability, "liabilities"),
            (T5.Equity, "equity"),
            (T5.Income, "income"),
            (T5.Expense, "expenses"),
        ):
            for account_name in getattr(self, attr):
                yield Add(account_name, t)
        yield Add(self.retained_earnings, T5.Equity)
        yield Add(self.current_earnings, T5.Equity)        

    def _contra_accounts(self):
        for account_name, contra_names in self.contra_accounts.items():
            for contra_name in contra_names:
                yield Offset(parent=account_name, name=contra_name)

    @property
    def regular_accounts(self):
        return [add.name for add in self._regular_accounts()]

    @property
    def contra_accounts(self):
        return [offset.name for offset in self._contra_accounts()]

    @property
    def accounts(self):
        """All accounts in this chart including the duplicates."""
        return self.regular_accounts + self.contra_accounts

    @property
    def primitives(self):
        return list(self._regular_accounts()) + list(self._contra_accounts())            

    def offset(self, account_name: str, contra_name: str):
        """Add contra account to chart."""
        self.contra_accounts.setdefault(account_name, list()).append(contra_name)
        return self

    def name(self, account_name: str, title: str):
        """Add descriptive account title."""
        self.names[account_name] = title
        return self
