from decimal import Decimal
from enum import Enum
from pathlib import Path
from typing import Iterable

Numeric = int | float | Decimal


class Operation(object):
    """A unit of change of the ledger state.

    Types of operations:
    - Account and Charting: change chart of accounts,
    - Posting: change account balances,
    - Closing: compound period end operation on a ledger.
    """


class Charting(Operation):
    """Change chart of accounts."""


class Posting(Operation):
    """Change account balances."""


class Closing(Operation):
    """Close accounts at period end."""


class AbacusError(Exception):
    pass

    @staticmethod
    def must_not_exist(collection: Iterable[str], name: str):
        if name in collection:
            raise AbacusError(f"Account {name} already exists.")

    @staticmethod
    def must_exist(collection: Iterable[str], name: str):
        if name not in collection:
            raise AbacusError(f"Account {name} not found.")


class T5(Enum):
    Asset = "asset"
    Liability = "liability"
    Equity = "equity"
    Income = "income"
    Expense = "expense"

    def __repr__(self):
        return self.value.capitalize()


class SaveLoadMixin:
    """A mix-in class for loading and saving pydantic models to files."""

    @classmethod
    def load(cls, filename: str | Path):
        return cls.model_validate_json(Path(filename).read_text())  # type: ignore

    def save(self, filename: str | Path, allow_overwrite: bool = False):
        if not allow_overwrite and Path(filename).exists():
            raise FileExistsError(f"File already exists: {filename}")
        content = self.model_dump_json(indent=2, warnings=False)  # type: ignore
        Path(filename).write_text(content)  # type: ignore
