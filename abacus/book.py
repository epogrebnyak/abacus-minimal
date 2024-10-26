import json
from collections import UserDict
from copy import deepcopy
from dataclasses import dataclass, field
from pathlib import Path
from typing import Mapping, Sequence

from pydantic import BaseModel

from abacus.chart import Chart, SaveLoadMixin
from abacus.core import AbacusError, Amount, Ledger
from abacus.entry import Entry


class BalancesDict(UserDict[str, Amount], SaveLoadMixin):
    def model_dump_json(self, indent=2) -> str:
        return json.dumps(self.data, default=str, indent=indent)

    @classmethod
    def coerce(cls, d: dict[str, int | float | str]):
        return cls({k: Amount(v) for k, v in d.items()})

    @classmethod
    def model_validate_json(cls, s: str) -> "BalancesDict":
        return cls.coerce(json.loads(s))


class EntryStore(BaseModel, SaveLoadMixin):
    entries: list[Entry] = []


@dataclass
class PathFinder:
    directory: str
    _chart: str = "chart.json"
    _store: str = "store.json"
    _balances: str = "balances.json"

    @property
    def base(self) -> Path:
        return Path(self.directory)

    @property
    def chart(self) -> Path:
        return self.base / self._chart

    @property
    def store(self) -> Path:
        return self.base / self._store

    @property
    def balances(self) -> Path:
        return self.base / self._balances

    def get_chart(self):
        return Chart.load(self.chart)

    def get_store(self):
        return EntryStore.load(self.store)

    def get_balances(self) -> BalancesDict:
        return BalancesDict.load(self.balances)


@dataclass
class Book:
    chart: Chart
    opening_balances: Mapping[str, str | int | float | Amount] | None = None
    store: EntryStore = field(default_factory=EntryStore)

    def set_opening_entry_title(self, text: str = "Opening entry"):
        self.opening_entry_title = text

    def __post_init__(self):
        self.ledger = Ledger.empty(self.chart.mapping)
        self._income_statement = None
        self.set_opening_entry_title()
        if self.opening_balances is not None:
            self.opening_balances = BalancesDict.coerce(self.opening_balances)
            raw_opening_entry = self.chart.mapping.opening_entry(self.opening_balances)
            entry = Entry(title=self.opening_entry_title, data=raw_opening_entry)
            self.post(entry)

    def post(self, entry: Entry):
        self.ledger.post(entry)
        self.store.entries.append(entry)

    def post_many(self, entries: Sequence[Entry]):
        for entry in entries:
            self.post(entry)

    def is_closed(self):
        return self.ledger.is_closed(chart_dict=self.chart.mapping)

    def close(self, closing_entry_title: str = "Closing entry"):
        """Close ledger at period end."""
        # Persist income statement before income and expense accounts are deleted
        self._income_statement = self.income_statement
        # Make closing pairs
        closing_pairs = self.chart.make_closing_pairs(self.chart.retained_earnings)
        # Delete current earnings account from the chart
        last_pair = (self.chart.current_earnings, self.chart.current_earnings)
        closing_pairs.append(last_pair)
        # Post closing entries to ledger
        entries = [
            Entry(closing_entry_title, data=me, is_closing=True)
            for me in self.ledger.close(closing_pairs)
        ]
        # Store closing entries that were posted
        self.store.entries.extend(entries)

    @property
    def income_statement(self):
        if self.is_closed():
            return self._income_statement
        return self.ledger.income_statement(self.chart.mapping)

    @property
    def balance_sheet(self):
        if self.is_closed():
            return self.ledger.balance_sheet(self.chart.mapping)
        else:
            # Close ledger up to current_earnings account
            temp_ledger = deepcopy(self.ledger)
            closing_pairs = self.chart.make_closing_pairs(self.chart.current_earnings)
            temp_ledger.close(closing_pairs)
            return temp_ledger.balance_sheet(self.chart.mapping)

    @property
    def trial_balance(self):
        return self.ledger.trial_balance

    @property
    def balances(self):
        return BalancesDict(**self.ledger.balances)

    @classmethod
    def load(cls, directory: str):
        """Load chart and starting balances from the directory."""
        path = PathFinder(directory)
        try:
            chart = Chart.load(path.chart)
        except FileNotFoundError:
            raise AbacusError(f"Chart file not found: {path.chart}")
        opening_balances = path.get_balances() if path.balances.exists() else {}
        return cls(chart, opening_balances)

    def save(self, directory: str, allow_overwrite: bool = False):
        """Save entries and period end balances to the directory."""
        self.store.save(PathFinder(directory).store, allow_overwrite)
        self.balances.save(PathFinder(directory).balances, allow_overwrite)
