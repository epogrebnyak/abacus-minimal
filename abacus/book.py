from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Sequence

from pydantic import BaseModel, RootModel

from abacus.chart import Chart, SaveLoadMixin
from abacus.core import AbacusError, AccountName, Amount, Ledger
from abacus.entry import Entry


class BalancesDict(RootModel[Dict[str, Amount]], SaveLoadMixin):
    pass


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

    def get_balances(self) -> dict[AccountName, Amount]:
        return BalancesDict.load(self.balances).root  # note the root part


class Book:
    def __init__(self, chart: Chart, opening_balances: dict | None = None):
        self.chart = chart
        if opening_balances is None:
            opening_balances = {}
        self.ledger = Ledger.open(chart.to_dict(), opening_balances)
        self.store = EntryStore()

    def save_chart(self, directory: str):
        self.chart.save(PathFinder(directory).chart)

    def save_store(self, directory: str):
        self.store.save(PathFinder(directory).store)

    def save_balances(self, directory: str):
        BalancesDict(self.ledger.balances).save(PathFinder(directory).balances)

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

    def open(self, starting_balances=None, opening_entry_title="Opening entry"):
        chart_dict = self.chart.to_dict()
        self.ledger = chart_dict.to_ledger()
        if starting_balances:
            entry = Entry(opening_entry_title).opening(starting_balances, chart_dict)
            self.ledger.post(entry)
        return self

    def save(self, directory: str):
        self.save_chart(directory)
        self.save_store(directory)
        self.save_balances(directory)

    def post(self, entry: Entry):
        self.ledger.post(entry)
        self.store.entries.append(entry)

    def post_many(self, entries: Sequence[Entry]):
        for entry in entries:
            self.post(entry)

    def close(self, closing_entry_title: str = "Closing entry"):
        entries = self.ledger.close(closing_pairs=self.chart.closing_pairs)
        entries = [Entry(closing_entry_title, data=e, is_closing=True) for e in entries]
        self.store.entries.extend(entries)

    # FIXME: no income statement if account was closed
    @property
    def income_statement(self):
        return self.ledger.income_statement(self.chart.to_dict())

    # FIXME: balance sheet is incomplete before accounts were closed
    @property
    def balance_sheet(self):
        return self.ledger.balance_sheet(self.chart.to_dict())

    @property
    def trial_balance(self):
        return self.ledger.trial_balance
