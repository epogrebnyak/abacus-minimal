from copy import deepcopy
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
        pydantic_data = BalancesDict.load(self.balances)
        return pydantic_data.root


class Book:
    def __init__(
        self,
        chart: Chart,
        opening_balances: dict | None = None,
        opening_entry_title: str = "Opening entry",
    ):
        self.chart = chart
        self.ledger = Ledger.empty(chart.mapping)
        self.store = EntryStore()
        self._income_statement = None
        if opening_balances is not None:
            self.ledger, me = self.ledger.post_opening(chart.mapping, opening_balances)
            entry = Entry(title=opening_entry_title, data=me)
            self.store.entries.append(entry)

    def is_closed(self):
        return self.ledger.is_closed(chart_dict=self.chart.mapping)

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
        """Close ledger"""
        # Persist income statement
        self._income_statement = self.income_statement
        # Make closing pairs
        closing_pairs = self.chart.make_closing_pairs(self.chart.retained_earnings)
        # Trick for deleting the *current_earnings* account from the chart
        last_pair = self.chart.current_earnings, self.chart.current_earnings
        closing_pairs.append(last_pair)
        # Post closing entries
        entries = self.ledger.close(closing_pairs)
        entries = [Entry(closing_entry_title, data=e, is_closing=True) for e in entries]
        self.store.entries.extend(entries)

    @property
    def income_statement(self):
        if self.is_closed():
            return self._income_statement
        return self.ledger.income_statement(self.chart.mapping)

    @property
    def balance_sheet(self):
        if self.is_closed():
            return self._retained_earnings_balance_sheet()
        return self._current_profit_balance_sheet()

    def _current_profit_balance_sheet(self):
        # Keep both current_earnings and retained_earnings in the balance sheet
        ledger = deepcopy(self.ledger)
        profit = self.chart.current_earnings
        closing_pairs = self.chart.make_closing_pairs(profit)
        ledger.close(closing_pairs)
        return ledger.balance_sheet(self.chart.mapping)

    def _retained_earnings_balance_sheet(self):
        # Keep only retained_earnings in the balance sheet
        return self.ledger.balance_sheet(self.chart.mapping)

    @property
    def trial_balance(self):
        return self.ledger.trial_balance
