"""User-facing Book class for an accounting ledger."""

from dataclasses import dataclass
from typing import Iterable

from .chart import Chart, Earnings
from .entry import Entry
from .ledger import History, Initial, Ledger


@dataclass
class Book:
    earnings: Earnings
    ledger: Ledger

    @classmethod
    def from_chart(cls, chart: Chart):
        ledger = Ledger.from_accounts(chart.accounts)
        return cls(chart.earnings, ledger)

    @property
    def chart(self) -> Chart:
        return self.earnings.to_chart(self.ledger.history.accounts)

    def open(self, balances: dict):
        self.ledger.apply(Initial(balances))

    def post(self, entry: Entry):
        self.ledger.apply(entry.to_multiple(), entry.title)

    def post_many(self, entries: Iterable[Entry]):
        for entry in entries:
            self.post(entry)

    def close(self):
        self.ledger.close(self.earnings.retained)

    @property
    def balances(self):
        return self.ledger.balances

    @property
    def income_statement(self):
        return self.ledger.income_statement()

    @property
    def balance_sheet(self):
        return self.ledger.balance_sheet(self.earnings.current)

    def save_chart(self, path, allow_overwrite=False):
        self.chart.save(path, allow_overwrite)

    @classmethod
    def from_chart_dump(cls, path):
        chart = Chart.load(path)
        return cls(chart.earnings, Ledger())

    def save_history(self, path, allow_overwrite=False):
        self.ledger.history.save(path, allow_overwrite)

    def load_history(self, path):
        return History.load(path)

    def save(self, chart_path, history_path, allow_overwrite=False):
        """Save book to chart and history JSON files."""
        self.chart.save(chart_path, allow_overwrite)
        self.ledger.history.save(history_path, allow_overwrite)

    @classmethod
    def load_unsafe(cls, chart_path, history_path):
        """Unsafe method to load a Book from chart and history JSON files."""
        self = cls.from_chart_dump(chart_path)
        history = self.load_history(history_path)
        self.ledger = Ledger.from_accounts(history.accounts)
        # pray that the history is consistent with the chart (fixme to cheeck)
        return self
