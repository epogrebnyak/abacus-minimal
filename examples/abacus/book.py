"""User-facing Book and Entry classes for an accounting ledger."""

from dataclasses import dataclass
from typing import Iterable

from .chart import Chart
from .entry import Entry
from .ledger import Initial, Ledger


@dataclass
class Book:
    chart: Chart
    ledger: Ledger

    @classmethod
    def from_chart(cls, chart: Chart):
        ledger = Ledger.from_accounts(chart)
        return cls(chart, ledger)

    def open(self, balances: dict):
        self.ledger.apply(Initial(balances))

    def post(self, entry: Entry):
        self.ledger.apply(entry.to_multiple(), entry.title)

    def post_many(self, entries: Iterable[Entry]):
        for entry in entries:
            self.post(entry)

    def close(self):
        self.ledger.close(self.chart.retained_earnings)

    @property
    def balances(self):
        return self.ledger.balances

    @property
    def income_statement(self):
        return self.ledger.income_statement()

    @property
    def balance_sheet(self):
        return self.ledger.balance_sheet(self.chart.current_earnings)

    def save_history(self, path, allow_overwrite=False):
        self.ledger.history.save(path, allow_overwrite)
