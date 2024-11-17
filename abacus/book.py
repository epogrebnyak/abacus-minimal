"""User-facing Book and Entry classes for an accounting ledger."""

from dataclasses import dataclass
from typing import Iterable

from .chart import Account, Chart, Earnings, ChartBase, QualifiedChart
from .entry import Entry
from .ledger import History, Initial, Ledger


@dataclass
class Book:
    earnings: Earnings
    ledger: Ledger

    @property
    def chart(self) -> QualifiedChart:
        accounts = [a for a in self.ledger.history.actions if isinstance(a, Account)]
        base = ChartBase.from_accounts(accounts)
        return QualifiedChart(earnings=self.earnings, base=base)

    @classmethod
    def from_chart(cls, chart: Chart):
        ledger = Ledger.from_accounts(chart.accounts)
        return cls(chart.earnings, ledger)

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

    def save_history(self, path, allow_overwrite=False):
        self.ledger.history.save(path, allow_overwrite)

    @classmethod
    def load(cls, history_path):
        history = History.load(history_path)
        chart = Chart.from_accounts(
            [a for a in history.actions if isinstance(a, Account)]
        )
        ledger = Ledger.from_list(history.actions)
        return cls(chart.earnings, ledger)
