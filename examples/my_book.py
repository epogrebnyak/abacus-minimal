from dataclasses import dataclass
from chart import Chart
from events import Ledger
from typing import Sequence


@dataclass
class Entry:
    pass


@dataclass
class Book:
    chart: Chart
    ledger: Ledger

    @classmethod 
    def from_chart(cls, chart: Chart):
        ledger = Ledger.from_accounts(chart.accounts)
        return cls(chart, ledger)
    
    def init(self, balances: dict):
        pass

    def post(self, entry: Entry):
        pass

    def post_many(self, entries: Sequence[Entry]):
        for entry in entries:
            self.post(entry)

    def close(self):
        self.ledger.close(self.chart.retained_earnings)
        
    @property    
    def income_statement(self):
        pass

    @property
    def balance_sheet(self):
        pass
