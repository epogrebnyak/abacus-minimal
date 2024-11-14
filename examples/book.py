from dataclasses import dataclass, field
from decimal import Decimal
from typing import Iterable

from chart import Chart
from events import Initial, Ledger, Multiple


@dataclass
class Entry:
    title: str
    debits: list[tuple[str, Decimal]] = field(default_factory=list)
    credits: list[tuple[str, Decimal]] = field(default_factory=list)
    _amount: Decimal | None = None

    def amount(self, amount):
        """Set amount for the entry."""
        self._amount = Decimal(amount)
        return self

    def get_amount(self, amount=None):
        """Use provided amount, default amount or raise error if no data about amount."""
        if amount is None and self._amount:
            if self._amount:
                return self._amount
            else:
                raise ValueError("Amount is not set.")
        return Decimal(amount)

    def debit(self, account, amount=None):
        amount = self.get_amount(amount)
        self.debits.append((account, amount))
        return self

    def credit(self, account, amount=None):
        amount = self.get_amount(amount)
        self.credits.append((account, amount))
        return self

    def double(self, debit, credit, amount):
        self.debit(debit, amount)
        self.credit(credit, amount)
        return self

    def to_multiple(self):
        return Multiple(debits=self.debits, credits=self.credits)


@dataclass
class Book:
    chart: Chart
    ledger: Ledger

    @classmethod
    def from_chart(cls, chart: Chart):
        ledger = Ledger.from_accounts(chart.primitives)
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
