from dataclasses import dataclass, field

from abacus.core import AbacusError, AccountName, Amount, MultipleEntry

Numeric = int | float | Amount


@dataclass  # make class serialisable for EntryStore
class Entry:
    """An Entry class is a user interface for creatting and manipulating a multiple entry."""

    title: str
    data: MultipleEntry = field(default_factory=MultipleEntry)
    is_closing: bool = False
    amount: Numeric | None = None

    def set_amount(self, amount: Numeric):
        """Set amount for the entry."""
        self.amount = Amount(amount)
        return self

    def get_amount(self, amount: Numeric | None = None) -> Amount:
        """Use provided amount, default amount or raise error if no data about amount."""
        if amount is None:
            if self.amount:
                return Amount(self.amount)
            else:
                raise AbacusError("Amount is not set.")
        return Amount(amount)

    def debit(self, account_name: AccountName, amount: Numeric | None = None):
        """Add debit part to entry."""
        self.data.debit(account_name, self.get_amount(amount))
        return self

    def credit(self, account_name: AccountName, amount: Numeric | None = None):
        """Add credit part to entry."""
        self.data.credit(account_name, self.get_amount(amount))
        return self

    def double(self, debit: AccountName, credit: AccountName, amount: Numeric):
        """Create double entry."""
        self.data = MultipleEntry.double(debit, credit, Amount(amount))
        return self
