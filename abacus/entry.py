from dataclasses import dataclass, field

from abacus.core import AbacusError, AccountName, Amount, MultipleEntry

Numeric = int | float | Amount


@dataclass
class Entry:
    """An Entry class is a user interface for manipulating a multiple entry."""

    title: str
    data: MultipleEntry = field(default_factory=MultipleEntry)
    is_closing: bool = False
    _current_amount: Amount | None = None

    @classmethod
    def new(cls, multiple_entry: MultipleEntry, title: str):
        """Create new entry."""
        return cls(title, data=multiple_entry)

    def amount(self, amount: Numeric):
        """Set amount for the entry."""
        self._current_amount = Amount(amount)
        return self

    def _get_amount(self, amount: Numeric | None = None) -> Amount:
        """Use provided amount, default amount or raise error if no sufficient data for amount."""
        if amount is None:
            if self._current_amount:
                return self._current_amount
            else:
                raise AbacusError("Amount is not set.")
        return Amount(amount)

    def debit(self, account_name: AccountName, amount: Numeric | None = None):
        """Add debit part to entry."""
        self.data.debit(account_name, self._get_amount(amount))
        return self

    def credit(self, account_name: AccountName, amount: Numeric | None = None):
        """Add credit part to entry."""
        self.data.credit(account_name, self._get_amount(amount))
        return self

    def double(self, debit: AccountName, credit: AccountName, amount: Numeric):
        """Create double entry."""
        self.data = MultipleEntry.double(debit, credit, Amount(amount))
        return self

    def __iter__(self):
        return iter(self.data)
