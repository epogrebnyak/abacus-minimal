from dataclasses import dataclass

from abacus.core import AbacusError, AccountName, Amount, MultipleEntry

Numeric = int | float | Amount


@dataclass  # make class serialisable for EntryStore
class Entry:
    """An Entry class is a user interface for creatting and manipulating a multiple entry."""

    def __init__(
        self,
        title: str,
        data: MultipleEntry | None = None,
        is_closing: bool = False,
        amount: Numeric | None = None,
    ):
        self.title = title
        self.data = data if data else MultipleEntry()
        self.is_closing = is_closing
        self._current_amount = Amount(amount) if amount else None

    def amount(self, amount: Numeric):
        """Set amount for the entry."""
        self._current_amount = Amount(amount)
        return self

    def _get_amount(self, amount: Numeric | None = None) -> Amount:
        """Use provided amount, default amount or raise error if no data about amount."""
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
