from dataclasses import dataclass, field
from decimal import Decimal
from events import AbacusError, Multiple

Numeric = int | float | Decimal

@dataclass
class Entry:
    title: str
    posting: Multiple = field(default_factory=Multiple)
    amount: Decimal | None = None

    def set_amount(self, amount: Numeric):
        """Set amount for the entry."""
        self.amount = Decimal(amount)
        return self

    def get_amount(self, amount: Numeric | None = None) -> Decimal:
        """Use provided or default amount or raise error if no data about amount."""
        if amount is None:
            if self.amount:
                return Decimal(self.amount)
            else:
                raise AbacusError("Amount is not set.")
        return Decimal(amount)

    def debit(self, account_name: str, amount: Numeric | None = None):
        """Add debit part to entry."""
        self.posting.debit(account_name, self.get_amount(amount))
        return self

    def credit(self, account_name: str, amount: Numeric | None = None):
        """Add credit part to entry."""
        self.posting.credit(account_name, self.get_amount(amount))
        return self

    def double(self, debit: str, credit: str, amount: Numeric):
        """Create double entry."""
        self.posting.debit(debit, amount)
        self.posting.credit(credit, amount)
        return self
