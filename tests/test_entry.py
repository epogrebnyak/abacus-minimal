import pytest

from abacus.core import AbacusError, DebitAccount, MultipleEntry
from abacus.entry import Entry


def test_unbalanced_entry_will_not_pass(toy_ledger):
    with pytest.raises(AbacusError):
        toy_ledger.post(MultipleEntry().credit("cash", 1))


def test_entry_double():
    entry = MultipleEntry.double(debit="cash", credit="equity", amount=10)
    assert entry.debits == [("cash", 10)]
    assert entry.credits == [("equity", 10)]


def test_entry_chained():
    entry = (
        MultipleEntry()
        .debit("cash", 6)
        .debit("ar", 6)
        .credit("sales", 10)
        .credit("vat", 2)
    )
    assert entry.debits == [("cash", 6), ("ar", 6)]
    assert entry.credits == [("sales", 10), ("vat", 2)]


@pytest.mark.entry
def test_closing_entry_for_debit_account():
    account = DebitAccount(20, 5)
    assert account.transfer("this", "that") == MultipleEntry().debit("that", 15).credit(
        "this", 15
    )


def test_entry_amount():
    entry = Entry("Entry with amount").amount(10).debit("cash").credit("equity")
    assert entry.data.debits == [("cash", 10)]
    assert entry.data.credits == [("equity", 10)]


def test_entry_no_amount_raises_error():
    with pytest.raises(AbacusError):
        Entry("Entry with no amount").debit("cash")
