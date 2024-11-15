import pytest

from abacus import AbacusError, Credit, Debit, Double, Entry
from abacus.ledger import DebitAccount


@pytest.mark.skip
def test_unbalanced_entry_will_not_pass(toy_ledger):
    with pytest.raises(AbacusError):
        toy_ledger.apply([Credit("cash", 1)])


def test_entry_double():
    assert list(Double(debit="cash", credit="equity", amount=10)) == [
        Debit("cash", 10),
        Credit("equity", 10),
    ]


@pytest.mark.entry
def test_closing_entry_for_debit_account():
    assert DebitAccount(20).transfer("this", "that") == Double("that", "this", 20)


def test_entry_amount():
    entry = Entry("Entry with amount").amount(10).debit("cash").credit("equity")
    assert list(entry) == [Debit("cash", 10), Credit("equity", 10)]


def test_entry_no_amount_raises_error():
    with pytest.raises(AbacusError):
        Entry("Entry with no amount").debit("cash")
