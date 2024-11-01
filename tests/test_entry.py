import pytest

from abacus.core import AbacusError, Credit, Debit, DebitAccount, double
from abacus.entry import Entry


def test_unbalanced_entry_will_not_pass(toy_ledger):
    with pytest.raises(AbacusError):
        toy_ledger.post([Credit("cash", 1)])


def test_entry_double():
    assert double(debit="cash", credit="equity", amount=10) == [
        Debit("cash", 10),
        Credit("equity", 10),
    ]


@pytest.mark.entry
def test_closing_entry_for_debit_account():
    account = DebitAccount(20, 5)
    assert account.transfer("this", "that") == [Debit("that", 15), Credit("this", 15)]


def test_entry_amount():
    entry = Entry("Entry with amount", amount=10).debit("cash").credit("equity")
    assert entry.posting == [Debit("cash", 10), Credit("equity", 10)]


def test_entry_no_amount_raises_error():
    with pytest.raises(AbacusError):
        Entry("Entry with no amount").debit("cash")


def test_entry_has_amount_in_constructor():
    assert Entry("Aha", amount=10).debit("cash").posting == [Debit("cash", 10)]


def test_entry_amount_may_change():
    assert Entry("Bis", amount=10).set_amount(200).debit("cash").posting == [
        Debit("cash", 200)
    ]
