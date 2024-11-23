from decimal import Decimal

import pytest
from pytest import fixture

from abacus import AbacusError, Credit, Debit, Double, Entry, Multiple
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


def test_double():
    d = Double("cash", "equity", 10)
    assert list(d) == [
        Debit(account="cash", amount=Decimal(10)),
        Credit(account="equity", amount=Decimal(10)),
    ]


@fixture
def reference_entry():
    return [
        Debit(account="cash", amount=Decimal(10)),
        Credit(account="equity", amount=Decimal(8)),
        Credit(account="retained_earnings", amount=Decimal(2)),
    ]


def test_multiple(reference_entry):
    m = Multiple(
        debits=[("cash", Decimal(10))],
        credits=[("equity", Decimal(8)), ("retained_earnings", Decimal(2))],
    )
    assert list(m) == reference_entry


def test_entry(reference_entry):
    e = (
        Entry("Some entry")
        .debit("cash", 10)
        .credit("equity", 8)
        .credit("retained_earnings", 2)
    )
    assert list(e) == reference_entry


@pytest.mark.entry
def test_how_it_fails():
    with pytest.raises(AbacusError):
        Multiple.from_list([Debit("cash", 10)])


@pytest.mark.entry
def test_opening_fails(toy_dict):
    with pytest.raises(AbacusError):
        toy_dict.initial_entry(dict(cash=10, equity=8))


@pytest.mark.entry
def test_opening_entry(toy_dict):
    opening_dict = dict(cash=10, equity=8, re=2)
    entry = toy_dict.initial_entry(opening_dict)
    assert list(entry) == [Debit("cash", 10), Credit("equity", 8), Credit("re", 2)]
