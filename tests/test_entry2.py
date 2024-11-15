from decimal import Decimal

from pytest import fixture

from abacus import Credit, Debit, Double, Entry, Multiple


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
