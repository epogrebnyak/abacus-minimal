import pytest

from abacus.chart import Chart
from abacus.core import T5, ChartDict, MultipleEntry


@pytest.fixture
def toy_dict():
    return (
        ChartDict()
        .set(T5.Asset, "cash")
        .set(T5.Capital, "equity")
        .set(T5.Capital, "re")
    )


@pytest.fixture
def toy_ledger(toy_dict):
    ledger = toy_dict.to_ledger()
    ledger.post(MultipleEntry.double("cash", "equity", 10))
    return ledger


@pytest.fixture
def realistic_chart():
    return Chart(
        retained_earnings="re",
        current_earnings="profit",
        assets=["cash", "inventory", "ar"],
        capital=["equity"],
        liabilities=["vat", "ap"],
        income=["sales"],
        expenses=["wages"],
        contra_accounts={"sales": ["refunds", "voids"], "equity": ["ts"]},
        names={
            "vat": "VAT payable",
            "ar": "Accounts receivable",
            "ap": "Accounts payable",
            "ts": "Treasury stock",
        },
    )
