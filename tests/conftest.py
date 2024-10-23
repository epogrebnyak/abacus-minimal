import pytest
from abacus.core import ChartDict, T5, Entry
from abacus.chart import Chart


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
    ledger.post(Entry("Start company").amount(10).debit("cash").credit("equity"))
    return ledger


@pytest.fixture
def realistic_chart():
    return Chart(
        retained_earnings="re",
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
