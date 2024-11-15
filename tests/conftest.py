import pytest

from abacus import T5, Asset, Chart, Double, Equity, Ledger
from abacus.ledger import ChartDict


@pytest.fixture
def toy_dict() -> ChartDict:
    return ChartDict(cash=T5.Asset, equity=T5.Equity, re=T5.Equity)


@pytest.fixture
def toy_ledger():
    return Ledger.from_list(
        [
            Asset("cash"),
            Equity("equity"),
            Equity("re"),
            Double("cash", "equity", 10),
        ]
    )


@pytest.fixture
def realistic_chart():
    return Chart(
        retained_earnings="re",
        current_earnings="profit",
        assets=["cash", "inventory", "ar"],
        equity=["equity"],
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
