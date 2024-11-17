import pytest
from pydantic import ValidationError

from abacus import AbacusError, Asset, Chart, Equity
from abacus.chart import ChartBase, Earnings, QualifiedChart


def test_chart_base_name():
    chart = ChartBase()
    chart.add_account(Asset("ar", title="AR"))
    assert chart.names["ar"] == "AR"


def test_chart_base_contra():
    chart = ChartBase()
    chart.add_account(Asset("ppe", contra_accounts=["depreciation"]))
    assert chart.contra_accounts == {"ppe": ["depreciation"]}


def test_chart_base_offset():
    chart = ChartBase(income=["sales"]).offset("sales", "refunds")
    assert chart.contra_accounts["sales"] == ["refunds"]


def test_post_init_on_dublicate():
    with pytest.raises(AbacusError):
        Chart(
            retained_earnings="retained_earnings",
            current_earnings="current_earnings",
            assets=["cash", "cash"],
        )


def test_all_contra_accounts_point_to_existing_accounts():
    with pytest.raises(AbacusError):
        Chart(
            retained_earnings="retained_earnings",
            current_earnings="current_earnings",
            contra_accounts={"reference_does_not_exist": ["depreciation"]},
        )


def test_chart_on_empty_list():
    chart = Chart(retained_earnings="re", current_earnings="profit")
    assert chart.account_names == ["re"]


def test_pydantic_will_not_accept_extra_fields():
    with pytest.raises(ValidationError):
        Chart(
            retained_earnings="re",
            current_earnings="profit",
            accounts=[],
            haha=["equity"],
        )


def test_chart_to_list():
    assert Chart(
        retained_earnings="re",
        current_earnings="profit",
        assets=["cash"],
        equity=["equity"],
        contra_accounts={"equity": ["ts"]},
    ).accounts == [
        Asset("cash"),
        Equity("equity", ["ts"]),
        Equity("re"),
    ]


def test_cannot_overwrite_chart(tmp_path):
    chart = Chart(retained_earnings="re", current_earnings="profit")
    path = tmp_path / "chart.json"
    chart.save(path)
    with pytest.raises(FileExistsError):
        chart.save(path)


def test_qualified():
    assert Chart(
        retained_earnings="re",
        current_earnings="profit",
        assets=["cash"],
        equity=["equity"],
        contra_accounts={"equity": ["ts"]},
    ).qualified == QualifiedChart(
        earnings=Earnings(current="profit", retained="re"),
        base=ChartBase(
            assets=["cash"],
            equity=["equity", "re"],
            liabilities=[],
            income=[],
            expenses=[],
            contra_accounts={"equity": ["ts"]},
            names={},
        ),
    )
