import pytest
from pydantic import ValidationError

from abacus import AbacusError, Asset, Chart, Equity


def test_post_init():
    with pytest.raises(AbacusError):
        Chart(
            retained_earnings="retained_earnings",
            current_earnings="current_earnings",
            assets=["cash", "cash"],
        )


def test_all_contra_accounts_point_to_existing_accounts():
    with pytest.raises(AbacusError):
        Chart(
            retained_earnings="re",
            current_earnings="profit",
            contra_accounts={"reference_does_not_exist": ["depreciation"]},
        )


def test_duplicates():
    chart = Chart(retained_earnings="re", current_earnings="profit")
    chart.equity = ["re"]
    assert chart.duplicates == ["re"]


def test_pydantic_will_not_accept_extra_fields():
    with pytest.raises(ValidationError):
        Chart(retained_earnings="re", current_earnings="profit", haha=["equity"])


def test_chart_offset():
    chart = Chart(
        retained_earnings="re", current_earnings="profit", income=["sales"]
    ).offset("sales", "refunds")
    assert chart.contra_accounts["sales"] == ["refunds"]


def test_chart_assert_unique_on_repeated_account_name():
    for failed_chart in [
        Chart(retained_earnings="re", current_earnings="profit", income=["sales"])
        .offset("sales", "refunds")
        .offset("sales", "refunds"),
        Chart(retained_earnings="re", current_earnings="profit", assets=["cash"]).add(
            "asset:cash"
        ),
        Chart(retained_earnings="re", current_earnings="profit", assets=["other"]).add(
            "L:other"
        ),
        Chart(retained_earnings="_", current_earnings="profit").add("expense:_"),
    ]:
        with pytest.raises(AbacusError):
            d = dict(**failed_chart.model_dump())
            print(d)
            Chart(**d)


def test_chart_to_list():
    assert list(
        Chart(
            retained_earnings="re",
            current_earnings="profit",
            assets=["cash"],
            equity=["equity"],
            contra_accounts={"equity": ["ts"]},
        )
    ) == [
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
