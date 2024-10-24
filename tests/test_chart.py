import pytest
from pydantic import ValidationError

from abacus import Chart
from abacus.core import T5, AbacusError, ChartDict, Contra, Regular


def test_all_contra_accounts_point_to_existing_accounts():
    chart = Chart(retained_earnings="re", contra_accounts={"cash": ["oh_no"]})
    with pytest.raises(AbacusError):
        chart.assert_no_invalid_contra_account_references()


@pytest.mark.parametrize(
    "chart",
    [
        Chart(retained_earnings="re", income=["sales"])
        .offset("sales", "refunds")
        .offset("sales", "refunds"),
        Chart(retained_earnings="re", assets=["cash", "cash"]),
        Chart(retained_earnings="re", assets=["other"], capital=["other"]),
        Chart(retained_earnings="_", assets=["_"]),
    ],
)
def test_chart_assert_unique_on_repeated_account_name(chart: Chart):
    with pytest.raises(AbacusError):
        chart.assert_all_account_names_are_unique()


def test_pydantic_will_not_accept_extra_fields():
    with pytest.raises(ValidationError):
        Chart(retained_earnings="re", haha=["equity"])


def test_chart_offset():
    chart = Chart(retained_earnings="re", income=["sales"]).offset("sales", "refunds")
    assert chart.contra_accounts["sales"] == ["refunds"]


def test_chart_to_dict():
    assert Chart(
        retained_earnings="re",
        assets=["cash"],
        capital=["equity"],
        contra_accounts={"equity": ["ts"]},
    ).to_dict() == ChartDict(
        {
            "cash": Regular(T5.Asset),
            "equity": Regular(T5.Capital),
            "re": Regular(T5.Capital),
            "ts": Contra("equity"),
        }
    )


def test_chart_closing_pairs(realistic_chart):
    assert realistic_chart.closing_pairs == [
        # income
        ("refunds", "sales"),
        ("voids", "sales"),
        ("sales", "re"),
        # expenses
        ("wages", "re"),
    ]


@pytest.mark.mixed
def test_chart_to_ledger_keys():
    assert Chart(
        retained_earnings="re", assets=["cash"], capital=["equity"]
    ).to_dict() == ChartDict().set(T5.Capital, "re").set(T5.Asset, "cash").set(
        T5.Capital, "equity"
    )


@pytest.mark.mixed
def test_is_debit_account():
    chart_dict = Chart(retained_earnings="re").to_dict().offset("re", "drawing")
    assert chart_dict.is_debit_account("drawing") is True
