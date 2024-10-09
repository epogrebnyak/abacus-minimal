import pytest  # type: ignore

from abacus import (
    T5,
    Chart,
    ChartDict,
    Contra,
    CreditAccount,
    DebitAccount,
    Entry,
    Regular,
    double_entry,
    AbacusError,
)


def test_chart_dict_for_regular():
    cd = ChartDict([("sales", Regular(T5.Income)), ("cashback", Contra("sales"))])
    assert cd.get_constructor("sales") == CreditAccount


def test_chart_dict_for_contra():
    cd = ChartDict([("sales", Regular(T5.Income)), ("cashback", Contra("sales"))])
    assert cd.get_constructor("cashback") == DebitAccount


def test_chart_offset():
    chart = Chart(retained_earnings="re", income=["sales"]).offset("sales", "refunds")
    assert chart.contra_accounts["sales"] == ["refunds"]


def make_chart():
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


def test_chart_closing_pairs():
    chart = make_chart()
    assert chart.closing_pairs == [
        # income
        ("refunds", "sales"),
        ("voids", "sales"),
        ("sales", "re"),
        # expenses
        ("wages", "re"),
    ]


def test_chart_to_ledger_keys():
    ledger = Chart(
        retained_earnings="re", assets=["cash"], capital=["equity"]
    ).to_ledger()
    assert list(ledger.keys()) == ["cash", "equity", "re"]


# parametrise
@pytest.mark.parametrize(
    "account_name, cls",
    [
        ("cash", DebitAccount),
        ("equity", CreditAccount),
        ("re", CreditAccount),
        ("ts", DebitAccount),
    ],
)
def test_ledger_creation(account_name, cls):
    ledger = (
        Chart(retained_earnings="re", assets=["cash"], capital=["equity"])
        .offset("equity", "ts")
        .to_ledger()
    )
    assert isinstance(ledger[account_name], cls)

@pytest.mark.parametrize(
    "chart",
    [
        Chart(retained_earnings="re", income=["sales"]).offset("sales", "refunds").offset("sales", "refunds"),
        Chart(retained_earnings="re", assets=["cash", "cash"]),
        Chart(retained_earnings="re", assets=["other"], capital=["other"]),
        Chart(retained_earnings="_", assets=["_"]),

    ],
)
def test_chart_assert_unique_on_repeated_account_name(chart):
    with pytest.raises(AbacusError):
        chart.assert_unique()
        print(chart)


chart = make_chart()
print(chart.to_dict())
print(chart.closing_pairs)
ledger = chart.to_ledger()
print(ledger)
ledger.post_many(
    [
        double_entry("cash", "equity", 20),
        Entry().dr("cash", 120).cr("sales", 100).cr("vat", 20),
        double_entry("refunds", "cash", 5),
        double_entry("wages", "cash", 10),
        double_entry("vat", "cash", 20),
    ]
)
ledger.close(chart)
print(ledger.trial_balance)
print(ledger.balances)
