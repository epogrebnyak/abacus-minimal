import pytest  # type: ignore
from pydantic import ValidationError

from abacus import Chart  # IncomeStatement
from abacus import (
    T5,
    AbacusError,
    BalancesDict,
    BalanceSheet,
    ChartDict,
    Contra,
    CreditAccount,
    CreditEntry,
    DebitAccount,
    DebitEntry,
    DoubleEntry,
    Entry,
    Regular,
    make_opening_entry,
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
    ledger = Chart(retained_earnings="re", assets=["cash"], capital=["equity"]).open()
    assert list(ledger.keys()) == ["cash", "equity", "re"]


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
        .open()
    )
    assert isinstance(ledger[account_name], cls)


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
def test_chart_assert_unique_on_repeated_account_name(chart):
    with pytest.raises(AbacusError):
        chart.assert_unique()
        print(chart)


def test_pydantic_will_not_accept_extra_fields():
    with pytest.raises(ValidationError):
        Chart(retained_earnings="re", haha=["equity"])


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


def test_end_to_end():
    chart = make_chart()
    ledger = chart.open()
    ledger.post_many(
        [
            DoubleEntry("Start", debit="cash", credit="equity", amount=20),
            Entry("Accepted payment").dr("cash", 120).cr("sales", 100).cr("vat", 20),
            [DebitEntry("refunds", 5), CreditEntry("cash", 5)],
            DoubleEntry("Paid salaries", "wages", "cash", 10),
            DoubleEntry("Paid VAT due", "vat", "cash", 20),
        ]
    )
    ledger.close(chart)
    assert ledger.balances == {
        "cash": 105,
        "inventory": 0,
        "ar": 0,
        "vat": 0,
        "ap": 0,
        "equity": 20,
        "re": 85,
        "ts": 0,
    }


def test_catch_negative_entry():
    ledger = Chart(retained_earnings="re", assets=["cash"], capital=["equity"]).open()
    with pytest.raises(AbacusError):
        ledger.post(Entry("Invalid entry").cr("cash", 1))


def test_closing_entry_for_debit_account():
    account = DebitAccount(20, 5)
    assert (
        account.closing_entry(("this", "that"), "close")
        == DoubleEntry("close", "that", "this", 15).entry
    )


def test_balance_sheet():
    chart = Chart(
        retained_earnings="re",
        assets=["cash"],
        capital=["equity"],
        income=["sales"],
        contra_accounts=dict(equity=["ts"], sales=["refunds"]),
    )
    ledger = chart.open()
    ledger.post(DoubleEntry("Launch", "cash", "equity", 10))
    ledger.post(DoubleEntry("Sold services", "cash", "sales", 50))
    ledger.post(DoubleEntry("Refunded", "refunds", "cash", 40))
    ledger.post(DoubleEntry("Buyback", "ts", "cash", 8))
    assert ledger.income_statement(chart).net_earnings == 10
    ledger.close(chart)
    assert ledger.balance_sheet(chart) == BalanceSheet(
        assets={"cash": 12}, capital={"equity": 2, "re": 10}, liabilities={}
    )


def test_net_earnings():
    chart = Chart(retained_earnings="re", assets=["cash"], income=["sales"])
    ledger = chart.open()
    ledger.post(DoubleEntry("Free lunch", "cash", "sales", 10))
    assert ledger.income_statement(chart).net_earnings == 10


@pytest.fixture
def toy_chart():
    return Chart(retained_earnings="re", assets=["cash"], capital=["equity"])


@pytest.fixture
def toy_ledger(toy_chart):
    ledger = toy_chart.open()
    ledger.post(DoubleEntry("Launch", "cash", "equity", 10))
    return ledger


def test_trial_balance(toy_ledger):
    assert toy_ledger.trial_balance == dict(cash=(10, 0), equity=(0, 10), re=(0, 0))


def test_balances(toy_ledger):
    assert toy_ledger.balances == dict(cash=10, equity=10, re=0)


def test_balances_dict_json(toy_ledger):
    content = BalancesDict(toy_ledger.balances).model_dump_json()
    x = BalancesDict.model_validate_json(content)
    assert x.root == dict(cash=10, equity=10, re=0)


def test_balances_load_save(tmp_path):
    path = str(tmp_path / "b.json")
    b = BalancesDict(a=1)
    b.save(path)
    assert b == BalancesDict.load(path)


def test_opening_entry_works(toy_chart):
    entry = make_opening_entry(
        toy_chart.to_dict(), dict(cash=10, equity=8, re=2), "open"
    )
    assert entry == Entry("open").dr("cash", 10).cr("equity", 8).cr("re", 2)


def test_opening_entry_fails(toy_chart):
    with pytest.raises(AbacusError):
        make_opening_entry(toy_chart.to_dict(), dict(cash=10))


def test_chart_open(toy_chart):
    ledger = toy_chart.open(dict(cash=10, equity=10))
    assert ledger.trial_balance == dict(re=(0, 0), cash=(10, 0), equity=(0, 10))


def test_is_debit_account():
    chart_dict = Chart(retained_earnings="re").to_dict().offset("re", "drawing")
    assert chart_dict.is_debit_account("drawing") is True
