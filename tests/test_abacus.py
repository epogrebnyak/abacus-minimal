import pytest  # type: ignore
from pydantic import ValidationError

from abacus import Book, Chart, Entry  # IncomeStatement
from abacus.book import BalancesDict
from abacus.core import (
    T5,
    AbacusError,
    BalanceSheet,
    ChartDict,
    Contra,
    CreditAccount,
    CreditEntry,
    DebitAccount,
    DebitEntry,
    Regular,
    Ledger
)


@pytest.mark.chart_dict
def test_chart_dict_for_regular():
    cd = ChartDict([("sales", Regular(T5.Income)), ("cashback", Contra("sales"))])
    assert isinstance(cd.t_account("sales"), CreditAccount)


@pytest.mark.chart_dict
def test_chart_dict_for_contra():
    cd = ChartDict([("sales", Regular(T5.Income)), ("cashback", Contra("sales"))])
    assert isinstance(cd.t_account("cashback"), DebitAccount)


@pytest.mark.chart_dict
def test_chart_dict_key_error():
    with pytest.raises(KeyError):
        ChartDict().t_account("vat")


@pytest.mark.chart
def test_chart_offset():
    chart = Chart(retained_earnings="re", income=["sales"]).offset("sales", "refunds")
    assert chart.contra_accounts["sales"] == ["refunds"]


@pytest.fixture
def chart():
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


@pytest.mark.chart
def test_chart_closing_pairs(chart):
    assert chart.closing_pairs == [
        # income
        ("refunds", "sales"),
        ("voids", "sales"),
        ("sales", "re"),
        # expenses
        ("wages", "re"),
    ]


@pytest.mark.chart
def test_chart_to_ledger_keys():
    chart_dict = Chart(retained_earnings="re", assets=["cash"], capital=["equity"]).to_dict()
    ledger = chart_dict.to_ledger()
    assert list(ledger.keys()) == ["cash", "equity", "re"]


@pytest.mark.chart
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
        .to_dict()
        .to_ledger()
    )
    assert isinstance(ledger[account_name], cls)


@pytest.mark.chart
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
    with pytest.raises(AbacusError) as e:
        chart.assert_unique()
        print(chart, e)


@pytest.mark.chart
def test_pydantic_will_not_accept_extra_fields():
    with pytest.raises(ValidationError):
        Chart(retained_earnings="re", haha=["equity"])


@pytest.mark.chart
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


@pytest.mark.chart
def test_end_to_end(chart):
    chart_dict = chart.to_dict()
    ledger = chart_dict.to_ledger()
    entries = [
        Entry("Start").debit("cash", 20).credit("equity", 20),
        Entry("Accepted payment")
        .debit("cash", 120)
        .credit("sales", 100)
        .credit("vat", 20),
        [DebitEntry("refunds", 5), CreditEntry("cash", 5)],
        Entry("Paid salaries").double(debit="wages", credit="cash", amount=10),
        Entry("Paid VAT due").double(debit="vat", credit="cash", amount=20),
    ]
    for entry in entries:
        ledger.post(entry)
    closing_pairs = chart_dict.closing_pairs(chart.retained_earnings)
    ledger.close(closing_pairs)
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


@pytest.mark.chart
def test_catch_negative_entry():
    ledger = Chart(retained_earnings="re", assets=["cash"], capital=["equity"]).to_dict().to_ledger()
    with pytest.raises(AbacusError):
        ledger.post(Entry("Invalid entry").credit("cash", 1))


@pytest.mark.entry
def test_entry_double():
    entry = Entry("Double entry").double(debit="cash", credit="equity", amount=10)
    assert entry.debits == [("cash", 10)]
    assert entry.credits == [("equity", 10)]


@pytest.mark.entry
def test_entry_double_cannot_recyle():
    entry = Entry("Double entry").double(debit="cash", credit="equity", amount=9)
    with pytest.raises(AbacusError):
        entry.double(debit="cash", credit="equity", amount=1)


@pytest.mark.entry
def test_entry_amount():
    entry = Entry("Entry with amount").amount(10).debit("cash").credit("equity")
    assert entry.debits == [("cash", 10)]
    assert entry.credits == [("equity", 10)]


@pytest.mark.entry
def test_entry_no_amount_raises_error():
    with pytest.raises(AbacusError):
        Entry("Entry with no amount").debit("cash").credit("equity")


@pytest.mark.entry
def test_entry_chained():
    entry = (
        Entry("Chained entry")
        .debit("cash", 6)
        .debit("ar", 6)
        .credit("sales", 10)
        .credit("vat", 2)
    )
    assert entry.debits == [("cash", 6), ("ar", 6)]
    assert entry.credits == [("sales", 10), ("vat", 2)]


@pytest.mark.report
def test_balance_sheet():
    chart = Chart(
        retained_earnings="re",
        assets=["cash"],
        capital=["equity"],
        income=["sales"],
        contra_accounts=dict(equity=["ts"], sales=["refunds"]),
    )
    chart_dict = chart.to_dict()
    ledger = chart_dict.to_ledger()
    ledger.post(Entry("Launch").debit("cash", 10).credit("equity", 10))
    ledger.post(Entry("Sold services").double(debit="cash", credit="sales", amount=50))
    ledger.post(Entry("Issued refund").debit("refunds", 40).credit("cash", 40))
    ledger.post(Entry("Made buyback").double(debit="ts", credit="cash", amount=8))
    assert ledger.income_statement(chart_dict).net_earnings == 10
    closing_pairs = chart.closing_pairs
    ledger.close(closing_pairs)
    assert ledger.balance_sheet(chart_dict) == BalanceSheet(
        assets={"cash": 12}, capital={"equity": 2, "re": 10}, liabilities={}
    )


@pytest.mark.report
def test_net_earnings():
    chart = Chart(retained_earnings="re", assets=["cash"], income=["sales"])
    chart_dict = chart.to_dict()
    ledger = chart_dict.to_ledger()
    ledger.post(Entry("Free cheese, no expenses").debit("cash", 10).credit("sales", 10))
    assert ledger.income_statement(chart_dict).net_earnings == 10


@pytest.mark.entry
def test_closing_entry_for_debit_account():
    account = DebitAccount(20, 5)
    assert account.closing_entry("this", "that", "close") == Entry(
        "close", is_closing=True
    ).debit("that", 15).credit("this", 15)


@pytest.fixture
def toy_chart():
    return Chart(retained_earnings="re", assets=["cash"], capital=["equity"])


@pytest.fixture
def toy_ledger(toy_chart):
    ledger = toy_chart.to_dict().to_ledger()
    ledger.post(Entry("Start company").amount(10).debit("cash").credit("equity"))
    return ledger


@pytest.mark.report
def test_trial_balance(toy_ledger):
    assert toy_ledger.trial_balance == dict(
        cash=(10, None), equity=(None, 10), re=(None, 0)
    )


@pytest.mark.report
def test_balances(toy_ledger):
    assert toy_ledger.balances == dict(cash=10, equity=10, re=0)


@pytest.mark.report
def test_balances_dict_json(toy_ledger):
    content = BalancesDict(toy_ledger.balances).model_dump_json()
    x = BalancesDict.model_validate_json(content)
    assert x.root == dict(cash=10, equity=10, re=0)


@pytest.mark.report
def test_balances_load_save(tmp_path):
    path = str(tmp_path / "b.json")
    b = BalancesDict(a=1)
    b.save(path)
    assert b == BalancesDict.load(path)


@pytest.mark.entry
def test_opening_entry_works(toy_chart):
    entry = Entry("Will open").opening(
        dict(cash=10, equity=8, re=2), toy_chart.to_dict()
    )
    assert entry == Entry("Will open").debit("cash", 10).credit("equity", 8).credit(
        "re", 2
    )


@pytest.mark.entry
def test_opening_entry_fails(toy_chart):
    with pytest.raises(AbacusError):
        Entry("Doomed").opening(dict(cash=10, equity=8), toy_chart.to_dict())


@pytest.mark.mixed
def test_chart_open(toy_chart):
    ledger = Ledger.open(toy_chart.to_dict(), dict(cash=10, equity=10))
    assert ledger.trial_balance == dict(
        re=(None, 0), cash=(10, None), equity=(None, 10)
    )


@pytest.mark.chart
def test_is_debit_account():
    chart_dict = Chart(retained_earnings="re").to_dict().offset("re", "drawing")
    assert chart_dict.is_debit_account("drawing") is True


@pytest.mark.mixed
def test_book(tmp_path):
    chart = Chart(
        retained_earnings="retained_earnings",
        assets=["cash"],
        capital=["equity"],
        income=["sales"],
        expenses=["salaries"],
    )
    chart.offset("sales", "refunds")
    book = Book(chart)
    book.post(Entry("Initial investment").debit("cash", 10000).credit("equity", 10000))
    book.save(tmp_path)
    del book
    book = Book.load(tmp_path)
    entries = [
        Entry("Sold services with VAT").double(
            amount=6500, debit="cash", credit="sales"
        ),
        Entry("Made refund").double(amount=500, debit="refunds", credit="cash"),
        Entry("Paid salaries").double(amount=1000, debit="salaries", credit="cash"),
    ]
    book.post_many(entries)
    book.close()
    assert book.ledger.balances == {
        "cash": 15000,
        "equity": 10000,
        "retained_earnings": 5000,
    }
