import pytest  # type: ignore

from abacus.core import (
    T5,
    AbacusError,
    ChartDict,
    Contra,
    Entry,
    CreditAccount,
    CreditEntry,
    DebitAccount,
    DebitEntry,
    Regular,
    Ledger,
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


def test_ledger_keys(toy_dict):
    assert list(toy_dict.to_ledger().keys()) == ["cash", "equity", "re"]


@pytest.mark.parametrize(
    "account_name, cls",
    [
        ("cash", DebitAccount),
        ("equity", CreditAccount),
        ("re", CreditAccount),
        ("ts", DebitAccount),
    ],
)
def test_ledger_creation(account_name, cls, toy_dict):
    ledger = toy_dict.offset("equity", "ts").to_ledger()
    assert isinstance(ledger[account_name], cls)


def test_catch_negative_entry(toy_dict):
    ledger = toy_dict.to_ledger()
    with pytest.raises(AbacusError):
        ledger.post(Entry("Invalid entry").credit("cash", 1))


@pytest.mark.entry
def test_entry_double():
    entry = Entry("Double entry").double(debit="cash", credit="equity", amount=10)
    assert entry.debits == [("cash", 10)]
    assert entry.credits == [("equity", 10)]


@pytest.mark.entry
def test_entry_double_cannot_recycle():
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
        Entry("Entry with no amount").debit("cash")


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
def test_net_earnings():
    chart_dict = (
        ChartDict().set(T5.Capital, "re").set(T5.Asset, "cash").set(T5.Income, "sales")
    )
    ledger = chart_dict.to_ledger()
    ledger.post(Entry("Free cheese, no expenses").debit("cash", 10).credit("sales", 10))
    assert ledger.income_statement(chart_dict).net_earnings == 10


@pytest.mark.entry
def test_closing_entry_for_debit_account():
    account = DebitAccount(20, 5)
    assert account.closing_entry("this", "that", "close") == Entry(
        "close", is_closing=True
    ).debit("that", 15).credit("this", 15)


@pytest.mark.report
def test_trial_balance(toy_ledger):
    assert toy_ledger.trial_balance == dict(
        cash=(10, None), equity=(None, 10), re=(None, 0)
    )


@pytest.mark.report
def test_balances(toy_ledger):
    assert toy_ledger.balances == dict(cash=10, equity=10, re=0)


@pytest.mark.entry
def test_opening_entry(toy_dict):
    opening_dict = dict(cash=10, equity=8, re=2)
    entry = Entry("Will open").opening(opening_dict, toy_dict)
    assert entry == Entry("Will open").debit("cash", 10).credit("equity", 8).credit(
        "re", 2
    )


@pytest.mark.entry
def test_opening_entry_fails(toy_dict):
    with pytest.raises(AbacusError):
        Entry("Doomed by -2").opening(dict(cash=10, equity=8), toy_dict)


@pytest.mark.ledger
def test_ledger_open(toy_dict):
    ledger = Ledger.open(chart_dict=toy_dict, opening_balances=dict(cash=10, equity=10))
    assert ledger.trial_balance == dict(
        re=(None, 0), cash=(10, None), equity=(None, 10)
    )
