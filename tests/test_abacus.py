import pytest  # type: ignore

from abacus.core import (
    T5,
    AbacusError,
    ChartDict,
    Contra,
    CreditAccount,
    DebitAccount,
    Ledger,
    MultipleEntry,
    Regular,
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


@pytest.mark.report
def test_net_earnings(toy_dict):
    chart_dict = toy_dict.set(T5.Income, "sales")
    ledger = chart_dict.to_ledger()
    sales_entry = MultipleEntry().debit("cash", 10).credit("sales", 10)
    ledger.post(sales_entry)
    assert ledger.income_statement(chart_dict).net_earnings == 10


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
    entry = MultipleEntry.opening(opening_dict, toy_dict)
    assert entry == MultipleEntry().debit("cash", 10).credit("equity", 8).credit(
        "re", 2
    )


@pytest.mark.entry
def test_opening_entry_fails(toy_dict):
    with pytest.raises(AbacusError):
        MultipleEntry.opening(dict(cash=10, equity=8), toy_dict)


@pytest.mark.ledger
def test_ledger_open(toy_dict):
    ledger = Ledger.open(chart_dict=toy_dict, opening_balances=dict(cash=10, equity=10))
    assert ledger.trial_balance == dict(
        re=(None, 0), cash=(10, None), equity=(None, 10)
    )
