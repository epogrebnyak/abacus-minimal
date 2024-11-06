import pytest  # type: ignore

from abacus.core import (
    T5,
    AbacusError,
    Amount,
    BalanceSheet,
    ChartDict,
    Contra,
    Credit,
    CreditAccount,
    Debit,
    DebitAccount,
    Ledger,
    Regular,
    ReportDict,
    is_balanced,
)


@pytest.mark.chart_dict
def test_chart_dict_for_regular():
    cd = ChartDict([("sales", Regular(T5.Income)), ("cashback", Contra("sales"))])
    assert isinstance(cd.t_account_class("sales").empty(), CreditAccount)


@pytest.mark.chart_dict
def test_chart_dict_for_contra():
    cd = ChartDict([("sales", Regular(T5.Income)), ("cashback", Contra("sales"))])
    assert isinstance(cd.t_account_class("cashback").empty(), DebitAccount)


@pytest.mark.chart_dict
def test_chart_dict_key_error_on_empty():
    with pytest.raises(KeyError):
        ChartDict().t_account_class("vat")


@pytest.mark.ledger
def test_ledger_keys(toy_ledger):
    assert list(toy_ledger.keys()) == ["cash", "equity", "re"]


@pytest.mark.ledger
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


@pytest.mark.ledger
def test_ledger_open(toy_dict):
    ledger = toy_dict.to_ledger()
    entry = toy_dict.opening_entry(dict(cash=10, equity=10))
    ledger.post(entry)
    assert ledger.trial_balance == dict(
        re=(None, 0), cash=(10, None), equity=(None, 10)
    )


def test_balance_sheet_is_not_balanced():
    assert (
        BalanceSheet(
            assets=ReportDict({"cash": 350}),
            capital=ReportDict({"equity": 300, "retained_earnings": 50}),
            liabilities=ReportDict({"extra": 1}),
        ).is_balanced()
        is False
    )


@pytest.mark.report
def test_net_earnings(toy_dict):
    chart_dict = toy_dict.set(T5.Income, "sales")
    ledger = chart_dict.to_ledger()
    sales_entry = [Debit("cash", 10), Credit("sales", 10)]
    ledger.post(sales_entry)
    assert ledger.income_statement(chart_dict).net_earnings == 10


@pytest.mark.report
def test_balance_sheet(toy_dict):
    ledger = Ledger(
        {
            "cash": DebitAccount(left=Amount("10"), right=Amount("0")),
            "equity": CreditAccount(left=Amount("0"), right=Amount("10")),
            "re": CreditAccount(left=Amount("0"), right=Amount("0")),
        }
    )
    assert ledger.balance_sheet(toy_dict) == BalanceSheet(
        assets=dict(cash=10), capital=dict(equity=10, re=0), liabilities=dict()
    )


@pytest.mark.report
def test_balance_sheet_again(toy_ledger, toy_dict):
    assert toy_ledger.balance_sheet(toy_dict) == BalanceSheet(
        assets=dict(cash=10), capital=dict(equity=10, re=0), liabilities=dict()
    )


@pytest.mark.report
def test_trial_balance(toy_ledger):
    assert toy_ledger.trial_balance == dict(
        cash=(10, None), equity=(None, 10), re=(None, 0)
    )


@pytest.mark.report
def test_balances(toy_ledger):
    assert toy_ledger.balances == dict(cash=10, equity=10, re=0)


@pytest.mark.entry
def test_opening_fails(toy_dict):
    with pytest.raises(AbacusError):
        toy_dict.opening_entry(dict(cash=10, equity=8))


@pytest.mark.entry
def test_opening_entry(toy_dict):
    opening_dict = dict(cash=10, equity=8, re=2)
    entry = toy_dict.opening_entry(opening_dict)
    assert entry == [Debit("cash", 10), Credit("equity", 8), Credit("re", 2)]


def test_assert_is_balanced():
    entry = [Debit("cash", 10), Credit("equity", 9)]
    assert not is_balanced(entry)


@pytest.mark.entry
def test_how_it_fails(toy_dict):
    ledger = toy_dict.to_ledger()
    entry = [Debit("cash", 10), Credit("equity", 12), Debit("ts", 2)]
    with pytest.raises(AbacusError):
        ledger.post(entry)
    assert ledger["cash"].balance == 0
    assert ledger["equity"].balance == 0
