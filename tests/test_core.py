import pytest  # type: ignore

from abacus import (  # ,; BalanceSheet,; ChartDict,; CreditAccount,; DebitAccount,; Regular,; ReportDict,; is_balanced,
    AbacusError,
    Credit,
    Debit,
    Double,
    Income,
    Ledger,
    Multiple,
)
from abacus.ledger import BalanceSheet, CreditAccount, DebitAccount, ReportDict


@pytest.mark.ledger
def test_ledger_keys(toy_ledger):
    assert list(toy_ledger.accounts) == ["cash", "equity", "re"]


@pytest.mark.ledger
def test_ledger_open(toy_ledger):
    entry = toy_ledger.chart.initial_entry(dict(cash=2, equity=2))
    toy_ledger.apply(entry)
    assert toy_ledger.balances == dict(re=0, cash=12, equity=12)


def test_balance_sheet_is_not_balanced():
    assert (
        BalanceSheet(
            assets=ReportDict({"cash": 350}),
            equity=ReportDict({"equity": 300, "retained_earnings": 50}),
            liabilities=ReportDict({"extra": 1}),
        ).is_balanced()
        is False
    )


@pytest.mark.report
def test_net_earnings(toy_ledger):
    toy_ledger.apply_many([Income("sales"), Double("cash", "sales", 10)])
    assert toy_ledger.income_statement().net_earnings == 10


@pytest.mark.report
def test_balance_sheet(toy_dict):
    ledger = Ledger()
    ledger.chart = toy_dict
    ledger.accounts = {
        "cash": DebitAccount(10),
        "equity": CreditAccount(10),
        "re": CreditAccount(0),
    }

    assert ledger.balance_sheet("cu") == BalanceSheet(
        assets=dict(cash=10), equity=dict(equity=10, re=0, cu=0), liabilities=dict()
    )


@pytest.mark.report
def test_balance_sheet_again(toy_ledger):
    assert toy_ledger.close("re").balance_sheet() == BalanceSheet(
        assets=dict(cash=10), equity=dict(equity=10, re=0), liabilities=dict()
    )


@pytest.mark.report
def test_balances(toy_ledger):
    assert toy_ledger.balances == dict(cash=10, equity=10, re=0)


@pytest.mark.entry
def test_opening_fails(toy_dict):
    with pytest.raises(AbacusError):
        toy_dict.initial_entry(dict(cash=10, equity=8))


@pytest.mark.entry
def test_opening_entry(toy_dict):
    opening_dict = dict(cash=10, equity=8, re=2)
    entry = toy_dict.initial_entry(opening_dict)
    assert list(entry) == [Debit("cash", 10), Credit("equity", 8), Credit("re", 2)]


@pytest.mark.entry
def test_how_it_fails():
    with pytest.raises(AbacusError):
        Multiple.from_list([Debit("cash", 10)])
