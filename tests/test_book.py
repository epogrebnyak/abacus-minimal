import pytest

from abacus import Book, Chart, Entry
from abacus.book import BalancesDict
from abacus.core import Amount, BalanceSheet, IncomeStatement


def test_balances_dict_loads():
    d = BalancesDict(cash=Amount(50))
    j = d.model_dump_json()
    d2 = BalancesDict.model_validate_json(j)
    assert d2["cash"] == Amount(50)


def test_balances_dict_load(tmp_path):
    d = BalancesDict(cash=Amount(50))
    path = tmp_path / "d.json"
    d.save(path)
    d3 = BalancesDict.load(path)
    assert d3["cash"] == Amount(50)


def test_balances_dict_serialisation(toy_ledger):
    content = BalancesDict(toy_ledger.balances).model_dump_json()
    assert BalancesDict.model_validate_json(content) == dict(cash=10, equity=10, re=0)


def test_balances_load_save(tmp_path):
    path = str(tmp_path / "b.json")
    b = BalancesDict(a=1)
    b.save(path)
    assert b == BalancesDict.load(path)


@pytest.fixture
def this_chart():
    chart = Chart(
        retained_earnings="retained_earnings",
        current_earnings="current_earnings",
        assets=["cash"],
        capital=["equity"],
        income=["sales"],
        expenses=["salaries"],
    )
    chart.offset("sales", "refunds")
    return chart


def test_book_may_open_with_retained_earnings(this_chart):
    opening_balances = {"cash": 10_000, "equity": 8_000, "retained_earnings": 2_000}
    book = Book(this_chart)
    book.open(opening_balances)
    assert book.balances == {
        "cash": 10_000,
        "equity": 8_000,
        "sales": 0,
        "salaries": 0,
        "retained_earnings": 2_000,
        "current_earnings": 0,
        "refunds": 0,
    }


@pytest.fixture
def book_before_close(this_chart):
    entries = [
        Entry("Initial investment").amount(300).debit("cash").credit("equity"),
        Entry("Sold services with VAT").amount(125).debit("cash").credit("sales"),
        Entry("Made refund").amount(25).debit("refunds").credit("cash"),
        Entry("Paid salaries").amount(50).debit("salaries").credit("cash"),
    ]
    book = Book(this_chart)
    book.post_many(entries)
    return book


@pytest.fixture
def book_after_close(book_before_close):
    book_before_close.close()
    return book_before_close


def test_book_now_closed(book_after_close):
    assert book_after_close.is_closed() is True


def test_income_statement_before_close(book_before_close):
    assert book_before_close.income_statement == IncomeStatement(
        income={"sales": 100}, expenses={"salaries": 50}
    )


def test_income_statement_after_close(book_after_close):
    assert book_after_close.income_statement == IncomeStatement(
        income={"sales": 100}, expenses={"salaries": 50}
    )


def test_balance_sheet_before_close(book_before_close):
    assert book_before_close.balance_sheet == BalanceSheet(
        assets={"cash": 350},
        capital={"equity": 300, "current_earnings": 50, "retained_earnings": 0},
        liabilities={},
    )


def test_balance_sheet_after_close(book_after_close):
    assert book_after_close.balance_sheet == BalanceSheet(
        assets={"cash": 350},
        capital={"equity": 300, "retained_earnings": 50},
        liabilities={},
    )


def test_balances_before_close(book_before_close):
    assert book_before_close.ledger.balances == {
        "cash": 350,
        "equity": 300,
        "sales": 125,
        "salaries": 50,
        "retained_earnings": 0,
        "current_earnings": 0,
        "refunds": 25,
    }


def test_balances_after_close(book_after_close):
    assert book_after_close.ledger.balances == {
        "cash": 350,
        "equity": 300,
        "retained_earnings": 50,
    }


def test_book_similar_to_readme(tmp_path):
    chart = Chart(
        retained_earnings="retained_earnings",
        current_earnings="current_earnings",
        assets=["cash"],
        capital=["equity"],
        income=["sales"],
        expenses=["salaries"],
    )
    chart.offset("sales", "refunds")
    book = Book(chart)
    book.post(Entry("Initial investment").debit("cash", 10000).credit("equity", 10000))
    book.chart.save(tmp_path / "chart.json")
    book.balances.save(tmp_path / "balances.json")
    print(book.balances)
    del book
    book = Book.load(tmp_path)
    entries = [
        Entry("Sold services with VAT").amount(6500).debit("cash").credit("sales"),
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
    book.save(tmp_path, allow_overwrite=True)
