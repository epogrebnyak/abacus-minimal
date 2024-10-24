import pytest

from abacus import Book, Chart, Entry
from abacus.book import BalancesDict


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


@pytest.fixture
def book_after_post(this_chart):
    entries = [
        Entry("Initial investment").amount(300).debit("cash").credit("equity"),
        Entry("Sold services with VAT").amount(125).debit("cash").credit("sales"),
        Entry("Made refund").amount(25).debit("refunds").credit("cash"),
        Entry("Paid salaries").amount(50).debit("salaries").credit("cash"),
    ]
    book = Book(this_chart)
    book.post_many(entries)
    return book


def test_book_after_post_not_closed(book_after_post, this_chart):
    assert book_after_post.ledger.is_closed(this_chart.to_dict()) is False


def test_book_now_closed(book_after_post):
    book_after_post.close()
    assert book_after_post.is_closed() is True


def test_book(tmp_path):
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
    book.save(tmp_path)


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
