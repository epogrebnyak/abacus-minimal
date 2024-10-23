from abacus import Book, Entry, Chart

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
