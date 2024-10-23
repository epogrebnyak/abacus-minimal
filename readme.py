from abacus import Book

book = Book()
book.chart.assets.append("cash")
book.chart.capital.append("equity")
book.chart.income.append("sales")
book.chart.offset("sales", "refunds")
book.chart.expenses.append("salaries")
book.post_double("Initial investment", debit="cash", credit="equity", amount=10000)
book.save(".")
del book
book = Book()
book.load(".")
book.post_double("Sold services", debit="cash", credit="sales", amount=2200)
book.post_double("Made refund", debit="refunds", credit="cash", amount=200)
book.post_double("Paid salaries", debit="salaries", credit="cash", amount=1000)
book.close()
assert book.ledger.balances == {
    "cash": 11000,
    "equity": 10000,
    "retained_earnings": 1000,
}
