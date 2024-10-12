from abacus import Book, Entry

book = Book.new()
book.chart.assets.append("cash")
book.chart.capital.append("equity")
book.chart.liabilities.append("vat")
book.chart.name("vat", "VAT payable")
book.chart.income.append("sales")
book.chart.offset("sales", "refunds")
book.chart.expenses.append("salaries")
book.post_double("Initial investment", debit="cash", credit="equity", amount=10000)
book.post(
    Entry("Sold services with VAT")
    .debit("cash", 6000)
    .credit("sales", 5000)
    .credit("vat", 1000)
)
book.post_double("Made client refund", debit="refunds", credit="cash", amount=500)
book.post_double("Paid salaries", debit="salaries", credit="cash", amount=1500)
book.close()
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 10000,
    "vat": 1000,
    "retained_earnings": 3000,
}
