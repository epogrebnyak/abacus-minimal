from abacus import Book, Chart, Entry

chart = Chart(
    retained_earnings="retained_earnings",
    assets=["cash"],
    capital=["equity"],
    liabilities=["vat_payable"],
    income=["sales"],
    expenses=["salaries"],
)
chart.offset("sales", "refunds")

chart.save("chart.json")

chart = Chart.load("chart.json")


# Create book with opening balances
opening_balances = {"cash": 10_000, "equity": 10_000}
book = Book(chart, opening_balances)

# Post entries
entries = [
    Entry("Sold services with VAT")
    .debit("cash", 6000)
    .credit("sales", 5000)
    .credit("vat_payable", 1000),
    Entry("Made client refund").double(debit="refunds", credit="cash", amount=500),
    Entry("Paid salaries").debit("salaries", 1500).credit("cash", 1500),
]
book.post_many(entries)

# Close at period end and show reports
print(book.income_statement)
book.close()
print(book.balance_sheet)

# Check account balances match expected values
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 10000,
    "vat_payable": 1000,
    "retained_earnings": 3000,
}

# Save everything to JSON files in current folder
book.save(directory=".")
