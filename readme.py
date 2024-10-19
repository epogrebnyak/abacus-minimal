from abacus import Book, Chart, Entry

# Create chart of accounts
chart = Chart(
    retained_earnings="retained_earnings",
    assets=["cash"],
    capital=["equity"],
    liabilities=["vat"],
    income=["sales"],
    expenses=["salaries"],
)
chart.offset("sales", "refunds")

# Post entries
book = Book(chart)
# fmt: off
entries = [
    Entry("Initial investment").amount(10_000).debit("cash").credit("equity"),
    Entry("Sold services with VAT").debit("cash", 6000).credit("sales", 5000).credit("vat", 1000),
    Entry("Made client refund").double(debit="refunds", credit="cash", amount=500),
    Entry("Paid salaries").debit("salaries", 1500).credit("cash", 1500),
]
# fmt: on
book.post_many(entries)

# Close at period end
print(book.income_statement.model_dump_json())
book.close()
print(book.balance_sheet.model_dump_json())
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 10000,
    "vat": 1000,
    "retained_earnings": 3000,
}

# Save to JSON files in current folder
book.save(directory=".")
