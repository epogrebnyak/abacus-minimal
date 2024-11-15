"""This file is automatically generated from README.md"""

from abacus import Book, Chart, Entry, History

chart = Chart(
    retained_earnings="retained_earnings",
    current_earnings="current_earnings",
    assets=["cash", "inventory"],
    equity=["equity"],
    income=["sales"],
    expenses=["salaries", "cogs"],
)
book = Book.from_chart(chart)
entries = [
    Entry("Initial shareholder funds").debit("cash", 500).credit("equity", 500),
    Entry("Acquired goods").debit("inventory", 200).credit("cash", 200),
    Entry("Accepted payment for goods").debit("cash", 400).credit("sales", 400),
    Entry("Shipped goods").debit("cogs", 200).credit("inventory", 200),
    Entry("Paid salaries").debit("salaries", 150).credit("cash", 150),
]
book.post_many(entries)
book.close()
print(book.income_statement)
print(book.balance_sheet)
# Some checks
assert book.balance_sheet.assets.total == 550
assert book.income_statement.net_earnings == 50
assert book.ledger.balances == {
    "cash": 550,
    "inventory": 0,
    "equity": 500,
    "retained_earnings": 50,
}


chart = Chart(
    retained_earnings="retained_earnings",
    current_earnings="current_earnings",
    assets=["cash", "ar"],
    equity=["equity"],
    liabilities=["vat_payable"],
    income=["sales"],
    expenses=["salaries"],
)
chart.offset("sales", "refunds")
chart.name("ar", "Accounts receivable")

chart.save("chart.json", allow_overwrite=True)
chart = Chart.load("chart.json")


book = Book.from_chart(chart)
opening_balances = {"cash": 10_000, "equity": 8_000, "retained_earnings": 2_000}
book.open(opening_balances)


entries = [
    Entry("Invoice with VAT")
    .debit("ar", 6000)
    .credit("sales", 5000)
    .credit("vat_payable", 1000),
    Entry("Cash payment").debit("cash", 6000).credit("ar", 6000),
    Entry("Cashback").double(debit="refunds", credit="cash", amount=500),
    Entry("Paid salaries").amount(1500).debit("salaries").credit("cash"),
]

# Post entries to book
book.post_many(entries)

# Show account balances
print(book.balances)

# Check account balances match expected values
assert book.balances == {
    "cash": 14000,
    "ar": 0,
    "equity": 8000,
    "vat_payable": 1000,
    "sales": 5000,
    "refunds": 500,
    "salaries": 1500,
    "retained_earnings": 2000,
}

print("=== Before closing ===")
print(book.income_statement)
print(book.balance_sheet)
print(book.ledger.chart)
assert book.balance_sheet.equity["current_earnings"] == 3000

# Close accounts at period end
book.close()

print("=== After closing ===")
print(book.income_statement)
print(book.balance_sheet)

# Check account balances match expected values
print(book.balances)
assert book.balances == {
    "cash": 14000,
    "ar": 0,
    "equity": 8000,
    "vat_payable": 1000,
    "retained_earnings": 5000,
}

# Save JSON file
book.save_history("./history.json", allow_overwrite=True)


# Load history from JSON file
new_history = History.load("./history.json")
for a, b in zip(new_history, book.ledger.history):
    assert a == b