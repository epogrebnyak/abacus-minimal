# Start a company with initial shareholder investment,
# acquire and then sell stock of merchandise and pay staff salaries.

from abacus import Book, Chart, Entry

chart = Chart(
    retained_earnings="retained_earnings",
    current_earnings="current_earnings",
    assets=["cash", "inventory"],
    capital=["equity"],
    income=["sales"],
    expenses=["salaries", "cogs"],
)
book = Book(chart)
entries = [
    Entry("Initial shareholder funds").debit("cash", 500).credit("equity", 500),
    Entry("Acquired goods").debit("inventory", 200).credit("cash", 200),
    Entry("Accepted payment for goods").debit("cash", 400).credit("ar", 400),
    Entry("Shipped goods").debit("cogs", 200).credit("inventory", 200),
    Entry("Paid salaries").debit("salaries", 100).credit("cash", 100),
]
book.post_many(entries)
book.close()
print(book.income_statement)
print(book.balance_sheet)
# Some checks
assert book.income_statement.net_earnings == 100
assert book.balances == {"cash": 600, "equity": 500, "retained_earnings": 100}
