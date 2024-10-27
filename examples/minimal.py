# In minimal example we start a company with initial shareholder investment (1000),
# pay rent (100) and salaries (350), and accept cash for provided services (400).
# After end of reporting period we get balance sheet and income statement.

from abacus import Book, Chart, Entry

chart = Chart(
    retained_earnings="retained_earnings",
    current_earnings="current_earnings",
    assets=["cash"],
    capital=["equity"],
    income=["services"],
    expenses=["salaries", "rent"],
)
book = Book(chart)
entries = [
    Entry("Initial shareholder investment").debit("cash", 1000).credit("equity", 1000),
    Entry("Paid office rent").debit("rent", 100).credit("cash", 100),
    Entry("Accept cash for services").debit("cash", 400).credit("services", 400),
    Entry("Paid salaries in cash").debit("salaries", 350).credit("cash", 350),
]
book.post_many(entries)
book.close()
print(book.income_statement)
print(book.balance_sheet)
# Some checks
assert book.income_statement.net_earnings == -50
assert book.balances == {"cash": 950, "equity": 1000, "retained_earnings": -50}
