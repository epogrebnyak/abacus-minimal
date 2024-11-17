"""This file is automatically generated from README.md"""

from pprint import pprint

from abacus import (
    Asset,
    Book,
    Chart,
    Close,
    Double,
    Entry,
    Equity,
    Expense,
    Income,
    Ledger,
)

events = [
    # Create accounts
    Asset("cash"),
    Equity("equity"),
    Equity("re", title="Retained earnings"),
    Income("sales"),
    Expense("salaries"),
    # Post entries
    Double("cash", "equity", 1000),
    Double("cash", "sales", 250),
    Double("salaries", "cash", 150),
    # Close period
    Close(earnings_account="re"),
]
ledger = Ledger.from_list(events)

print(ledger.balances)
print(ledger.income_statement())
print(ledger.balance_sheet())

for p in ledger.history.primitives:
    print(p)


# Create chart and ledger
chart = Chart(
    assets=["cash", "ar"],
    equity=["equity"],
    liabilities=["tax_due"],
    income=["services"],
    expenses=["salaries"],
    contra_accounts={"services": ["refunds"]},
    retained_earnings="retained_earnings",
    current_earnings="current_earnings",
)
book = Book.from_chart(chart)

# Post entries
entries = [
    Entry("Shareholder investment").double("cash", "equity", 1000),
    Entry("Invoiced services")
    .debit("ar", 1200)
    .credit("services", 1000)
    .credit("tax_due", 200),
    Entry("Accepted payment").double("cash", "ar", 600),
    Entry("Made refund").double("refunds", "cash", 150),
    Entry("Paid salaries").double("salaries", "cash", 450),
]
book.post_many(entries)
print(book.balances)

# Close the period and show reports
book.close()
print(book.income_statement)
print(book.balance_sheet)

# Save
book.save("chart.json", "history.json", allow_overwrite=True)

# Load and re-enter
book2 = Book.load_unsafe("chart.json", "history.json")

pprint(book2)  # may not fully identical to `book` yet
