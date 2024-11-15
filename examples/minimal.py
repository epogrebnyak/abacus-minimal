from abacus import Asset, Book, Chart, Entry, Equity, Expense, Income, Liability

# Create chart and ledger
accounts = [
    Asset("cash"),
    Asset("ar", title="Accounts receivable"),
    Equity("equity"),
    Equity("retained_earnings"),
    Liability("tax_due"),
    Income("services"),
    Expense("salaries"),
]
chart = Chart(retained_earnings="retained_earnings").extend(accounts)
book = Book.from_chart(chart)

# Post entries
entries = [
    Entry("Shareholder investment").double("cash", "equity", 1000),
    Entry("Service invoice")
    .debit("ar", 1200)
    .credit("services", 1000)
    .credit("tax_due", 200),
    Entry("Paid salaries").double("salaries", "cash", 600),
]
book.post_many(entries)
print(book.balances)

# Close the period and show reports
book.close()
print(book.income_statement)
print(book.balance_sheet)

# Save, load and re-enter
chart.save("chart.json", allow_overwrite=True)
book.save_history("history.json", allow_overwrite=True)

# TODO: implement load
# book2 = Book.load_history("history.json")
# assert book == book2
