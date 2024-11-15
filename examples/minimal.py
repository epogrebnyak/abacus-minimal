from abacus import Asset, Book, Chart, Entry, Equity, Expense, Income, Liability
from abacus.chart import ChartBase

chart =  ChartBase()
chart.add_account(Asset("ar", title="AR"))
assert chart.names["ar"] == "AR"

chart.add_account(Asset("ppe", contra_accounts=["depreciation"]))
print(chart)  
print(chart.contra_accounts)    
print(list(chart))

# Create chart and ledger
accounts = [
    Asset("cash"),
    Asset("ar", title="Accounts receivable"),
    Equity("equity", contra_accounts=["treasury_shares"]),
    Equity("retained_earnings"),
    Liability("tax_due"),
    Income("services"),
    Expense("salaries"),
]
chart = Chart.from_list(accounts, retained_earnings="retained_earnings")
book = Book.from_chart(chart)

# Post entries
entries = [
    Entry("Shareholder investment").double("cash", "equity", 1000),
    Entry("Sold services with VAT")
    .debit("cash", 1200)
    .credit("services", 1000)
    .credit("tax_due", 200),
    Entry("Paid salaries").double("salaries", "cash", 500),
]
book.post_many(entries)
print(book.balances)

# Close the period
book.close()
print(book.income_statement)
print(book.balance_sheet)

# Save, load and re-enter
book.save_history("history.json", allow_overwrite=True)

# TODO: implement load
# book2 = Book.load_history("history.json")
# assert book == book2
