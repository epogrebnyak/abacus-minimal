# Micro example

from abacus import Asset, Double, Equity, Expense, Income, Ledger, Close

accounts = [
    Asset("cash"),
    Equity("equity"),
    Equity("re", title="Retained earnings"),
    Income("sales"),
    Expense("salaries"),
]
entries = [
    Double("cash", "equity", 1000),
    Double("cash", "sales", 250),
    Double("salaries", "cash", 150),
    Close("re")
]
ledger = Ledger.from_list(accounts+entries)
print(ledger.balances)
print(ledger.income_statement())
print(ledger.balance_sheet())
