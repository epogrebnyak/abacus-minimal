# Micro example

from abacus import Asset, Close, Double, Equity, Expense, Income, Ledger

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
    Close("re"),
]
events = accounts + entries
ledger = Ledger.from_list(events)
print(ledger.balances)
print(ledger.income_statement())
print(ledger.balance_sheet())
print(list(ledger.history.primitives))
