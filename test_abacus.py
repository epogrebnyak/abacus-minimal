from abacus import ChartDict, Regular, Contra, T5, Chart, double_entry, Entry

cd = ChartDict([("sales", Regular(T5.Income)), ("cashback", Contra("sales"))])
print(cd.t_account("sales"))
print(cd.t_account("cashback"))


chart = Chart(
    retained_earnings="re",
    assets=["cash"],
    capital=["equity"],
    liabilities=["vat"],
    income=["sales"],
    expenses=["wages"],
).offset("sales", "cashback")
print(chart)
print(chart.to_dict())
print(chart.closing_pairs)
ledger = chart.to_ledger()
print(ledger)
ledger.post_many(
    [
        double_entry("cash", "equity", 20),
        Entry().dr("cash", 120).cr("sales", 100).cr("vat", 20),
        double_entry("cashback", "cash", 5),
        double_entry("wages", "cash", 10),
        double_entry("vat", "cash", 20),
    ]
)
ledger.close(chart)
print(ledger.trial_balance)
print(ledger.balances)
