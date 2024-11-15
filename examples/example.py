from abacus import (
    T5,
    Add,
    Asset,
    Close,
    Contra,
    Double,
    Equity,
    Event,
    Expense,
    History,
    Income,
    Initial,
    Ledger,
    Liability,
    Multiple,
)

# Create accounts
accounts = [
    Asset("cash"),
    Equity("equity"),
    Income("services", contra_accounts=["refunds", "voids"]),
    Liability("vat", title="VAT due to tax authorities"),
    Expense("salaries"),
    Equity("retained_earnings"),
]

entries = [
    # Start ledger with initial balances
    Initial({"cash": 10, "equity": 8, "retained_earnings": 2}),
    # Make transactions
    Multiple(debits=[("cash", 120)], credits=[("services", 100), ("vat", 20)]),
    Double("refunds", "cash", 15),
    Double("voids", "cash", 15),
    Double("salaries", "cash", 50),
    # Close period end
    Close("retained_earnings"),
]

ledger = Ledger.from_accounts(accounts).apply_many(entries)
print(ledger.chart)
print(ledger.balances)
assert len(ledger.history.events) == len(accounts) + len(entries)
assert ledger.balances == {
    "cash": 50,
    "equity": 8,
    "vat": 20,
    "retained_earnings": 22,
}
assert ledger.chart == {
    "cash": T5.Asset,
    "equity": T5.Equity,
    "services": T5.Income,
    "refunds": Contra(name="services"),
    "voids": Contra(name="services"),
    "salaries": T5.Expense,
    "retained_earnings": T5.Equity,
    "vat": T5.Liability,
}
ledger2 = ledger.history.to_ledger()
assert ledger2.balances == ledger.balances
if ledger.accounts_before_close is not None:
    assert len(ledger.accounts_before_close) > 3
assert ledger2.is_closed() is True
assert ledger2.income_statement().net_earnings == 20
assert ledger2.balance_sheet().is_balanced() is True
content = ledger.history.model_dump_json(indent=2)
history2 = History.model_validate_json(content)
for a, b in zip(history2, ledger.history):
    assert a == b
history2.save("history2.json", allow_overwrite=True)
history3 = History.load("history2.json")
for a, b in zip(history2, history3):
    assert a == b


e = Event(
    action=Add(name="cash", t=T5.Asset, tag="add"),
    primitives=[Add(name="cash", t=T5.Asset, tag="add")],
    note=None,
)
h = History(events=[e])
d = h.model_dump()
h2 = History.model_validate(d)
print(h2)
