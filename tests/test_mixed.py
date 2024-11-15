from abacus import Book, Chart, Entry, Ledger
from abacus.ledger import BalanceSheet


def test_readme():
    chart = Chart(
        retained_earnings="re",
        current_earnings="profit",
        assets=["cash"],
        equity=["equity"],
        income=["sales"],
        contra_accounts=dict(equity=["ts"], sales=["refunds"]),
    )
    book = Book.from_chart(chart)
    book.post(Entry("Launch").debit("cash", 10).credit("equity", 10))
    book.post(Entry("Sold services").double(debit="cash", credit="sales", amount=50))
    book.post(Entry("Issued refund").debit("refunds", 40).credit("cash", 40))
    book.post(Entry("Made buyback").double(debit="ts", credit="cash", amount=8))
    assert book.income_statement.net_earnings == 10
    book.close()
    assert book.balance_sheet == BalanceSheet(
        assets={"cash": 12},
        equity={"equity": 2, "re": 10},
        liabilities={},
    )


def test_end_to_end(realistic_chart):
    ledger = Ledger.from_accounts(realistic_chart)
    entries = [
        Entry("Start").debit("cash", 20).credit("equity", 20),
        Entry("Accepted payment")
        .debit("cash", 120)
        .credit("sales", 100)
        .credit("vat", 20),
        Entry("Refund").double("refunds", "cash", 5),
        Entry("Paid salaries").double(debit="wages", credit="cash", amount=10),
        Entry("Paid VAT due").double(debit="vat", credit="cash", amount=20),
    ]
    for entry in entries:
        ledger.apply(entry.to_multiple())
    ledger.close("re")
    assert ledger.balances == {
        "cash": 105,
        "inventory": 0,
        "ar": 0,
        "vat": 0,
        "ap": 0,
        "equity": 20,
        "re": 85,
        "ts": 0,
    }

from abacus import Asset, Close, Contra, Double, Equity, Expense, History, Income, Initial, Multiple, T5, Liability, Event, Add

def test_very_mixed():
        
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
        
