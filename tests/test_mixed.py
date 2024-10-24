from abacus import Chart, Entry
from abacus.core import BalanceSheet, CreditEntry, DebitEntry


def test_readme():
    chart = Chart(
        retained_earnings="re",
        assets=["cash"],
        capital=["equity"],
        income=["sales"],
        contra_accounts=dict(equity=["ts"], sales=["refunds"]),
    )
    chart_dict = chart.to_dict()
    ledger = chart_dict.to_ledger()
    ledger.post(Entry("Launch").debit("cash", 10).credit("equity", 10))
    ledger.post(Entry("Sold services").double(debit="cash", credit="sales", amount=50))
    ledger.post(Entry("Issued refund").debit("refunds", 40).credit("cash", 40))
    ledger.post(Entry("Made buyback").double(debit="ts", credit="cash", amount=8))
    assert ledger.income_statement(chart_dict).net_earnings == 10
    closing_pairs = chart.closing_pairs
    ledger.close(closing_pairs)
    assert ledger.balance_sheet(chart_dict) == BalanceSheet(
        assets={"cash": 12}, capital={"equity": 2, "re": 10}, liabilities={}
    )


def test_end_to_end(realistic_chart):
    chart_dict = realistic_chart.to_dict()
    ledger = chart_dict.to_ledger()
    entries = [
        Entry("Start").debit("cash", 20).credit("equity", 20),
        Entry("Accepted payment")
        .debit("cash", 120)
        .credit("sales", 100)
        .credit("vat", 20),
        [DebitEntry("refunds", 5), CreditEntry("cash", 5)],
        Entry("Paid salaries").double(debit="wages", credit="cash", amount=10),
        Entry("Paid VAT due").double(debit="vat", credit="cash", amount=20),
    ]
    for entry in entries:
        ledger.post(entry)
    closing_pairs = realistic_chart.closing_pairs
    ledger.close(closing_pairs)
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
