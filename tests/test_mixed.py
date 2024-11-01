from abacus import Chart, Entry
from abacus.core import BalanceSheet


def test_readme():
    chart = Chart(
        retained_earnings="re",
        current_earnings="profit",
        assets=["cash"],
        capital=["equity"],
        income=["sales"],
        contra_accounts=dict(equity=["ts"], sales=["refunds"]),
    )
    chart_dict = chart.mapping
    ledger = chart_dict.to_ledger()
    ledger.post(Entry("Launch").debit("cash", 10).credit("equity", 10).posting)
    ledger.post(
        Entry("Sold services").double(debit="cash", credit="sales", amount=50).posting
    )
    ledger.post(Entry("Issued refund").debit("refunds", 40).credit("cash", 40).posting)
    ledger.post(
        Entry("Made buyback").double(debit="ts", credit="cash", amount=8).posting
    )
    assert ledger.income_statement(chart_dict).net_earnings == 10
    closing_pairs = chart.make_closing_pairs(chart.retained_earnings)
    ledger.close(closing_pairs)
    assert ledger.balance_sheet(chart_dict) == BalanceSheet(
        assets={"cash": 12},
        capital={"equity": 2, "re": 10, "profit": 0},
        liabilities={},
    )


def test_end_to_end(realistic_chart):
    chart_dict = realistic_chart.mapping
    ledger = chart_dict.to_ledger()
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
        ledger.post(entry.posting)
    closing_pairs = realistic_chart.make_closing_pairs(
        realistic_chart.retained_earnings
    )
    ledger.close(closing_pairs)
    assert ledger.balances == {
        "cash": 105,
        "inventory": 0,
        "ar": 0,
        "vat": 0,
        "ap": 0,
        "equity": 20,
        "re": 85,
        "profit": 0,
        "ts": 0,
    }
