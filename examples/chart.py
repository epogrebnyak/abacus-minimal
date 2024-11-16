from abacus.chart import Chart, ChartBase, Earnings, QualifiedChart


def test_qualified():
    assert Chart(
        retained_earnings="re",
        current_earnings="profit",
        assets=["cash"],
        equity=["equity"],
        contra_accounts={"equity": ["ts"]},
    ).qualified == QualifiedChart(
        earnings=Earnings(current="profit", retained="re"),
        base=ChartBase(
            assets=["cash"],
            equity=["equity", "re"],
            liabilities=[],
            income=[],
            expenses=[],
            contra_accounts={"equity": ["ts"]},
            names={},
        ),
    )
