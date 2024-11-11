from events import Credit, Debit, Decimal, Multiple


def test_multiple():
    m = Multiple(
        debits=[("cash", 10)], credits=[("equity", 8), ("retained_earnings", 2)]
    )
    assert list(m) == [
        Debit(account="cash", amount=Decimal("10")),
        Credit(account="equity", amount=Decimal("8")),
        Credit(account="retained_earnings", amount=Decimal("2")),
    ]
