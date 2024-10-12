# abacus-minimal

Accounting system in Python that allows to create a chart of accounts,
start general ledger, post entries, properly close ledger
at accounting period end and produce trial balance, balance sheet 
and income statement reports.

More features:

- allows contra accounts;
- double or multiple entries;
- saves everything to JSON files.

Limitations:

- one currency;
- one level of accounts;
- no cash flow statement.

## Install

```bash
git clone https://github.com/epogrebnyak/abacus-minimal.git
cd abacus-minimal
```

## Usage example

```python
from abacus import Book

book = Book.new()
book.chart.assets.append("cash")
book.chart.capital.append("equity")
book.chart.income.append("sales")
book.chart.offset("sales", "refunds")
book.chart.expenses.append("salaries")
book.post_double("Initial investment", debit="cash", credit="equity", amount=10000)
book.post_double("Sold services", debit="cash", credit="sales", amount=6500)
book.post_double("Made refund", debit="refunds", credit="cash", amount=500)
book.post_double("Paid salaries", debit="salaries", credit="cash", amount=1000)
book.close()
assert book.ledger.balances == {
    "cash": 15000,
    "equity": 10000,
    "retained_earnings": 5000,
}
```

