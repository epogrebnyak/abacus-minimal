# abacus-minimal

Accounting system in Python that allows to create a chart of accounts,
start general ledger, post entries, properly close ledger
at accounting period end and produce trial balance, balance sheet 
and income statement reports.

More features:

- allows contra accounts;
- double or multiple entries;
- saves everything to JSON files.

Intentions:

- explain book-keeping rules through code;
- make route into accounting for programmers and the route opposite way;
- curate standard chart of accounts by country;
- make web learning tools in accounting.  

Limitations:

- one currency;
- one level of accounts;
- no cash flow statement yet.

## Install

```bash
git clone https://github.com/epogrebnyak/abacus-minimal.git
cd abacus-minimal
```

## Usage example

```python
from abacus import Chart, Book, Entry


# Create chart of accounts
chart = Chart(
    retained_earnings="retained_earnings",
    assets=["cash"],
    capital=["equity"],
    liabilities=["vat"],
    income=["sales"],
    expenses=["salaries"],
)
chart.offset("sales", "refunds")

# Post entries
book = Book(chart=chart)
book.post_double("Initial investment", debit="cash", credit="equity", amount=10000)
book.post(
    Entry("Sold services with VAT")
    .debit("cash", 6000)
    .credit("sales", 5000)
    .credit("vat", 1000)
)
book.post_double("Made client refund", debit="refunds", credit="cash", amount=500)
book.post_double("Paid salaries", debit="salaries", credit="cash", amount=1500)

# Close at period end
book.close()
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 10000,
    "vat": 1000,
    "retained_earnings": 3000,
}
book.save(directory=".")
```

