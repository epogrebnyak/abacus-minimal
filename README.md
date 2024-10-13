# abacus-minimal

`abacus-minimal` aims to be as concise as possible in implemetation
of book-keeping rules.

`abacus-minimal` allows to create a chart of accounts,
open general ledger, post entries, and properly close ledger
at accounting period end.

On input we get a Chart and list on entries and on output we
get balance, balance sheet and income statement reports.

Extra features:

- allows contra accounts,
- double or multiple entries,
- saves everything to JSON files.

Limitations:

- one currency,
- one level of accounts,
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
book = Book(chart)
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

# Save to JSON files in current folder
book.save(directory=".")
```

npm install --save-dev --save-exact prettier

## Project intentions

- Explain book-keeping rules through code,
- Make route into accounting for programmers and the route opposite way,
- Curate typical charts of accounts by country and convert between them.
- Make web learning tools in accounting like [abacus-streamlit][ex].

[ex]: https://abacus.streamlit.app/

# Alternatives

`abacus-minimal` is inspired by the following great projects:

- [hledger](https://github.com/simonmichael/hledger) and [plain text accounting tools](https://plaintextaccounting.org/).
- [medici](https://github.com/flash-oss/medici) ledger in JavaScript based on Mongo database.
- [microbooks](https://microbooks.io/) API.
