# abacus-minimal

`abacus-minimal` aims to be as concise as possible in implemetation
of book-keeping rules.

`abacus-minimal` allows to create a chart of accounts,
open general ledger, post entries, and properly close ledger
at accounting period end. On output we get trial balance, 
balance sheet and income statement reports.

Extra features:

- contra accounts,
- double or multiple entries,
- everything saved to JSON files.

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
from abacus import Book, Chart, Entry

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
entries = [
    Entry("Initial investment", amount=10_000).debit("cash").credit("equity"),
    Entry("Sold services with VAT").debit("cash", 6000).credit("sales", 5000).credit("vat", 1000),
    Entry("Made client refund", amount=500).debit("refunds").credit("cash"),
    Entry("Paid salaries", amount=1500).debit("salaries").credit("cash"),
]
book.post_many(entries)

# Close at period end
print(book.income_statement)
book.close()
print(book.balance_sheet)
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 10000,
    "vat": 1000,
    "retained_earnings": 3000,
}

# Save to JSON files in current folder
book.save(directory=".")
```

## Project intent

- Explain book-keeping rules through code.
- Make routes into accounting for programmers and vice versa.
- Curate typical charts of accounts by country and convert between them.
- Make web learning tools in accounting similar to [abacus-streamlit][ex].

[ex]: https://abacus.streamlit.app/

# Alternatives

`abacus-minimal` is inspired by the following great projects:

- [hledger](https://github.com/simonmichael/hledger) and [plain text accounting tools](https://plaintextaccounting.org/),
- [medici](https://github.com/flash-oss/medici) ledger in JavaScript based on Mongo database,
- [microbooks](https://microbooks.io/) API.
