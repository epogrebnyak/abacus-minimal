# abacus-minimal

`abacus-minimal` aims to be as concise as possible in implementation of double entry book-keeping rules as applied for corporate accounting.

## Project goals

- Make valid accounting engine in fewer lines of code (Python or other languages).
- Explain book-keeping rules through code examples.
- Make routes into accounting for programmers and vice versa.
- Curate various charts of accounts as JSON files and make conversions between them.
- Make free web learning tools in accounting similar to [abacus-streamlit][ex].
- Ultimately, lower the book-keeping costs for the businesses. 

[ex]: https://abacus.streamlit.app/

## For the next version

- [ ] `book.income_statement` that works before and after close.
- [ ] `book.balance_sheet` with `current_profit` account when not closed. 

## Install

```bash
git clone https://github.com/epogrebnyak/abacus-minimal.git
cd abacus-minimal
```

## Workflow

There are four steps involved in using `abacus-minimal` – creating a chart of accounts, 
posting transactions to ledger, closing accounts and reporting financial results.

### 1. Create chart of accounts

Steps involved:

- specify name of the retained earnings account that will accumulate company gains and losses,
- add account names for assets, capital, liabilities, income and expenses,
- add contra accounts (eg refunds is a contra account to sales).

Code example:

```python
from abacus import Chart

chart = Chart(
    retained_earnings="retained_earnings",
    assets=["cash"],
    capital=["equity"],
    liabilities=["vat_payable"],
    income=["sales"],
    expenses=["salaries"],
)
chart.offset("sales", "refunds")
```

Note:

- `Chart` class is a `pydantic` model, which means it is easily converted to a human-readable JSON file.

###  2. Post entries to ledger

Steps involved:

- create a data structure that represents state of accounts (ledger),
- record account starting balances from the previous period (skip for a new company),
- record accounting entries that represent business transactions,
- show state of ledger (trial balance or account balances) at any time.

Code example:

```python
from abacus import Book, Entry

book = Book(chart)
entries = [
    Entry("Initial investment", amount=10_000).debit("cash").credit("equity"),
    Entry("Sold services with VAT").debit("cash", 6000).credit("sales", 5000).credit("vat_payable", 1000),
    Entry("Made client refund", amount=500).debit("refunds").credit("cash"),
    Entry("Paid salaries", amount=1500).debit("salaries").credit("cash"),
]
book.post_many(entries)
```

Note:

- invalid entries will be rejected with `AbacusError` raised.

### 3. Closing account and reporting

Closing accounts at period end:

- make reconciliation entries (not in current example),
- make adjustment entries for accruals and deferrals (not in current example), 
- close temporary accounts to the retained earnings account,
- make post-close entries if applicable, eg dividend payout (not in current example).

Reporting:

- show balance sheet and income statement,
- save account balances for the next period. 

Code example:

```python 
# Close at period end and show reports
print(book.income_statement)
book.close()
print(book.balance_sheet)

# Check account balances match expected values
# (used as part of testing)
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 10000,
    "vat_payable": 1000,
    "retained_earnings": 3000,
}

# Save everything to JSON files in current folder
book.save(directory=".")
```

Notes:

- closing accounts properly was probably the hardest part of the project
  where I had to refactor code several times to make it both correct and legible,
- saving the book will write `chart.json`, `store.json` and `balances.json` files.

### Complete example

Complete usage example is located in [readme.py](readme.py) file.

### Key limitations

Several assumptions and simplifications are used to make advances in
`abacus-minimal` incremental. 

The key assumptions are:

- one currency,
- one level of accounts in chart,
- no account durations (current vs non-current),  
- no changes in equity and cash flow statements yet.

See [abacus.py](abacus.py) source code in the module docstring for detail.

# Alternatives

`abacus-minimal` takes inspiration from the following great projects:

- [hledger](https://github.com/simonmichael/hledger) and [plain text accounting tools](https://plaintextaccounting.org/),
- [medici](https://github.com/flash-oss/medici) ledger in JavaScript using Mongo database,
- [microbooks](https://microbooks.io/) API and [python-accounting](https://github.com/ekmungai/python-accounting).

Plain text accounting tools are usually for personal finance while `abacus-minimal` targets accounting for a corporate entity. 
`medici` is a high performance ledger, but does not enforce the accounting rules on data entry. 
`python-accounting` is a production-grade project, tightly coupled to a database. 

Big players in accounting software are Intuit Quickbooks (US) and Xero (Australia) for small and middle-sized companies.
Many other office automation providers do also have accounting APIs (eg Zoho) and there are open source packages that have accounting functionality (eg Frappe). 
Several outlets advertise they provide IFRS-compliant charts of accounts, but usually as Excel files.

# Accounting knowledge

If you are totally new to accounting the suggested friendly course is <https://www.accountingcoach.com/>. 

ACCA and CPA are the international and the US professional qualifications and IFRS and GAAP are the accounting standards for accounting recognition, measurement and disclosure. 

You might want to review part B-G in the [ACCA syllabus for the FFA exam](https://www.accaglobal.com/content/dam/acca/global/PDF-students/acca/f3/studyguides/fa-ffa-syllabusandstudyguide-sept23-aug24.pdf)
to familiarize yourself with what `abacus-minimal` allows you to do and figure out its limitations.

# Implementation detail

I use a superb [`just` command runner](https://github.com/casey/just) to invoke `just test` and `just fix` scripts that will run:

- `pytest`,
- `mypy`,
- `black` and `isort --float-to-top`,
- `ruff`,
- other quality-of-life utilities as specified in `justfile`.

A future version of `abacus-minimal` will be a PyPI package managed through `poetry`.
