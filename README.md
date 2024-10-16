# abacus-minimal

`abacus-minimal` aims to be as concise as possible in implementation of double entry book-keeping rules as applied for corporate accounting.

## Project goals

- Make valid accounting engine in fewer lines of code (Python or other languages).
- Explain book-keeping rules through code examples.
- Make routes into accounting for programmers andvand into programming for accountants.
- Curate various charts of accounts as JSON files and make conversions between them.
- Make free web learning tools in accounting similar to [abacus-streamlit][ex].
- Ultimately, lower the book-keeping and analytics costs for the businesses.

[ex]: https://abacus.streamlit.app/

## For the next version

- [ ] `book.income_statement` that works before and after close.
- [ ] `book.balance_sheet` with `current_profit` account when not closed.
- [ ] separate `Entry(Iterable[SingleEntry])` into `MultipleEntry(Entry)`, `DoubleEntry` and `ClosingEntry(Entry)` (is_closing=True)

## Install

```bash
pip install abacus-minimal
```

Latest:

```bash
pip install git+https://github.com/epogrebnyak/abacus-minimal.git
```

## Workflow

The steps for using `abacus-minimal` follow typical accounting cycle:

- create a chart of accounts,
- post transactions to ledger,
- make reconciliations and adjustments,
- close accounts,
- report financial results,
- save data for the next accounting period.

The [readme.py](readme.py) file contains complete example code.

### 1. Create chart of accounts

Steps involved:

- specify name of the retained earnings account that will accumulate company profits less dividend,
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

`Chart` class is a `pydantic` model, which means it is easily converted to a JSON file:

```python
chart.save("chart.json")
```

### 2. Post entries to ledger

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

Invalid entries will be rejected with `AbacusError` raised.

### 3. Closing accounts

Steps before closing (not in current example):

- make reconciliation entries,
- make adjustment entries for accruals and deferrals.

Closing accounts at period end:

- close contra accounts to temporary accounts,
- close temporary accounts to the retained earnings account,
- make post-close entries if applicable (not in current example).

Closing accounts was probably the hardest part of the project
where I had to refactor code several times to make it both correct and
understandable to follow.

I ended up making a list of account pairs for closing based on chart of accounts,
then making closing actual entries from pairs and processing them one by one.

### 4. Reporting and saving

Steps involved:

- show balance sheet and income statement,
- save account balances for the next period.

Saving the book will write `chart.json`, `store.json` and `balances.json` files.

Code example:

```python
# Close at period end and show reports
print(book.income_statement)
book.close()
print(book.balance_sheet)

# Check account balances match expected values
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 10000,
    "vat_payable": 1000,
    "retained_earnings": 3000,
}

# Save everything to JSON files in current folder
book.save(directory=".")
```

# Key limitations

Several assumptions and simplifications are used to make advances in `abacus-minimal` incremental.

The key assumptions are:

- one currency,
- one level of accounts in chart,
- no account durations (current vs non-current),
- no changes in equity and cash flow statements.

See [main.py](abacus/main.py) module docstring for detail.

# Alternatives

`abacus-minimal` takes inspiration from the following great projects:

- [hledger](https://github.com/simonmichael/hledger) and [plain text accounting tools](https://plaintextaccounting.org/),
- [medici](https://github.com/flash-oss/medici) ledger in JavaScript using Mongo database,
- [microbooks](https://microbooks.io/) API and [python-accounting](https://github.com/ekmungai/python-accounting).

Plain text accounting tools are usually for personal finance while `abacus-minimal` targets accounting for a corporate entity.
`medici` is a high performance ledger, but does not enforce any accounting rules.
`python-accounting` is a production-grade project, tightly coupled to a database.

Big players in accounting software are Intuit Quickbooks (US) and Xero (Australia) for small and middle-sized companies.
Many other office automation providers do also have accounting APIs (eg Zoho) and there are open source packages that have accounting functionality (eg Frappe).
Several outlets advertise they provide IFRS-compliant charts of accounts, but usually as Excel files and as account taxanomies, not charts.

# Accounting knowledge

If you are totally new to accounting the suggested friendly course is <https://www.accountingcoach.com/>.

ACCA and CPA are the international and the US professional qualifications and IFRS and GAAP are the accounting standards for accounting recognition, measurement and disclosure.

You might want to review part B-G in the [ACCA syllabus for the FFA exam](https://www.accaglobal.com/content/dam/acca/global/PDF-students/acca/f3/studyguides/fa-ffa-syllabusandstudyguide-sept23-aug24.pdf)
to familiarize yourself with what `abacus-minimal` is designed for.

# Implementation detail

I use [`just` command runner](https://github.com/casey/just) to automate code maintenance tasks in this project.

`just test` and `just fix` scripts will run the following tools:

- `pytest`
- `mypy`
- `black` and `isort --float-to-top`
- `ruff`
- other utilities as specified in [`justfile`](justfile).
