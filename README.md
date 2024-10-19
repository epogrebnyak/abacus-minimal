# abacus-minimal

`abacus-minimal` aims to be as concise as possible in implementation of double entry book-keeping rules as applied in corporate accounting.

## Project goals

- Make valid accounting engine in fewer lines of code (Python or other languages).
- Explain book-keeping rules through code examples.
- Make pathways into accounting for programmers and into programming for accountants.
- Curate various charts of accounts as JSON files and make conversions between them.
- Make free web learning tools in accounting similar to [abacus-streamlit][ex].
- Ultimately, lower the book-keeping and financial analytics costs for the businesses.

[ex]: https://abacus.streamlit.app/

Non-goals:

- replacing SAP or QBO immediately with this Python code.

## For the next version

- [ ] `book.income_statement` that works before and after close.
- [ ] `book.balance_sheet` with `current_profit` account when not closed.
- [ ] relax check for negativity, make warning

## Install

```bash
pip install abacus-minimal
```

Latest:

```bash
pip install git+https://github.com/epogrebnyak/abacus-minimal.git
```

## Workflow

The steps for using `abacus-minimal` follow the steps of a typical accounting cycle:

- create a chart of accounts,
- post business transactions to ledger,
- make reconciliations and adjustments,
- close accounts at reporting period end,
- show reports for the financial results,
- save data for the next reporting period.

Complete example code is in [readme.py](readme.py).
For lower level implementation details see ['Data structures and actions'
section below][#ds]

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

Equally, you can load an existing chart from file:

```python
chart = Chart.load("chart.json")
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
    # various formats to specify entry are shown
    Entry("Initial investment").amount(10_000).debit("cash").credit("equity"),
    Entry("Sold services with VAT"
       ).debit("cash", 6000
       ).credit("sales", 5000
       ).credit("vat_payable", 1000),
    Entry("Made client refund").double(debit="refunds", credit="cash", amount=500),
    Entry("Paid salaries").debit("salaries", 1500).credit("cash", 1500),
]
book.post_many(entries)
```

Invalid entries will be rejected with `AbacusError` raised. The invalid entriesa are the ones that touch non-existent accounts or the entries where debits and credits are not balanced.

### 3. Closing accounts

Steps before closing (not in current example):

- make reconciliation entries,
- make adjustment entries for accruals and deferrals.

Closing accounts at period end involves:

- closing contra accounts to temporary accounts,
- closing temporary accounts to the retained earnings account,
- make post-close entries if applicable (not in current example).

Closing accounts was probably the hardest part of the project where I had to refactor code several times to make the code look good to the reader.

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

<a href="ds">
# Data structures and actions

Underneath `Chart`, `Entry` and `Book` clasees there are more primitive data
structures that make up the core of `abacus-minimal`:

- `T5(Enum)` - the indicator of type of an account,
- `ChartDict` - holds chart of accounts information and ensures uniqueness and consistency of account names,
- `SingleEntry` - debit or credit specific account with amount,
- `MultipleEntry` - list of `SingleEntry` items where sum of debit and credit entries match,
- `TAccount` - base class for `DebitAccount` and `CreditAccount`,
- `Ledger` - a dictionary that maps account names to accounts and accepts entries for posting,
- `TrailBalance`, `BalanceSheet` and `IncomeStatement` reports,
- `BalancesDict` - saves account names and their balance.

The principal chain of actions is the following:

- create ledger: `ChartDict` -> `Ledger`
- post entries to ledger: `Ledger` -> `[MultipleEntry]` -> `Ledger`
- make a list of closing entries: (`ChartDict`, `AccountName`) -> `Ledger` -> `[MultipleEntry]`
- close the ledger at period end: (`ChartDict`, `AccountName`) -> `Ledger` -> (`IncomeStatement`, `Ledger`)
- report balance sheet: `Ledger` -> `BalanceSheet`
- show trial balance: `Ledger` -> `Trial Balance`
- save account balances: `Ledger` -> `BalancesDict`.

# Key limitations

Several assumptions and simplifications are used to
`abacus-minimal` more managable to develop -- cannot do everything at once.

The key assumptions are:

- one currency,
- one level of accounts in chart,
- no intermediate accounts,
- no account durations in current chart of accounts (current vs non-current),
- no changes in equity and cash flow statements.

See [main.py](abacus/main.py) module docstring for details.

# Alternatives

`abacus-minimal` takes inspiration from the following great projects:

- [hledger](https://github.com/simonmichael/hledger) and [plain text accounting tools](https://plaintextaccounting.org/),
- [medici](https://github.com/flash-oss/medici) ledger in JavaScript using Mongo database,
- [microbooks](https://microbooks.io/) API and [python-accounting](https://github.com/ekmungai/python-accounting).

Plain text accounting tools are usually for personal finance while `abacus-minimal` targets accounting for a corporate entity.
`medici` is a high performance ledger, but does not enforce accounting rules on data entry.
`python-accounting` is a production-grade project, tightly coupled to a database.

Big players in accounting software are Intuit Quickbooks (US) and Xero (Australia) for small and middle-sized companies. There are ongoing debates which gives more hassle for the users
and the value of provided services, especially when moving form desktop version to the cloud.

Many other office automation providers do also have accounting APIs (eg Zoho) and there are open source packages that have accounting functionality (eg Frappe).

Several outlets advertise they provide IFRS-compliant charts of accounts, but usually as Excel files. Account taxonomies for reporting, but not charts are often published as well.

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
- `black` and `isort --float-to-top` (probably should replace with `ruff format`)
- `ruff check`
- `prettier` for markdown formatting
- `codedown` to extract code from README.md and try running it.
