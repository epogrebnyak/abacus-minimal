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

## Changelog

- `0.10.0` (2024-10-24) -- separates core, chart, entry and book code and tests.

### For the `0.11.0` version

- [ ] `book.income_statement` that works before and after period close
- [ ] `book.balance_sheet` with `current_profit` account when not closed + `Chart.current_earnings` attribute

### Future versions

- cleaner `BalancesDict`
- [ ] `Book.increase()` and `Book.decrease()` methods
- `Entry.explain()` and `Entry.validate()` methods

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

The complete example code for the workflow is in [readme.py](examples/readme.py).
For more implementation details see "Data structures, actions and architecture" section below.

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

# Create book with opening balances
opening_balances = {"cash": 10_000, "equity": 10_000}
book = Book(chart, opening_balances)

# Post entries
entries = [
    Entry("Sold services with VAT")
    .debit("cash", 6000)
    .credit("sales", 5000)
    .credit("vat_payable", 1000),
    Entry("Made client refund").double(debit="refunds", credit="cash", amount=500),
    Entry("Paid salaries").debit("salaries", 1500).credit("cash", 1500),
]
book.post_many(entries)
print(book.trial_balance)
```

Invalid entries will be rejected with `AbacusError` raised. The invalid entries are the ones that touch non-existent accounts or the entries where debits and credits are not balanced.

### 3. Closing accounts

Steps before closing (not in current example):

- make reconciliation entries,
- make adjustment entries for accruals and deferrals.

Closing accounts at period end involves:

- closing contra accounts to temporary accounts,
- closing temporary accounts to the retained earnings account,
- make post-close entries if applicable (not in current example).

Closing accounts was probably the hardest part of the project
where I had to refactor code several times
to make it more explicit and concise.

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

## Data structures, actions and architecture

### Data structures

Underneath `Chart`, `Entry` and `Book` clasees there are more primitive data
structures that make up the core of `abacus-minimal`:

- `ChartDict` holds chart of accounts information and ensures uniqueness and consistency of account names.
- `SingleEntry` specifies amount and debit or credit side.
- `MultipleEntry` is a list of `SingleEntry` items where sum of debit and credit entries should match.
- `TAccount` is the base class for `DebitAccount` and `CreditAccount`.
- `Ledger` is a dictionary that maps account names to accounts and accepts entries for posting.
- `TrailBalance` and `BalancesDict` show account names and their balances.
- `BalanceSheet` and `IncomeStatement` are financial reports based on ledger state.

### Actions

The principal chain of actions in `abacus-minimal` is shown in a table below.
The function signatures (or type annotations) indicate want variables participate
in the calculation.

| Action                       | Function signature                                                    |
| ---------------------------- | --------------------------------------------------------------------- |
| Create ledger                | `ChartDict` -> `Ledger`                                               |
| Post entries to ledger       | `Ledger` -> `[MultipleEntry]` -> `Ledger`                             |
| Make a list of closing pairs | `(ChartDict, AccountName)` -> `[(AccountName, AccountName)]`          |
| Close ledger at period end   | `(ChartDict, AccountName)` -> `Ledger` -> `(IncomeStatement, Ledger)` |
| Report balance sheet         | `Ledger` -> `BalanceSheet`                                            |
| Show trial balance           | `Ledger` -> `TrialBalance`                                           |
| Show account balances        | `Ledger` -> `BalancesDict`.                                           |

# Architecture

`abacus-minimal` focuses on enforcing the book-keeping rules, but not on not storing entires.


Unlike production accounting projects `abacus-minimal` is database-free. For `abacus-minimal` it would not  
matter how entries are saved and where they are coming from -- this responsibility should be taken by some other
part of software, eg the `medici` ledger. The `Book` class does offer saving of entries to a JSON file,
but this is done for demonstration only.

# Limitations

Several assumptions and simplifications are used to make `abacus-minimal` more manageable to develop.

The key assumptions are:

- one currency,
- one level of accounts in chart,
- no intermediate accounts,
- no changes in equity and cash flow statements.

See [main.py](abacus/main.py) module docstring for more details.

# Alternatives

`abacus-minimal` takes inspiration from the following great projects:

- [hledger](https://github.com/simonmichael/hledger) and [plain text accounting tools](https://plaintextaccounting.org/),
- [medici](https://github.com/flash-oss/medici) ledger in JavaScript using Mongo database,
- [microbooks](https://microbooks.io/) API and [python-accounting](https://github.com/ekmungai/python-accounting).

Plain text accounting tools are usually for personal finance while `abacus-minimal` targets accounting for a corporate entity.
`medici` is a high performance ledger, but does not enforce accounting rules on data entry.
`python-accounting` is a production-grade project, tightly coupled to a database.

Big players in accounting software for small and middle-sized companies
are Intuit Quickbooks (US) and Xero (Australia). 
Not everyone is happy with how they work or how much they cost, especially 
when moving from a desktop version to the cloud.

Many other office automation providers do also have accounting APIs (eg Zoho) and there are open source packages that have accounting functionality (eg Frappe).

Several outlets advertise they provide IFRS-compliant charts of accounts, but usually as Excel files. There are account taxonomies for reporting, but the charts more seldom.

# Accounting knowledge

If you are totally new to accounting the suggested friendly course is <https://www.accountingcoach.com/>.

ACCA and CPA are the international and the US professional qualifications and IFRS and GAAP are the standards for accounting recognition, measurement and disclosure.

Part B-G in the [ACCA syllabus for the FFA exam](https://www.accaglobal.com/content/dam/acca/global/PDF-students/acca/f3/studyguides/fa-ffa-syllabusandstudyguide-sept23-aug24.pdf)
talk about what `abacus-minimal` is designed for.

# Project conventions

I use [`just` command runner](https://github.com/casey/just) to automate
code maintenance tasks in this project.

`just test` and `just fix` scripts will run the following tools:

- `pytest`
- `mypy`
- `black` and `isort --float-to-top` (probably should replace with `ruff format`)
- `ruff check`
- `prettier` for markdown formatting
- `codedown` to extract Python code from README.md.

`examples/readme.py` is overwritten by the `just readme` command.
