# abacus-minimal

`abacus-minimal` aims to be concise and expressive in implementation of double entry book-keeping rules for corporate accounting.

Project goals are the following:

- make valid accounting engine in fewer lines of code (thus `minimal` in project name);
- curate various charts of accounts as JSON files and make conversions between them;
- make free web learning tools in accounting similar to [abacus-streamlit][ex].

[ex]: https://abacus.streamlit.app/

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
- open ledger for the current reporting period,
- post entries that reflect business transactions,
- post reconciliation and adjustment entries,
- close accounts at reporting period end,
- show reports for the financial results,
- save the data for the next reporting period.

## Code example

The complete code example is in [readme.py](examples/readme.py).

### 1. Create chart of accounts

Steps involved:

- specify names of the current earnings and retained earnings accounts,
- add account names for assets, capital, liabilities, income and expenses,
- add contra accounts (in example below `refunds` is a contra account to `sales`).

Code example:

```python
from abacus import Chart

chart = Chart(
    retained_earnings="retained_earnings",
    current_earnings="current_earnings",
    assets=["cash", "ar"],
    capital=["equity"],
    liabilities=["vat_payable"],
    income=["sales"],
    expenses=["salaries"],
)
chart.offset("sales", "refunds")
chart.name("ar", "Accounts receivable")
```

`Chart` class is a `pydantic` model, which means it is easily converted to a JSON file.
You can save a chart to or load a chart from a JSON file.

```python
chart.save("chart.json")
chart = Chart.load("chart.json")
```

### 2. Post entries to ledger

Steps involved:

- create a data structure that represents state of accounts (ledger),
- record account starting balances from the previous period,
- record entries that represent business transactions,
- show state of ledger (trial balance or account balances) at any time.

Code example:

```python
from abacus import Book, Entry

# Create book with account opening balances from previous period
opening_balances = {"cash": 10_000,
                    "equity": 8_000,
                    "retained_earnings": 2_000}
book = Book(chart, opening_balances)

# Create a list of entries using a type notation you prefer
entries = [
    Entry("Sales with VAT").debit("cash", 6000).credit("sales", 5000).credit("vat_payable", 1000),
    Entry("Сlient refund").double(debit="refunds", credit="cash", amount=500),
    Entry("Paid salaries").amount(1500).debit("salaries").credit("cash"),
]

# Post entries to book
book.post_many(entries)

# Show trial balance and account balances
print(book.trial_balance)
print(book.legder.balances)
```
### 3. Closing accounts 

Closing accounts at period end involves:

- closing contra accounts to income and expense accounts, and 
- closing income and expense accounts to retained earnings.

Steps not shown in current example:

1. Before closing there are reconciliation entries and adjustment entries for accruals and deferrals.
2. After closing one can also make post-close entries that affect permanent accounts.

### 4. Reporting financial statements 

Financial reports can be shown before and after account closing:

- income statement will be the same before and after closing,
- before closing the balance sheet will contain current earnings and retained earnings from previous periods,
- after closing the balance sheet has just retained earnings, the current earnings account is removed from ledger.

Trial balance and account balances can be shown at any time through the accounting cycle.

Code example:

```python
# Income statement and balance sheet before closing
print("=== Before closing ===")
print(book.income_statement)
print(book.balance_sheet)

# Check account balances match expected values
print(book.ledger.balances)
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 8000,
    "vat_payable": 1000,
    "sales": 5000,
    "refunds": 500,
    "salaries": 1500,
    "current_earnings": 0,
    "retained_earnings": 2000,
}

# Close accounts at period end
book.close()

# Show income statement and balance sheet after closing
print("=== After closing ===")
print(book.income_statement)
print(book.balance_sheet)

# Check account balances match expected values. Т
print(book.ledger.balances)
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 8000,
    "vat_payable": 1000,
    "retained_earnings": 5000,
}

### 5. Saving data for the next period

Saving the book will write `chart.json`, `store.json` and `balances.json` files
to specified folder.

```python
# Save JSON files in current folder
book.save(directory=".")
```

## Limitations

Several assumptions and simplifications are used to make `abacus-minimal` easier to develop
and reason about. The key assumptions are:

- one currency,
- one level of accounts in chart,
- no intermediate accounts,
- no changes in equity and cash flow statements.

See [core.py](abacus/core.py) module docstring for more details.

## Alternatives

`abacus-minimal` takes inspiration from the following great projects:

- [hledger](https://github.com/simonmichael/hledger) and [plain text accounting tools](https://plaintextaccounting.org/),
- [medici](https://github.com/flash-oss/medici) ledger in JavaScript using Mongo database,
- [microbooks](https://microbooks.io/) API and [python-accounting](https://github.com/ekmungai/python-accounting).

Plain text accounting tools are usually for personal finance while `abacus-minimal` targets accounting for a corporate entity.
`medici` is a high performance ledger, but does not enforce accounting rules on data entry.
`python-accounting` is a production-grade project, tightly coupled to a database.

## Accounting knowledge

If you are totally new to accounting the suggested friendly course is <https://www.accountingcoach.com/>.

ACCA and CPA are the international and the US professional qualifications and IFRS and GAAP are the standards for accounting recognition, measurement and disclosure.

Part B-G in the [ACCA syllabus for the FFA exam](https://www.accaglobal.com/content/dam/acca/global/PDF-students/acca/f3/studyguides/fa-ffa-syllabusandstudyguide-sept23-aug24.pdf) talk about what `abacus-minimal` is designed for.

## Project conventions

I use [`just` command runner](https://github.com/casey/just) to automate code maintenance tasks in this project.

`just test` and `just fix` scripts will run the following tools:

- `pytest`
- `mypy`
- `black` and `isort --float-to-top` (probably should replace with `ruff format`)
- `ruff check`
- `prettier` for markdown formatting
- `codedown` to extract Python code from README.md.

`examples/readme.py` is overwritten by the `just readme` command.

I use `poetry` as a package manager, but heard good things about `uv`. 

## Changelog

- `0.10.0` (2024-10-24) separates core, chart, entry and book code and tests.

## Roadmap

### For cleanup

- [x] `book.income_statement` that works before and after period close
- [x] `book.balance_sheet` with `current_earnings` account when not closed, `Chart.current_earnings` attribute
- [x] dedupulicate `Book.open()`
- [ ] cleaner `BalancesDict`
- [ ] reorder `test_book.py`

### New features

- [ ] `Book.increase()` and `Book.decrease()` methods
- [ ] `Entry.explain()` and `Entry.validate()` methods
