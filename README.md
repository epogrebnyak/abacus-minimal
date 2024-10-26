# abacus-minimal

`abacus-minimal` aims to be concise and expressive in implementation of double entry book-keeping rules for corporate accounting.

The project goals is to make a valid accounting engine in fewer lines of code.

## Current version

![PyPI - Version](https://img.shields.io/pypi/v/abacus-minimal)

`0.10.4` is a good candidate release for `1.0` -- I will be looking for
comments and peer review on this version of `abacus-minimal` (reddit, HN, etc).

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
- show financial reports,
- save the data for the next reporting period.

## Code example

In this code example we will programmatically run
the accounting workflow within one reporting period
using `abacus-minimal`.

The inputs to this code are:

- the chart of accounts,
- account opening balances from previous period,
- accounting entries that reflect business transactions within the reporting period.

The resulting outputs are:

- account balances at period end,
- balance sheet,
- income statement.

There are no reconciliations, adjustments and post-close entries in this example.

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
You can save or load a chart from a file.

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

Trial balance and account balances can be displayed at any time.

Let's create a book with opening balances known from previous period:

```python
from abacus import Book

opening_balances = {
    "cash": 10_000,
    "equity": 8_000,
    "retained_earnings": 2_000
    }
book = Book(chart, opening_balances)
```

At this point the book is ready ro record entries.
Each entry has a title and directions to alter the accounts that are called debits and credits.
The sum of debits should match the sum of credits.

The `Entry` class provies several ways to record the composition of an entry as shown below:

```python
from abacus import Entry

entries = [
    Entry("Invoice with VAT").debit("ar", 6000).credit("sales", 5000).credit("vat_payable", 1000),
    Entry("Cash payment").debit("cash", 6000).credit("ar", 6000),
    Entry("Cashback").double(debit="refunds", credit="cash", amount=500),
    Entry("Paid salaries").amount(1500).debit("salaries").credit("cash"),
]

# Post entries to book
book.post_many(entries)
```

After posting entries you can inspect the trial balance or account balances:

```python
# Show trial balance and account balances
print(book.trial_balance)
print(book.balances)

# Check account balances match expected values
assert book.balances == {
    "cash": 14000,
    "ar": 0,
    "equity": 8000,
    "vat_payable": 1000,
    "sales": 5000,
    "refunds": 500,
    "salaries": 1500,
    "current_earnings": 0,
    "retained_earnings": 2000,
}
```

### 3. Closing accounts

Closing accounts at period end involves:

- closing contra accounts to income and expense accounts, and
- closing income and expense accounts to retained earnings.

Code to close accounts shown in the section below.

### 4. Reporting financial statements

Financial reports are typically dslayed after account closing,
but can be shown before closing as well.

**The income statement** will be the same before and after closing.

**The balance sheet** before closing the will contain current earnings
and retained earnings from previous periods.
After closing the current earnings account will be transfered
to retained earnings account, the current earnings account
is removed from the ledger and does not appear in balance sheet.

Expect to see a lot of dictionary-like data structures in code ouput below:

```python
print("=== Before closing ===")
print(book.income_statement)
print(book.balance_sheet)
assert book.balance_sheet.capital["current_earnings"] == 3000

# Close accounts at period end
book.close()

print("=== After closing ===")
print(book.income_statement)
print(book.balance_sheet)

# Check account balances match expected values
print(book.balances)
assert book.balances == {
    "cash": 14000,
    "ar": 0,
    "equity": 8000,
    "vat_payable": 1000,
    "retained_earnings": 5000,
}
```

### 5. Saving data for the next period

It makes sense to save the entries and period end account balances
to JSON files. You will not be able to save if files already exist,
pick a different folder or filename in that case.

```python
# Save JSON files
book.store.save("./entries.json")
book.balances.save("./end_balances.json")
```

## Limitations

Several assumptions and simplifications are used to make `abacus-minimal`
easier to develop and reason about.

The key assumptions are:

- one currency,
- unique account names,
- one level of accounts in chart,
- no intermediate accounts,
- no treatment of other comprehensive income,
- no changes in equity and cash flow statements (at least yet).

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

I use `poetry` as a package manager, but heard good things about `uv` that I want to try.

## Changelog

- `0.11.0` for this release version `0.10.4` is candidate.
- `0.10.4` (2024-10-27) Handles income statement and balances sheet before and after close.
- `0.10.0` (2024-10-24) Separates core, chart, entry and book code and tests.

## Roadmap

### For cleanup

- [x] `Chart.current_earnings` attribute
- [x] `book.income_statement` that works before and after period close
- [x] `book.balance_sheet` with `current_earnings` account when not closed
- [x] dedupulicate `Book.open()`
- [x] cleaner `BalancesDict`
- [x] reorder tests in `test_book.py`, use assert's from README

### New features

- [ ] `Book.increase()` and `Book.decrease()` methods
- [ ] `Entry.explain()` method

## Using `abacus-minimal` upstream

`abacus-minimal` can run:

- CLI tools similar to [abacus-py][cli],
- online accounting simulators similar to [abacus-streamlit][app], and
- may allow conversions between charts of accounts as requested in [#4][ras].

[cli]: https://github.com/epogrebnyak/abacus
[app]: https://abacus.streamlit.app/
[ras]: https://github.com/epogrebnyak/abacus/issues/4
