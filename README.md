# abacus-minimal

![PyPI - Version](https://img.shields.io/pypi/v/abacus-minimal?color=blue)

`abacus-minimal` is an accounting logic library that aims to be concise, correct and expressive in implementation of double entry book-keeping rules.

## Install

```bash
pip install abacus-minimal
```

Latest:

```bash
pip install git+https://github.com/epogrebnyak/abacus-minimal.git
```

## Minimal example

> Start a company with initial shareholder investment (500),
> acquire and then sell stock of merchandise (worth 200 sold for 400)
> and pay staff salaries (150). Demonstrate period earnings are 50.

```python
from abacus import Book, Chart, Entry

chart = Chart(
    retained_earnings="retained_earnings",
    current_earnings="current_earnings",
    assets=["cash", "inventory"],
    capital=["equity"],
    income=["sales"],
    expenses=["salaries", "cogs"],
)
book = Book(chart)
entries = [
    Entry("Initial shareholder funds").debit("cash", 500).credit("equity", 500),
    Entry("Acquired goods").debit("inventory", 200).credit("cash", 200),
    Entry("Accepted payment for goods").debit("cash", 400).credit("sales", 400),
    Entry("Shipped goods").debit("cogs", 200).credit("inventory", 200),
    Entry("Paid salaries").debit("salaries", 150).credit("cash", 150),
]
book.post_many(entries)
book.close()
print(book.income_statement)
print(book.balance_sheet)
# Some checks
assert book.income_statement.net_earnings == 50
assert book.balances == {"cash": 550, "inventory": 0, "equity": 500, "retained_earnings": 50}
```

## Accounting workflow

The steps for using `abacus-minimal` follow the steps of a typical accounting cycle:

- create a chart of accounts,
- open ledger for the current reporting period,
- post account balances for the previous period,
- post entries that reflect business transactions within the period,
- post reconciliation and adjustment entries,
- close accounts at reporting period end,
- make post-close entries,
- show financial reports,
- save account balances data for the next reporting period.

## End-to-end example

In this example we will programmatically run
the accounting workflow within one reporting period
using more `abacus-minimal` features including:

- contra accounts specification,
- opening ledger with account balances at the start of reporting period,
- posting multiple entries and using various types of syntax for an entry,
- showing proxy income statement and balance sheet before closing,
- saving and loading data to JSON files.

The complete code is in [readme.py](examples/readme.py).

### 1. Create chart of accounts

Steps involved:

- specify names of the current earnings and retained earnings accounts,
- add account names for assets, capital, liabilities, income and expenses,
- add contra accounts (e.g. `refunds` is a contra account to `sales`).

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

### 2. Start ledger

Steps involved:

- create a data structure that represents state of accounts (a 'book'),
- record account starting balances from the previous period,

Let's create a book with opening balances known from previous period:

```python
from abacus import Book

book = Book(chart)
opening_balances = {
    "cash": 10_000,
    "equity": 8_000,
    "retained_earnings": 2_000
    }
book.open(opening_balances)
```

At this point the book is ready to record new entries.

### 3. Post entries to ledger

Steps involved:

- record entries that represent business transactions,
- show state of ledger (as trial balance or as account balances) at any time.

Each entry has a title and directions to alter the accounts that are called debits and credits.
The sum of debits should match the sum of credits for a valid entry.
The `Entry` class provides several ways to record the composition of an entry as shown below.

```python
from abacus import Entry

entries = [
    Entry("Invoice with VAT").debit("ar", 6000).credit("sales", 5000).credit("vat_payable", 1000),
    Entry("Cash payment").debit("cash", 6000).credit("ar", 6000),
    Entry("Cashback").double(debit="refunds", credit="cash", amount=500),
    Entry("Paid salaries").set_amount(1500).debit("salaries").credit("cash"),
]

# Post entries to book
book.post_many(entries)
```

Note: there are no reconciliations, adjustments and post-close entries in this example.

### 4. Inspecting ledger

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

### 5. Closing accounts

Closing accounts at period end involves:

- closing contra accounts related to income and expense accounts, and
- closing income and expense accounts to retained earnings.

See section below for code for closing accounts.

Note: account closing was a surprisingly hard part of `abacus-minimal` code that I had to refactor several times.

### 6. Reporting financial statements

Financial reports are typically displayed after account closing,
but proxy reports are available before closing as well:

- **The income statement** will be the same before and after closing.
- **The balance sheet before closing** the will contain current earnings account
  and retained earnings from previous periods.
- **After closing** the current earnings account will be transferred to the retained earnings account and removed from the ledger; it will not appear in the balance sheet.

Expect to see a lot of dictionary-like data structures in code output below:

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

### 7. Saving data for the next period

You can save the list of entries and the period end account balances to JSON files, unless these files already exist.
In that case you will need extra precaution – for example, save files to a different folder or under a different name.

```python
# Save JSON files
book.store.save("./entries.json")
book.balances.save("./end_balances.json")
```

## Architecture

### Core library

There is a small core library that consists of:

- `ChartDict` maps account names to their types,
- `Ledger` class maps account names to debit normal and credit normal T-accounts, and
- `Posting` class that represents a double or a multiple entry.

`Ledger` is created from `ChartDict` and incoming entries change the state of ledger.
`ChartDict` allows to create closing entries at accounting period end.
Trial balance, account balances, balance sheet and income statement reports
reflect the state of ledger.

### User interface

As a user you do not have to interact with the core directly. `abacus-minmal` exports `Chart`, `Entry` and `Book` classes.
The `Book` class holds together a chart, store of entries, and a ledger and allows posting entries, closing the accounts,
creating reports and saving and loading JSON files.

### Limitations

Several assumptions and simplifications are used to make `abacus-minimal` easier to develop and reason about.

The key assumptions are:

- one currency,
- globally unique account names,
- one level of accounts in chart and no account aggregation for reports,
- no intermediate accounts,
- no treatment of other comprehensive income,
- no changes in equity and cash flow statements.

See [core.py](abacus/core.py) module docstring for more details.

## Alternatives

`abacus-minimal` takes a lot of inspiration from the following projects:

- [ledger](https://ledger-cli.org/),
  [hledger](https://github.com/simonmichael/hledger),
  [beancount](https://github.com/beancount/beancount)
  and other [plain text accounting tools](https://plaintextaccounting.org/),
- [medici](https://github.com/flash-oss/medici), a high performance ledger in JavaScript using Mongo database,
- [microbooks](https://microbooks.io/) API and [python-accounting](https://github.com/ekmungai/python-accounting), a production-grade project, tightly coupled to a database.

## Accounting knowledge

If you are totally new to accounting the suggested friendly course is <https://www.accountingcoach.com/>.

ACCA and CPA are the international and the US professional qualifications and IFRS and GAAP are the standards for accounting recognition, measurement and disclosure.

Part B-G in the [ACCA syllabus for the FFA exam](https://www.accaglobal.com/content/dam/acca/global/PDF-students/acca/f3/studyguides/fa-ffa-syllabusandstudyguide-sept23-aug24.pdf) talk about what `abacus-minimal` is designed for.

Tetbooks:

- [list of free and open source textbooks](https://library.sacredheart.edu/opentextbooks/accounting)
- [Frank Wood "Business Accounting"](https://www.google.com/search?q=Frank+Wood+%22Business+Accounting)
- ["200 Years of Accounting History Dates and Events"](https://maaw.info/AccountingHistoryDatesAndEvents.htm)

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

- `0.10.5` (2024-10-27) Handles income statement and balances sheet before and after close.
- `0.10.0` (2024-10-24) Separates core, chart, entry and book code and tests.

## Roadmap

### Using upstream

- [ ] implanting `abacus-minimal` as a dependency to [abacus-py][cli] and [abacus-streamlit][app],
- [ ] allow conversions between charts of accounts as requested in [#4][ras].

### New features

- [ ] Business event layer `Event("Invoice", net_amount=200, vat=40)`
- [ ] `Book.increase()` and `Book.decrease()` methods
- [ ] `Entry.explain()` method

### Application ideas

- [ ] Walmart accounts
- [ ] business simulation layer
- [ ] more examples from textbooks
- [ ] chart repository
- [ ] a quiz based on Book class

[cli]: https://github.com/epogrebnyak/abacus
[app]: https://abacus.streamlit.app/
[ras]: https://github.com/epogrebnyak/abacus/issues/4
