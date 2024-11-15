

## End-to-end example

In this example we use more features including:

- contra accounts specification,
- opening ledger with account balances at the start of reporting period,
- posting multiple entries and using various types of syntax for an entry,
- showing proxy income statement and balance sheet before closing,
- saving and loading data to JSON files.

The complete code is in [readme.py](examples/readme.py).

### 1. Create chart of accounts

Steps involved:

- specify names of the current earnings and retained earnings accounts,
- add account names for assets, equity, liabilities, income and expenses,
- add contra accounts (e.g. `refunds` is a contra account to `sales`).

Code example:

```python
from abacus import Chart

chart = Chart(
    retained_earnings="retained_earnings",
    current_earnings="current_earnings",
    assets=["cash", "ar"],
    equity=["equity"],
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
assert book.balance_sheet.equity["current_earnings"] == 3000

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

You can save the list of entries and the period end account balances to JSON files, unless these files already exist (save files to a different folder or under a different name in that case).

```python
# Save JSON files
book.store.save("./entries.json")
book.balances.save("./end_balances.json")
```

## Architecture

### Core library

In `abacus-minimal` there is a small core library that consists of:

- `ChartDict` maps account names to their types,
- `Ledger` class maps account names to debit normal and credit normal T-accounts, and
- `Posting` type that represents a double or a multiple entry.

`Ledger` is created from `ChartDict` and incoming entries change the state of ledger.
`ChartDict` allows to create closing entries at accounting period end.
Trial balance, account balances, balance sheet and income statement reports
reflect the state of ledger.

### User interface

As a user you do not have to interact with the core library directly. 
`abacus-minmal` exports `Chart`, `Entry` and `Book` classes that we used in the examples.
The `Book` class holds together a chart, a store of entries, and a ledger and allows posting entries, closing the accounts, creating reports and saving and loading JSON files.



## Roadmap

### Using upstream

Implanting `abacus-minimal` as a dependency to:

- [ ] [abacus-py][cli],
- [ ] [abacus-streamlit][app].

### New features

- [ ] Business event layer `Event("Invoice", net_amount=200, vat=40)`
- [ ] `Book.increase()` and `Book.decrease()` methods
- [ ] `Entry.explain()` method

### Application ideas

- [ ] real company - eg Walmart accounts
- [ ] business simulation layer - stream of entries
- [ ] more examples from textbooks
- [ ] chart repository and conversions between charts of accounts as requested in [#4][ras].
- [ ] a quiz based on Book class - play entries and expect the use picks correct debit and credit

[cli]: https://github.com/epogrebnyak/abacus
[app]: https://abacus.streamlit.app/
[ras]: https://github.com/epogrebnyak/abacus/issues/4


## Extension ideas

Replacing `History` class with a database connector is a way to use `abacus-minimal` 
for high-load applications that would not fit in memory. 
