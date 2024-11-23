# abacus-minimal

![PyPI - Version](https://img.shields.io/pypi/v/abacus-minimal?color=blue)

Accounting logic should be possible to express in readable code, right?

`abacus-minimal` aims to be concise, correct and expressive in implementation of double entry book-keeping rules.

Progress and features so far:

- event-based ledger,
- regular and contra accounts,
- multiple entries,
- period end closing,
- income statement and balance sheet reports,
- reports before and after closing,
- saving and loading data from JSON.

## Install

```bash
pip install abacus-minimal
```

<details>
    
Latest:

```bash
pip install git+https://github.com/epogrebnyak/abacus-minimal.git
```

</details>

## Ledger as a sequence of events

`abacus-minimal` provides an accounting ledger that is controlled by
events:

- chart of account changes,
- business transactions, and
- closing entries.

Given a sequence of events you can always recreate the ledger state from scratch.

### Example

In code below account creation, double entries and a command to close
accounts are in the `events` list.

```python
from abacus import Asset, Double, Equity, Expense, Income, Ledger, Close

events = [
    # Create accounts
    Asset("cash"),
    Equity("equity"),
    Equity("re", title="Retained earnings"),
    Income("sales"),
    Expense("salaries"),
    # Post entries
    Double("cash", "equity", 1000),
    Double("cash", "sales", 250),
    Double("salaries", "cash", 150),
    # Close period
    Close(earnings_account="re")
]
ledger = Ledger.from_list(events)
```

Reports reflect the state of ledger:

```python
print(ledger.balances)
print(ledger.income_statement())
print(ledger.balance_sheet())
```

### Primitive and compound events

There are six types of basic, or 'primitive', events in `abacus-minimal`:

1. `Add`: add an account of asset, equity, liability, income or expense type,
2. `Offset`: add a contra account to an existing account,
3. `Debit`: debit an account with amount,
4. `Credit`: credit an account with amount,
5. `Drop`: deactivate an account, and
6. `PeriodEnd`: mark reporting period end.

From example above you can extract the primitives as following:

```python
for p in ledger.history.primitives:
   print(p)
```

There are also compound events. Every compound event
can be represented as a list of primitives. 

<!-- prettier-ignore-start -->

Compound event | What it does                                 | Translates to a list of 
:-------------:|:---------------------------------------------|:-------------
`Account`      | Specifies an account and its contra accounts | `Add` and `Offset`
`Double` and `Multiple` | Accounting entries                  | `Debit` and `Credit`
`Tranfer`      | Move account balance from one account to another | `Double` 
`Close`        | Close accounts at period end                 | `PeriodEnd` and `Transfer`

`Transfer` and `Close` are further processed to primitives

<!-- prettier-ignore-end -->

## Ledger as class

You can also work with higher-level `Chart`, `Book` and `Entry` classes
without looking at events level:

- `Chart` holds a chart of accounts and can be saved and loaded from JSON (`chart.json`).
- `Book` is created from chart, accepts entries to post to ledger and
  also can be saved and loaded from JSON (`history.json`).
- `Entry` is a posting to a ledger that can be a double or a multiple entry.

### Example

Consider an example where you need to process the following transactions:

- a company gets €1000 equity investment from shareholders,
- bills a client €1000 plus 20% [value added tax (VAT)][vat],
- receives €600 installment payment,
- makes a €150 refund,
- pays €450 in salaries to the staff.

[vat]: https://taxation-customs.ec.europa.eu/taxation/vat_en

```python
from abacus import Book, Chart, Entry

# Create chart and ledger
chart = Chart(
    assets=["cash", "ar"],
    equity=["equity"],
    liabilities=["tax_due"],
    income=["services"],
    expenses=["salaries"],
    contra_accounts={"services": ["refunds"]},
    retained_earnings="retained_earnings",
    current_earnings="current_earnings")
book = Book.from_chart(chart)

# Post entries
entries = [
    Entry("Shareholder investment").double("cash", "equity", 1000),
    Entry("Invoiced services")
       .debit("ar", 1200)
       .credit("services", 1000)
       .credit("tax_due", 200),
    Entry("Accepted payment").double("cash", "ar", 600),
    Entry("Made refund").double("refunds", "cash", 150),
    Entry("Paid salaries").double("salaries", "cash", 450),
]
book.post_many(entries)
print(book.balances)

# Close the period and show reports
book.close()
print(book.income_statement)
print(book.balance_sheet)
```

## Everything as JSON

All data structures used are serialisable. You can write code to create a chart of accounts and a ledger, save them to JSONs or pick up data from the JSON files and restore the ledger.

```python
# Save
book.save("chart.json", "history.json", allow_overwrite=True)

# Load and re-enter
book2 = Book.load_unsafe("chart.json", "history.json")

from pprint import pprint
pprint(book2) # may not fully identical to `book` yet
```

## Accounting concepts and workflow

### Key concepts

In `abacus-minimal`:

- there are regular accounts of five types (asset, liability, equity, income, expense);
- contra accounts to regular accounts are possible (eg depreciation, discounts);
- period end closes temporary accounts (income, expense and their associated contra accounts);
- balance sheet and income statement are available before and after close;
- post close entries are allowed on permanent accounts;

See a detailed list of assumptions and limitations [here](prose/assumptions.md).

### Workflow

The steps for using `abacus-minimal` follow the steps of a typical accounting cycle:

- create a chart of accounts,
- open ledger for the current reporting period,
- post account balances from the previous period,
- post entries that reflect business transactions within the period,
- post reconciliation and adjustment entries,
- close accounts at reporting period end,
- post entries after close,
- show financial reports,
- save account balances data for the next reporting period.

### Accounting identity

`abacus-minimal` adheres to the following interpretation of accounting identity.

1. The value of company property, or assets, equals to shareholder and creditor claims on the company:

```
Assets = Equity + Liabilities
```

Equity is the residual claim on assets after creditors:

```
Equity = Assets - Liabilities
```

2. Company current earnings, or profit, is equal to income less expenses associated with generating this income:

```
Current Earnings = Income - Expenses
```

Current earnings accumulate to retained earnings:

```
Retained Earnings = Retained Earnings From Previous Period + Current Earnings
```

3. Equity consists of shareholder equity, other equity accounts and
   retained earnings:

```
Equity = Shareholder Equity + Other Equity + Retained Earnings
```

4. Substituting we get a form of extended accounting equation:

```
Assets + Expenses =
   Shareholder Equity + Other Equity + Retained Earnings + Income + Liabilities
```

5. We also add contra accounts:

```
Assets + Expenses + Contra Accounts To Equity, Income and Liabilitites =
   Shareholder Equity + Other Equity + Retained Earnings + Income + Liabilities
   + Contra Accounts to Assets and Expenses
```

Our book-keeping goal is to reflect business events as changes to the variables
in this eqaution while maintaining the balance of it.

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

ACCA and CPA are the international and the US professional qualifications and IFRS and US GAAP are the standards for accounting recognition, measurement and disclosure.

A great overview of accounting concepts is at  
[IFRS Conceptual Framework for Financial Reporting]
(https://www.ifrs.org/content/dam/ifrs/publications/pdf-standards/english/2021/issued/part-a/conceptual-framework-for-financial-reporting.pdf).

Part B-G in the [ACCA syllabus for the FFA exam](https://www.accaglobal.com/content/dam/acca/global/PDF-students/acca/f3/studyguides/fa-ffa-syllabusandstudyguide-sept23-aug24.pdf) talk about what `abacus-minimal` is designed for.

Textbooks and articles:

1. [list of free and open source textbooks](https://library.sacredheart.edu/opentextbooks/accounting)
2. [Frank Wood "Business Accounting"](https://www.google.com/search?q=Frank+Wood+%22Business+Accounting)
3. ["200 Years of Accounting History Dates and Events"](https://maaw.info/AccountingHistoryDatesAndEvents.htm)

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

- `0.14.2` (2024-11-23) Added `just sql` command to run database examples.
- `0.14.0` (2024-11-15) Event-based ledger now on `main`. Mixed test suite and `pyright`.
- `0.13.0` (2024-11-15) Event-based ledger will become next minor version.
- `0.12.0` (2024-11-13) `events.py` offers events-based ledger modification.
- `0.11.1` (2024-11-06) `abacus.core` now feature complete.
- `0.10.7` (2024-11-02) `Posting` type is a list of single entries.
- `0.10.5` (2024-10-27) Handles income statement and balances sheet before and after close.
- `0.10.0` (2024-10-24) Separates core, chart, entry and book code and tests.
