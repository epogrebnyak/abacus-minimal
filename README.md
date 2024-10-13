# abacus-minimal

`abacus-minimal` aims to be as concise as possible in implemetation
of double entry book-keeping rules as applied in corporate accounting.

## Workflow

There are three steps involved in using `abacus-minmal` - creating a chart of accounts, 
posting transactions to ledger and reporting financial results.

1. Create chart of accounts:

- specifiy name of the retained earnings account that will accumulate company gains and losses,
- add account names for assets, capital, liabilities, income and expenses,
- add contra accounts (eg refunds is a contra account to sales).

2. Post entries to ledger:

- create a data structure that represents state of accounts (ledger),
- record account starting balances from the previoius period (skip for a new company)
- record accounting entries that represent business transactions,
- show state of ledger (trial balance) at any time.

3. Report financial results for the period:

- make adjustment entries for accruals and deferrals, 
- close temporary accounts to the retained earnings account,
- make post-close entries if applicable (eg dividend payout),
- show balance sheet and income statement.

Key limitations:

- one currency,
- one level of accounts,
- no changes in equity and cash flow statements yet.

Other assumptions and simplifacations listed in the module docstring.

## Install

```bash
git clone https://github.com/epogrebnyak/abacus-minimal.git
cd abacus-minimal
```

## Usage example

```python
from abacus import Book, Chart, Entry

# 1. Create chart of accounts
chart = Chart(
    retained_earnings="retained_earnings",
    assets=["cash"],
    capital=["equity"],
    liabilities=["vat_payable"],
    income=["sales"],
    expenses=["salaries"],
)
chart.offset("sales", "refunds")

# 2. Post entries to ledger
book = Book(chart)
entries = [
    Entry("Initial investment", amount=10_000).debit("cash").credit("equity"),
    Entry("Sold services with VAT").debit("cash", 6000).credit("sales", 5000).credit("vat_payable", 1000),
    Entry("Made client refund", amount=500).debit("refunds").credit("cash"),
    Entry("Paid salaries", amount=1500).debit("salaries").credit("cash"),
]
book.post_many(entries)

# 3. Close at period end and show reports
print(book.income_statement)
book.close()
print(book.balance_sheet)
assert book.ledger.balances == {
    "cash": 14000,
    "equity": 10000,
    "vat_payable": 1000,
    "retained_earnings": 3000,
}

# Save everything to JSON files in current folder
book.save(directory=".")
```

## Project intent

- Make valid accounting engine in fewer lines of code.
- Explain book-keeping rules through code examples.
- Make routes into accounting for programmers and vice versa.
- Curate typical charts of accounts by various country, industry and accounting standards and make conversions between them.
- Make free web learning tools in accounting similar to [abacus-streamlit][ex].

[ex]: https://abacus.streamlit.app/

# Alternatives

`abacus-minimal` takes inspiration from the following great projects:

- [hledger](https://github.com/simonmichael/hledger) and [plain text accounting tools](https://plaintextaccounting.org/),
- [medici](https://github.com/flash-oss/medici) ledger in JavaScript using Mongo database,
- [microbooks](https://microbooks.io/) API.

Plain text accounting tools are ususally for personal finance while `abacus-minimal` 
describes corporate accounting. `medici` is a high perfromance ledger, but does
not enforce the accounting rules on data entry. `microbooks` is one of the first
solo projects to provide accounting as a cloud service, but the code is not open source.        

Big players in accounting software are Intuit Quickbooks and Xero, many other providers 
do also have accounting APIs (eg Zoho). There are open source packages that have accounting 
functionality (eg Frappe).

If you are totally new to accounting the suggested friendly course is <https://www.accountingcoach.com/>.

