### Assumptions

Several assumptions and simplifications are used to make `abacus-minimal` easier to develop and reason about. Some of them are here stay, some may be relaxed in future versions.

General:

1. one currency
1. one reporting period
1. one reporting entity

Chart of accounts:

1. one level of accounts, no sub-accounts, no account aggregation for reports
1. account names must be globally unique (eg cannot have two accounts named "other")
1. chart always has current earnings account and retained earnings account
1. no account durations, current vs non-current accounts not distinguished

Accounts and closing:

1. accounts are either debit normal or credit normal, no mixed accounts
1. no intermediate accounts except current earnings
1. period end closing will transfer current earnings to retained earnings

Entries:

1. no journals, entries are posted to ledger directly
1. an entry can touch any accounts
1. entry amount can be positive, negative or zero
1. no date or any transaction metadata recorded

Valuation changes:

1. valuations changes no recorded
1. other comprehensive income account (OCIA) not calculated

Reporting:

1. net earnings are income less expenses, no gross profit or earnings before tax calculated
1. no cash flow statement
1. no statement of changes in equity
