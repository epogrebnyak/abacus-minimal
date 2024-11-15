# Roadmap

## Using upstream

Implanting `abacus-minimal` as a dependency to:

- [ ] [abacus-py][cli],
- [ ] [abacus-streamlit][app].

## New features

- [ ] Business event layer `Event("Invoice", net_amount=200, vat=40)`
- [ ] `Book.increase()` and `Book.decrease()` methods
- [ ] `Entry.explain()` method

## Application ideas

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
