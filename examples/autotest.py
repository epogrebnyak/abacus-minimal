# given the syntaxis of this package provide an example of starting a new business
# and the steps to follow to get the business up and running

# Import the package
from abacus import Book, Chart, Entry

# Create a chart of accounts
# use abacus.chart.Chart to create a chart of accounts
chart = Chart(
    current_earnings="profit",
    retained_earnings="re",
    assets=["cash", "inventory"],
    capital=["equity"],
    income=["sales"],
    expenses=["cogs", "sga"],
)
book = Book(chart)
entries = [
    Entry(
        title="Starting a new business",
        debit="cash",
        credit="equity",
        amount=10,
    ),
    # Buy some inventory
    Entry(
        title="Buy inventory",
        debit="inventory",
        credit="cash",
        amount=8,
    ),
    # Sell some inventory
    Entry(
        title="Sell goods",
        debit="cash",
        credit="sales",
        amount=10,
    ),
    # Actually ship goods
    Entry(
        title="Ship goods",
        debit="cogs",
        credit="inventory",
        amount=5,
    ),
    # Pay for shipping and handling
    Entry(
        title="Pay for shipping",
        debit="sga",
        credit="cash",
        amount=1,
    ),
    # Pay for labor
    Entry(
        title="Pay for labor",
        debit="sga",
        credit="cash",
        amount=1,
    ),
    # Pay for utilities
    Entry(
        title="Pay for utilities",
        debit="sga",
        credit="cash",
        amount=1,
    ),
    # Pay for rent
    Entry(
        title="Pay for rent",
        debit="sga",
        credit="cash",
        amount=1,
    ),
    # Pay for insurance
    Entry(
        title="Pay for insurance",
        debit="sga",
        credit="cash",
        amount=1,
    ),
    # Do something else
    Entry(
        title="Do something else",
        debit="sga",
        credit="cash",
        amount=1,
    ),
]
