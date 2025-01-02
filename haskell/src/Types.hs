module Types where

import qualified Data.Map as Map

-- Basic Types
type Amount = Int  -- This could be Decimal E2
type Name = String

-- Accounting Side
data Side = Debit | Credit deriving (Show, Eq)

-- T-Account
data TAccount = TAccount Side Amount Amount deriving Show

accountBalance :: TAccount -> Amount
accountBalance (TAccount Debit a b) = a - b
accountBalance (TAccount Credit a b) = b - a

-- Account Map
type AccountMap = Map.Map Name TAccount

-- Account Types
data T5 = Asset | Expense | Equity | Liability | Income deriving (Show, Eq)

-- Chart of Accounts
data Role = Regular T5 | Contra Name deriving (Show, Eq)
type ChartMap = Map.Map Name Role

-- Single Entry
data SingleEntry = Single Side Name Amount deriving Show

-- Error Handling
data Error = NotFound [Name] | NotBalanced [SingleEntry] | NotUnique [Name] | NotEquity Name

explain :: Error -> String
explain (NotFound names) = "Accounts not found: " ++ show names
explain (NotBalanced posts) = "Entry not balanced: " ++ show posts
explain (NotUnique names) = "Duplicate account names: " ++ show names
explain (NotEquity name) = "Must be an equity account: " ++ name

-- Ledger
type Balances = Map.Map Name Amount
data Ledger = Ledger {
    chart :: ChartMap,
    accounts :: AccountMap,
    deactivated :: [Name] } deriving Show

-- Accounting Cycle Stages
data Activity = Opening | Business | Adjustment | Closing | PostClose deriving Show

-- Chart Actions
data ChartAction = Account T5 Name
                | Accounts T5 [Name]
                | Account' T5 Name [Name]
                deriving Show

-- Ledger Actions
data LedgerAction = DoubleEntry Name Name Amount
                 | Balanced [SingleEntry]
                 | Close Name
                 | Transfer Name Name
                 | Deactivate Name
                 | Finish Activity
                 deriving Show

-- Actions
data Action = ChartA ChartAction | LedgerA LedgerAction deriving Show

-- Primitives
data Primitive = Add T5 Name
               | Offset Name Name
               | Post SingleEntry
               | Drop Name  -- same as Deactivate
               | End Activity  -- same as Finish
               deriving Show
