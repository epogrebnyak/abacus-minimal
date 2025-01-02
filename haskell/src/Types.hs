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
data Error = AccountError AccountError | TransactionError TransactionError
data AccountError = NotFound Name | NotUnique Name | NotEquity Name | Dropped Name
data TransactionError = NotBalanced [SingleEntry]

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

data Entry = DoubleEntry Name Name Amount | BalancedEntry [SingleEntry] deriving Show

-- Ledger actions
data Action =   Use ChartAction
              | Post String Entry 
              | Close Name
              | Transfer Name Name
              | Deactivate Name
              | Finish Activity
              | Unsafe [Primitive]
              deriving Show

-- Primitives
data Primitive = Add T5 Name
               | Offset Name Name
               | Record SingleEntry
               | Drop Name  -- same as Deactivate
               | End Activity  -- same as Finish
               deriving Show
