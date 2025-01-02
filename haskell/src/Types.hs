module Types where

import qualified Data.Map as Map

type Amount = Int  -- This could be Decimal E2
type Name = String
data Side = Debit | Credit deriving (Show, Eq)
data TAccount = TA Side Amount deriving Show
type AccountMap = Map.Map Name TAccount
data T5 = Asset | Expense | Equity | Liability | Income deriving (Show, Eq)

-- Chart of accounts  
data Role = Regular T5 | Contra Name deriving (Show, Eq)
type ChartMap = Map.Map Name Role
data ChartItem = Add T5 Name | Offset Name Name deriving (Show, Eq)

-- Define single entry 
data SingleEntry = Single Side Name Amount deriving Show

-- Abacus package error
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

-- Stages of accounting cycle and types of entries
data Activity = Opening | Business | Adjustment | Closing | PostClose deriving Show

data Compound = Account T5 Name |
                Accounts T5 [Name] |
                Account' T5 Name [Name] |
                OpenWith Balances | 
                DoubleEntry Name Name Amount |
                Balanced [SingleEntry] |
                Close Name | 
                Transfer Name Name | 
                Deactivate Name | 
                End Activity deriving Show

data Primitive = Use ChartItem |  
                 Post SingleEntry |
                 Drop Name |   -- same as Deactivate
                 EndP Activity -- same as End 
                 deriving Show
