module Bases where

import qualified Data.Map as Map

type Amount = Int  -- This could be Decimal E2
type Name = String
data Side = Debit | Credit deriving (Show, Eq)
type TAccount = (Side, Amount)
type AccountMap = Map.Map Name TAccount
data T5 = Asset | Expense | Equity | Liability | Income deriving Show

-- Chart of accounts  
data Role = Regular T5 | Contra Name deriving Show
type ChartMap = Map.Map Name Role
data ChartItem = Add T5 Name | Offset Name Name deriving Show

-- Define single entry 
data SingleEntry = Single Side Name Amount deriving Show

-- Abacus error
data Error = NotFound [Name] | NotBalanced [SingleEntry] | NotUnique [Name]

-- Ledger
type Balances = Map.Map Name Amount
