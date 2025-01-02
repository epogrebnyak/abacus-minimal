module Abacus (module Types, 
               module Chart, 
               module Ledger, 
               module Print,
               someFunc, 
               exampleStream,
               playWithThisChart,               
               playWithThisLedger               
               ) where

import Types
import Chart
import Ledger
import Print

playWithThisChart :: ChartMap
playWithThisChart = fromChartItems $ [
            Add Asset "cash", 
            Add Equity "equity",
            Add Equity "re",
            Add Expense "salary",
            Add Income "sales", 
            Offset "sales" "refunds"
        ]

playWithThisLedger :: Ledger
playWithThisLedger = fromChartMap playWithThisChart

chartStream :: [ChartAction]
chartStream =  [
    Accounts Asset ["cash", "ap"],
    Account' Equity "eq" ["ts"],      -- "ts" is treasury shares
    Account  Equity "re",             -- "re" is retained earnings account  
    Account' Income "sales" ["voids", "refunds"],
    Account  Expense "salary",
    Accounts Liability ["ap", "dd", "tax"]]

ledgerStream :: [Action] 
ledgerStream = [
    Enter $ DoubleEntry "cash" "eq" 1000,    -- shareholder investment
    Enter $ BalancedEntry [Single Credit "sales" 500,
              Single Credit "tax" 25,     -- 5% sales tax
              Single Debit "ar" 525],     -- invoiced customer 
    Enter $ DoubleEntry "refunds" "ar" 25,   -- issued refund 
    Enter $ DoubleEntry "voids" "ar" 75,     -- voided part of invoice 
    Enter $ DoubleEntry "salary" "cash" 200, -- paid salaries
    Close "re",                      -- closed period   
    Enter $ DoubleEntry "re" "dd" 100,       -- accrued dividend
    Enter $ DoubleEntry "dd" "cash" 100,     -- payed dividend
    Enter $ DoubleEntry "cash" "ts" 50]      -- bought back shares 

exampleStream :: [Action]
exampleStream = map Use chartStream ++ ledgerStream

someFunc :: IO ()
someFunc = putStrLn "someFunc"
