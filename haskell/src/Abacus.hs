module Abacus (module Types, 
               module Chart, 
               module Ledger, 
               module Print,
               exampleStream,
               playWithThisChart,               
               playWithThisLedger               
               ) where

import Types
import Chart
import Ledger
import Print

-- Basic chart
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

double :: String -> Name -> Name -> Amount -> Action
double comment d c a = Post comment $ DoubleEntry d c a

balanced :: String -> [SingleEntry] -> Action
balanced comment ss = Post comment $ BalancedEntry ss

close :: Name -> Action
close = Close   

ledgerStream :: [Action] 
ledgerStream = [
    double "Shareholder investment" "cash" "eq" 1000,    
    balanced "Invoice with 5% sales tax" [Single Credit "sales" 500,
              Single Credit "tax" 25,     
              Single Debit "ar" 525],     
    double "Issued partial refund" "refunds" "ar" 25,    
    double "Made partial void" "voids" "ar" 75,     
    double "Paid salaries" "salary" "cash" 200, 
    close "re",                         
    double "Accrued dividend" "re" "dd" 100,         
    double "Paid dividend" "dd" "cash" 100,          
    double "Bought back shares" "cash" "ts" 50]      

exampleStream :: [Action]
exampleStream = map Use chartStream ++ ledgerStream
