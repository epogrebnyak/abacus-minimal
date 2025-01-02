module Abacus (module Ledger, module Types, module Chart, someFunc, exampleStream) where

import Types
import Chart
import Ledger

exampleStream :: [Compound]
exampleStream = [
    Accounts Asset ["cash", "ap"],
    Account' Equity "eq" ["ts"],      -- "ts" is treasury shares
    Account  Equity "re",             -- "re" is retained earnings account  
    Account' Income "sales" ["voids", "refunds"],
    Account  Expense "salary",
    Accounts Liability ["ap", "dd", "tax"],  
    -- may add OpenWith later here
    DoubleEntry "cash" "eq" 1000,    -- shareholder investment
    Balanced [Single Credit "sales" 500,
              Single Credit "tax" 25,     -- 5% sales tax
              Single Debit "ar" 525],     -- invoiced customer 
    DoubleEntry "refunds" "ar" 25,   -- issued refund 
    DoubleEntry "voids" "ar" 75,     -- voided part of invoice 
    DoubleEntry "salary" "cash" 200, -- paid salaries
    Close "re",                      -- closed period   
    DoubleEntry "re" "dd" 100,       -- accrued dividend
    DoubleEntry "dd" "cash" 100,     -- payed dividend
    DoubleEntry "cash" "ts" 50]      -- bought back shares 

someFunc :: IO ()
someFunc = putStrLn "someFunc"
