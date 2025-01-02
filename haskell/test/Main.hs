module Main (main) where

import Test.HUnit
import Chart
import Bases

chartSample :: ChartMap
chartSample = fromChartItems (account Income "sales" ["voids", "refunds"] ++
                              account Expense "cogs" ["sgoc", "abc"])

-- End-to-end test for chart creation
testWhichSide :: Test
testWhichSide = TestCase $ do
    assertEqual "Refunds is a debit account" (Just Debit) (whichSide "refunds" chartSample)

-- Test capital constructor
testConstructorCapital :: Test
testConstructorCapital = TestCase $ do
    let chartItems = capital ["eq", "re"]
    assertEqual "Capital constructor" [Add Equity "eq", Add Equity "re"] chartItems

-- Note that keys returned should be alphabetically sorted 
testContras :: Test
testContras = TestCase $ do
    assertEqual "Return contra accounts for cogs" ["abc", "sgoc"] (contras chartSample "cogs")

-- Code under test is closingPairs :: ChartMap -> Name -> [(Name, Name)]
testClosingPairs :: Test
testClosingPairs = TestCase $ do
    let ref =  [("abc","cogs"), ("sgoc","cogs"), -- close contra expense
                ("refunds","sales"),("voids","sales"), -- close contra income
                ("cogs","re"), -- close expense to retained earnings 
                ("sales","re")] -- close income to retained earnings 
    assertEqual "List closing pairs for chartSample" ref (closingPairs chartSample "re")

-- Main function to run the test
main :: IO ()
main = do
    _ <- runTestTT testConstructorCapital
    _ <- runTestTT testWhichSide
    _ <- runTestTT testContras
    _ <- runTestTT testClosingPairs
    return ()
