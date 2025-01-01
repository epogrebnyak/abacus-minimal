module Main (main) where

import Test.HUnit
import Chart
import Bases

-- End-to-end test for chart creation
testWhichSide :: Test
testWhichSide = TestCase $ do
    let chartMap = fromChartItems (account Income "sales" ["refunds", "voids"])
    assertEqual "Refunds is a debit account" (Just Debit) (whichSide "refunds" chartMap)

-- Test capital constructor
testConstructorCapital :: Test
testConstructorCapital = TestCase $ do
    let chartItems = capital ["eq", "re"]
    assertEqual "Capital constructor" [Add Equity "eq", Add Equity "re"] chartItems

-- Main function to run the test
main :: IO ()
main = do
    _ <- runTestTT testConstructorCapital
    _ <- runTestTT testWhichSide
    return ()
