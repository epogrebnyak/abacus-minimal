module Main (main) where

import Test.HUnit
import Abacus

account :: T5 -> Name -> [Name] -> [Primitive]
account t name contraNames = Add t name : map (Offset name) contraNames

-- Chart where contra accounts not listed alphabetically 
chartSample :: ChartMap
chartSample = fromChartItems (account Income "sales" ["voids", "refunds"] ++ -- ... real names
                              account Expense "cogs" ["sgoc", "abc"]) -- ... fictional names

-- End-to-end test for chart creation
testWhichSide :: Test
testWhichSide = TestCase $ do
    assertEqual "Refunds is a debit account" (Just Debit) (whichSide "refunds" chartSample)


-- Contra account names are alphabetically sorted 
testContras :: Test
testContras = TestCase $ do
    assertEqual "Return contra accounts for cogs" ["abc", "sgoc"] (contras chartSample "cogs")

-- Code under test is closingPairs :: ChartMap -> Name -> [(Name, Name)]
testClosingPairs :: Test
testClosingPairs = TestCase $ do
    let ref =  [("abc","cogs"), ("sgoc","cogs"), -- close contra expense
                ("cogs","re"), -- close expense to retained earnings 
                ("refunds","sales"),("voids","sales"), -- close contra income
                ("sales","re")] -- close income to retained earnings 
    assertEqual "List closing pairs for chartSample" ref (closingPairs chartSample "re")

-- Main function to run the test
main :: IO ()
main = do
    _ <- runTestTT testWhichSide
    _ <- runTestTT testContras
    _ <- runTestTT testClosingPairs
    return ()
