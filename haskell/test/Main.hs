module Main (main) where

-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented."

import Test.HUnit
import Chart
import Bases

-- Define the test case for the function f
testF :: Test
testF = TestCase $ do
    assertEqual "f should return Just Debit" (Just Debit) f

-- Main function to run the test
main :: IO ()
main = do
    _ <- runTestTT testF
    return ()
