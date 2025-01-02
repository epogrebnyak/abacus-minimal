module Ledger where 

import Control.Monad (foldM)
import Chart (emptyChartMap, emptyAccount, closingPairs, playWithThisChart, whichSide)
import Bases

import qualified Data.Map as Map

-- Create AccountMap from ChartMap using do notation
toAccountMap :: ChartMap -> AccountMap
toAccountMap chartMap = Map.fromList $ do
    name <- Map.keys chartMap
    Just side <- [whichSide name chartMap]
    return (name, emptyAccount side)

-- Create Ledger
fromChartMap :: ChartMap -> Ledger
fromChartMap chartMap = Ledger chartMap (toAccountMap chartMap) []

-- Create empty Ledger
emptyLedger :: Ledger
emptyLedger = fromChartMap emptyChartMap

dr :: Name -> Amount -> SingleEntry
dr = Single Debit

cr :: Name -> Amount -> SingleEntry
cr = Single Credit

-- Check if a list of single entries is balanced
isBalanced :: [SingleEntry] -> Bool
isBalanced posts = let f = sideSum posts in f Debit == f Credit

-- Sum amounts of all entries of a given side (debit or credit) 
sideSum :: [SingleEntry] -> Side -> Amount
sideSum posts side = sum [a | Single s _ a <- posts, s == side]

-- Post single entry to t-account.
post :: Side -> Amount -> TAccount -> TAccount
post side amount (TA tSide balance) =
    if side == tSide then TA tSide (balance + amount)
                     else TA tSide (balance - amount)

-- Apply a single entry to the AccountMap
applySingle :: SingleEntry -> AccountMap -> Either Error AccountMap
applySingle (Single side name amount) accountMap = case Map.lookup name accountMap of
    Just tAccount -> Right $ Map.insert name tAccount' accountMap
        where tAccount' = post side amount tAccount
    Nothing -> Left $ NotFound [name]

-- Accept a single entry into the Ledger
acceptSingle :: Ledger -> SingleEntry -> Either Error Ledger
acceptSingle (Ledger chartMap accountMap deactivatedNames) post' =
    case applySingle post' accountMap of
        Left err -> Left err
        Right accountMap' -> Right $ Ledger chartMap accountMap' deactivatedNames

-- Accept multiple entries into the Ledger, entries must be balanced
acceptMany :: [SingleEntry] -> Ledger -> Either Error Ledger
acceptMany posts ledger
    | isBalanced posts = acceptUnbalanced posts ledger
    | otherwise = Left $ NotBalanced posts

-- Accept unbalanced entries into the Ledger
acceptUnbalanced :: [SingleEntry] -> Ledger -> Either Error Ledger
acceptUnbalanced posts ledger = foldM acceptSingle ledger posts

-- Print account names and balances by lines
printAccountBalances :: Ledger -> IO ()
printAccountBalances ledger = mapM_ putStrLn (accountStrings ledger) 
  where 
    accountStrings :: Ledger -> [String]
    accountStrings (Ledger _ accountMap _) = do
        (name, TA _ balance) <- Map.toList accountMap
        return $ name ++ ": " ++ show balance

-- Diagnose the Ledger for errors or print accounts
diagnose :: Either Error Ledger -> IO ()
diagnose (Left e) = putStrLn $ explain e
diagnose (Right ledger) = printAccountBalances ledger

data Compound = Account T5 Name |
                Accounts T5 [Name] |
                Account' T5 Name [Name] |
                OpenWith Balances | 
                DoubleEntry Name Name Amount |
                Balanced [SingleEntry] |
                Close Name | 
                Transfer Name Name | 
                Deactivate Name | 
                End Activity

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
    

data Primitive = Add T5 Name | 
                 Offset Name Name |  
                 Post SingleEntry |
                 Drop Name |   -- same as Deactivate
                 EndP Activity -- same as End

closeActions :: ChartMap -> Name -> Either Error [Compound]
closeActions chartMap accName = 
    case Map.lookup accName chartMap of
        Just (Regular Equity) -> Right $ closingPairs chartMap accName >>= toActions
        Just _ -> Left $ NotEquity accName
        _ -> Left $ NotFound [accName]
    where toActions (fromName, toName) = [Transfer fromName toName, Deactivate fromName]

-- accept ledger and a list containing transfer actions 
-- convert transfer to double and apply this entry to ledger
-- return a new list where transfers are changed to double 
-- Your task create a ledger with sales, refunds, salary and retained earnings
-- to test this action
playWithThisLedger :: Ledger
playWithThisLedger = fromChartMap playWithThisChart

-- Q: how can I execute this main using cabal repl?
main :: IO ()
main = do 
    print playWithThisLedger