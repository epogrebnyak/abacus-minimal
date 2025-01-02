module Ledger where

import qualified Data.Map as Map
import Control.Monad (foldM)

import Chart (emptyChartMap, emptyAccount, closingPairs, whichSide)
import Types

-- Create AccountMap from ChartMap using do notation (similar to list comprehension)
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

-- Check if a list of single entries is balanced
isBalanced :: [SingleEntry] -> Bool
isBalanced posts = let f = sideSum posts in f Debit == f Credit

-- Sum amounts of all entries of a given side (debit or credit)
sideSum :: [SingleEntry] -> Side -> Amount
sideSum posts side = sum [a | Single s _ a <- posts, s == side]

-- Post single entry to t-account.
post :: Side -> Amount -> TAccount -> TAccount
post Debit amount (TAccount side d c)  = TAccount side (d+amount) c
post Credit amount (TAccount side d c) = TAccount side d (c+amount)

-- Apply a single entry to the AccountMap
applySingle :: SingleEntry -> AccountMap -> Either Error AccountMap
applySingle (Single side name amount) accountMap = case Map.lookup name accountMap of
    Just tAccount -> Right $ Map.insert name (post side amount tAccount) accountMap
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
        (name, tAccount) <- Map.toList accountMap
        return $ name ++ ": " ++ show (accountBalance tAccount)

-- Diagnose the Ledger for errors or print accounts
diagnose :: Either Error Ledger -> IO ()
diagnose (Left e) = putStrLn $ explain e
diagnose (Right ledger) = printAccountBalances ledger

-- Close ledger to an equity account
closeActions :: ChartMap -> Name -> Either Error [LedgerAction]
closeActions chartMap accName =
    case Map.lookup accName chartMap of
        Just (Regular Equity) -> Right $ closingPairs chartMap accName >>= toActions
        Just _ -> Left $ NotEquity accName
        _ -> Left $ NotFound [accName]
    where toActions (fromName, toName) = [Transfer fromName toName, Deactivate fromName]

transferEntry :: Name -> Name -> TAccount -> LedgerAction
transferEntry fromName toName (TAccount side a b) =
    case side of
        Debit -> DoubleEntry toName fromName (a-b)
        Credit -> DoubleEntry fromName toName (b-a)

-- Process ledger actions
process :: Ledger -> [LedgerAction] -> Either Error Ledger
process ledger [] = Right ledger
process ledger (l:ls) = do
    ledger' <- run ledger l
    process ledger' ls

-- Run a single ledger action
run :: Ledger -> LedgerAction -> Either Error Ledger
run ledger (DoubleEntry d c a) = run ledger $ Balanced [Single Debit d a, Single Credit c a]
run ledger (Balanced posts) = acceptMany posts ledger
run ledger (Close accName) = do
    actions <- closeActions (chart ledger) accName
    foldM run ledger actions
run ledger (Transfer fromName toName) =
    case Map.lookup fromName (accounts ledger) of
        Just tAccount -> run ledger (transferEntry fromName toName tAccount)
        Nothing -> Left $ NotFound [fromName]
run (Ledger cm am ds) (Deactivate name) = Right $ Ledger cm am (name:ds)
run ledger (Finish _) = Right ledger  -- assuming Finish does not change the ledger


--update :: LedgerAction -> State Ledger (Either Error) -> State Ledger (Either Error)
