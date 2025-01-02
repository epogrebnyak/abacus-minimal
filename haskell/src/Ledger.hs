module Ledger where

import qualified Data.Map as Map
import Control.Monad (foldM)

import Chart (emptyAccount, closingPairs, whichSide)
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
emptyLedger = fromChartMap Map.empty

-- Check if a list of single entries is balanced
isBalanced :: [SingleEntry] -> Bool
isBalanced posts = let f = sideSum posts in f Debit == f Credit

-- Sum amounts of all entries of a given side (debit or credit)
sideSum :: [SingleEntry] -> Side -> Amount
sideSum posts side = sum [a | Single s _ a <- posts, s == side]

-- Post single entry to t-account.
alter :: Side -> Amount -> TAccount -> TAccount
alter Debit amount (TAccount side d c)  = TAccount side (d+amount) c
alter Credit amount (TAccount side d c) = TAccount side d (c+amount)

-- Apply a single entry to the AccountMap
applySingle :: SingleEntry -> AccountMap -> Either Error AccountMap
applySingle (Single side name amount) accountMap = case Map.lookup name accountMap of
    Just tAccount -> Right $ Map.insert name (alter side amount tAccount) accountMap
    Nothing -> Left $ AccountError (NotFound name)

-- Accept a single entry into the Ledger
acceptSingle :: Ledger -> SingleEntry -> Either Error Ledger
acceptSingle (Ledger chartMap accountMap names) s@(Single side name amount) =
    if name `elem` names then Left (AccountError (Dropped name)) else 
    case applySingle s accountMap of
        Left err -> Left err
        Right accountMap' -> Right $ Ledger chartMap accountMap' names

-- Accept multiple entries into the Ledger, entries must be balanced
acceptMany :: [SingleEntry] -> Ledger -> Either Error Ledger
acceptMany posts ledger
    | isBalanced posts = acceptUnbalanced posts ledger
    | otherwise = Left $ TransactionError (NotBalanced posts)

-- Accept unbalanced entries into the Ledger
acceptUnbalanced :: [SingleEntry] -> Ledger -> Either Error Ledger
acceptUnbalanced posts ledger = foldM acceptSingle ledger posts

-- Close ledger to an equity account
closeActions :: ChartMap -> Name -> Either Error [Action]
closeActions chartMap accName =
    case Map.lookup accName chartMap of
        Just (Regular Equity) -> Right $ closingPairs chartMap accName >>= toActions
        Just _                -> Left $ AccountError (NotEquity accName)
        _                     -> Left $ AccountError (NotFound accName)
    where toActions (fromName, toName) = [Transfer fromName toName, Deactivate fromName]

transferEntry :: Name -> Name -> TAccount -> Entry
transferEntry fromName toName (TAccount side a b) =
    case side of
        Debit -> DoubleEntry toName fromName (a-b)
        Credit -> DoubleEntry fromName toName (b-a)

-- Process ledger actions
process :: Ledger -> [Action] -> Either Error Ledger
process ledger [] = Right ledger
process ledger (l:ls) = do
    ledger' <- run ledger l
    process ledger' ls

toBalanced :: Entry -> Entry
toBalanced (DoubleEntry d c a) = BalancedEntry [Single Debit d a, Single Credit c a]
toBalanced x = x

-- Run a single ledger action
run :: Ledger -> Action -> Either Error Ledger
run ledger (Enter d@(DoubleEntry _ _ _)) = run ledger $ Enter (toBalanced d)
run ledger (Enter (BalancedEntry singles)) = acceptMany singles ledger
run ledger (Close accName) = do
    actions <- closeActions (chart ledger) accName
    foldM run ledger actions
run ledger (Transfer fromName toName) =
    case Map.lookup fromName (accounts ledger) of
        Just tAccount -> run ledger $ Enter (transferEntry fromName toName tAccount)
        Nothing -> Left $ AccountError (NotFound fromName)
run (Ledger cm am ds) (Deactivate name) = Right $ Ledger cm am (name:ds)
run ledger (Finish _) = Right ledger  -- assuming Finish does not change the ledger
run ledger (_) = Right ledger  -- other items

-- update :: Action -> State Ledger (Maybe Error) 
