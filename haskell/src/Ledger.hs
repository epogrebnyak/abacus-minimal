module Ledger where 

import Data.Maybe (mapMaybe)
import Control.Monad (foldM)
import Chart (emptyChartMap, emptyAccount)
import Bases

import qualified Data.Map as Map

-- Create AccountMap from ChartMap using list comprehension
toAccountMap :: ChartMap -> AccountMap
toAccountMap chartMap = Map.fromList [ (name, emptyAccount side) | name <- Map.keys chartMap, Just side <- [whichSide name chartMap] ]

-- Create Ledger
fromChartMap :: ChartMap -> Ledger
fromChartMap chartMap = Ledger chartMap (toAccountMap chartMap) Nothing

-- Create empty Ledger
emptyLedger :: Ledger
emptyLedger = fromChartMap emptyChartMap

dr :: Name -> Amount -> SingleEntry
dr = Single Debit

cr :: Name -> Amount -> SingleEntry
cr = Single Credit

-- Check if a list of single entries is balanced
isBalanced :: [SingleEntry] -> Bool
isBalanced posts = sideSum Debit posts == sideSum Credit posts

-- Sum amounts of all entries of a given side 
sideSum :: Side -> [SingleEntry] -> Amount
sideSum side posts = sum $ map (getAmount side) posts
    where 
        getAmount side (Single s _ a) = if s == side then a else 0

-- Post single entry to t-account.
post :: Side -> Amount -> TAccount -> TAccount
post side amount (tSide, balance) =
    let newBalance = if side == tSide then balance + amount else balance - amount
    in (tSide, newBalance)

-- Apply a single entry to the AccountMap
applySingle :: SingleEntry -> AccountMap -> Either Error AccountMap
applySingle (Single side name amount) accountMap = case Map.lookup name accountMap of
    Just tAccount -> Right $ Map.insert name tAccount' accountMap
        where tAccount' = post side amount tAccount
    Nothing -> Left $ NotFound [name]

-- Accept a single entry into the Ledger
acceptSingle :: SingleEntry -> Ledger -> Either Error Ledger
acceptSingle post (Ledger chartMap accountMap storage) =
    case applySingle post accountMap of
        Left err -> Left err
        Right accountMap' -> Right $ Ledger chartMap accountMap' storage

-- Accept multiple entries into the Ledger
acceptMany :: [SingleEntry] -> Ledger -> Either Error Ledger
acceptMany posts ledger
    | isBalanced posts = acceptUnbalanced posts ledger
    | otherwise = Left $ NotBalanced posts

-- Accept unbalanced entries into the Ledger
acceptUnbalanced :: [SingleEntry] -> Ledger -> Either Error Ledger
acceptUnbalanced posts ledger = foldM (flip acceptSingle) ledger posts

-- Generate a list of strings representing account names and balances
accountBalances :: Ledger -> [String]
accountBalances (Ledger _ accountMap _) =
    mapMaybe getBalanceString (Map.keys accountMap)
    where
        getBalanceString :: Name -> Maybe String
        getBalanceString name = do
            (side, balance) <- Map.lookup name accountMap
            return (name ++ ": " ++ show balance)

-- Print account names and balances by lines
printAccountBalances :: Ledger -> IO ()
printAccountBalances ledger = mapM_ putStrLn (accountBalances ledger)

-- Diagnose the Ledger for errors
diagnose :: Either Error Ledger -> IO ()
diagnose (Left (NotFound names)) = putStrLn ("Accounts not found: " ++ show names)
diagnose (Left (NotBalanced posts)) = putStrLn ("Entry not balanced" ++ show posts)
diagnose (Right ledger) = printAccountBalances ledger
