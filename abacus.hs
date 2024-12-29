module Accounting where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Control.Monad (foldM)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Control.Monad.Trans.State (StateT(..), runStateT, modify, get)

-- Type Synonyms
type Amount = Int  -- This could be Decimal E2
type Name = String
type Balances = Map.Map Name Amount
data Side = Debit | Credit deriving (Show, Eq)
type TAccount = (Side, Amount)
type AccountMap = Map.Map Name TAccount
data T5 = Asset | Expense | Equity | Liability | Income deriving Show

-- Create an empty T-account
emptyAccount :: Side -> TAccount
emptyAccount side = (side, 0)

-- Determine normal debit or credit for a T5 account type
which :: T5 -> Side
which Asset = Debit
which Expense = Debit
which _ = Credit

-- Reverse side
revert :: Side -> Side
revert Debit = Credit
revert Credit = Debit

-- Chart of accounts
data Role = Regular T5 | Contra Name deriving Show
type ChartMap = Map.Map Name Role
data ChartItem = Add T5 Name | Offset Name Name deriving Show

-- Aliases for ChartItem
assets :: [Name] -> [ChartItem]
assets = map (Add Asset)

capital :: [Name] -> [ChartItem]
capital = map (Add Equity)

expenses :: [Name] -> [ChartItem]
expenses = map (Add Expense)

incomes :: [Name] -> [ChartItem]
incomes = map (Add Income)

liabilities :: [Name] -> [ChartItem]
liabilities = map (Add Liability)

offset :: Name -> [Name] -> [ChartItem]
offset name = map (Offset name)

account :: T5 -> Name -> [Name] -> [ChartItem]
account t name contraNames = [Add t name] ++ offset name contraNames

-- Add ChartItem to Chart
dispatch :: ChartItem -> ChartMap -> ChartMap
dispatch (Add t name) chartMap = Map.insert name (Regular t) chartMap
dispatch (Offset base contra) chartMap = if Map.member base chartMap
    then Map.insert contra (Contra base) chartMap
    else chartMap

-- Add several ChartItems to Chart
dispatchMany :: [ChartItem] -> ChartMap -> ChartMap
dispatchMany xs chartMap = foldl (flip dispatch) chartMap xs

-- Create ChartMap from a list of ChartItem
fromChartItems :: [ChartItem] -> ChartMap
fromChartItems xs = dispatchMany xs emptyChartMap

-- Get side of a given account
whichSide :: Name -> ChartMap -> Maybe Side
whichSide name chartMap = case Map.lookup name chartMap of
    Just (Regular t) -> Just (which t)
    Just (Contra cname) -> fmap revert (whichSide cname chartMap)
    Nothing -> Nothing

-- Define empty ChartMap
emptyChartMap :: ChartMap
emptyChartMap = Map.empty

-- Define Ledger
data Ledger = Ledger ChartMap AccountMap (Maybe AccountMap) deriving Show

-- Create AccountMap from ChartMap using list comprehension
toAccountMap :: ChartMap -> AccountMap
toAccountMap chartMap = Map.fromList [ (name, emptyAccount side) | name <- Map.keys chartMap, Just side <- [whichSide name chartMap] ]

-- Create Ledger
fromChartMap :: ChartMap -> Ledger
fromChartMap chartMap = Ledger chartMap (toAccountMap chartMap) Nothing

-- Create empty Ledger
emptyLedger :: Ledger
emptyLedger = fromChartMap emptyChartMap

-- Define single entry
data SingleEntry = Single Side Name Amount deriving Show
dr :: Name -> Amount -> SingleEntry
dr = Single Debit

cr :: Name -> Amount -> SingleEntry
cr = Single Credit

-- Check if a list of single entries is balanced
isBalanced :: [SingleEntry] -> Bool
isBalanced posts = sideSum Debit posts == sideSum Credit posts

sideSum side posts = sum $ map (getAmount side) posts
    where 
        getAmount side (Single s _ a) = if s == side then a else 0

-- Post single entry to t-account.
post :: Side -> Amount -> TAccount -> TAccount
post side amount (tSide, balance) =
    let newBalance = if side == tSide then balance + amount else balance - amount
    in (tSide, newBalance)

data Error = NotFound [Name] | NotBalanced [SingleEntry]

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


-- Print account names and balances by lines
printAccountBalances :: Ledger -> IO ()
printAccountBalances (Ledger _ accountMap _) = 
    mapM_ print $ mapMaybe getBalance (Map.keys accountMap)
    where
        getBalance :: Name -> Maybe (Name, Amount)
        getBalance name = do
            (side, balance) <- Map.lookup name accountMap
            return (name, balance)

-- Diagnose the Ledger for errors
diagnose :: Either Error Ledger -> IO ()
diagnose (Left (NotFound names)) = putStrLn ("Accounts not found: " ++ show names)
diagnose (Left (NotBalanced posts)) = putStrLn ("Entry not balanced" ++ show posts)
diagnose (Right ledger) = printAccountBalances ledger

-- Sample chart items and ledgers
chartItems :: [ChartItem]
chartItems = assets ["cash"] ++
  capital ["eq"] ++
  account Equity "re" ["ts"] ++
  expenses ["rent", "wages"] ++
  account Income "sales" ["refunds", "voids"]

sampleChart :: ChartMap
sampleChart = fromChartItems chartItems

ledger :: Ledger
ledger = fromChartMap sampleChart

ledger2 :: Either Error Ledger
ledger2 = acceptMany (double "cash" "no_account" 5) ledger

ledger3 :: Either Error Ledger
ledger3 = acceptMany [dr "cash" 1000, cr "eq" 1000] ledger

ledger4 :: Either Error Ledger
ledger4 = acceptMany [dr "cash" 1000, cr "eq" 1001] ledger

-- Stages of accounting cycle and types of entries
data Activity = Opening | Business | Adjustment | Closing | PostClose

-- Irreducible actions that change a ledger
data Primitive = Insert ChartItem |
                 Post SingleEntry |
                 Drop Name |
                 End Activity

data Action = Act [Primitive] |
              Open Balances |
              Enter [SingleEntry] |
              Transfer Name Name |
              CloseTo Name

-- Get the balance of an account
balance :: Name -> AccountMap -> Maybe Amount
balance name accountMap = case Map.lookup name accountMap of
    Just (_, balance) -> Just balance
    Nothing -> Nothing

-- Create a double entry
double :: Name -> Name -> Amount -> [SingleEntry]
double drName crName amount = [Single Debit drName amount, Single Credit crName amount]

-- Transfer balance between accounts
transfer :: Name -> Name -> AccountMap -> Maybe [SingleEntry]
transfer fromName toName accountMap =
    case Map.lookup fromName accountMap of
        Just (side, balance) ->
            if Map.member toName accountMap
            then Just (transferEntry side fromName toName balance)
            else Nothing
        Nothing -> Nothing
    where
        transferEntry :: Side -> Name -> Name -> Amount -> [SingleEntry]
        transferEntry Debit from to = double to from
        transferEntry Credit from to = double from to

-- Main function to demonstrate the creation of a Ledger
main :: IO ()
main = do
    print (whichSide "ts" sampleChart)
    putStrLn "This is incorrect posting:"
    diagnose ledger2
    putStrLn "This is also incorrect posting:"
    diagnose ledger4
    putStrLn "This is correct posting:"
    diagnose ledger3
