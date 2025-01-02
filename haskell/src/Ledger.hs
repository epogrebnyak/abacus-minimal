module Ledger where

import qualified Data.Map as Map
import Control.Monad (foldM)
import Control.Monad.State (State, gets, put)

import Chart (emptyAccount, dispatchMany, closingPairs, whichSide)
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
acceptSingle (Ledger chartMap accountMap names) s@(Single _ name _) =
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
toBalanced b = b

updateAdd :: ChartMap -> AccountMap -> T5 -> Name -> (ChartMap, AccountMap) 
updateAdd chartMap accMap t name = 
    let chartMap' = Map.insert name (Regular t) chartMap
        accMap' = case whichSide name chartMap' of
            Just side -> Map.insert name (emptyAccount side) accMap
            Nothing -> accMap
    in (chartMap', accMap')

updateOffset :: ChartMap -> AccountMap -> Name -> Name -> (ChartMap, AccountMap)
updateOffset chartMap accMap name contraName = 
    let chartMap' = Map.insert contraName (Contra name) chartMap
        accMap' = case whichSide contraName chartMap' of
            Just side -> Map.insert contraName (emptyAccount side) accMap
            Nothing -> accMap
    in (chartMap', accMap')


data Book = Book {chartM :: ChartMap, ledgerM :: AccountMap, copy :: Maybe AccountMap}

emptyBook :: Book
emptyBook = Book Map.empty Map.empty Nothing  

isRegular :: Role -> Bool
isRegular (Regular _) = True
isRegular _ = False

isMember :: Name -> Map.Map Name a -> Bool
isMember = Map.member

allowOffset :: ChartMap -> Name -> Name -> Maybe AccountError
allowOffset chartMap name contraName  
   | not (isMember name chartMap) = Just $ NotFound name
   | isMember contraName chartMap = Just $ AlreadyExists contraName
   | (isRegular <$> Map.lookup name chartMap) /= Just True = Just $ NotRegular name
   | otherwise = Nothing

update :: Primitive -> State Book (Maybe Error)  
update p = do
    let error e = return $ Just $ AccountError e
    chart <- gets chartM
    ledger <- gets ledgerM
    copy <- gets copy 
    case p of
        Add t name -> 
            case Map.lookup name chart of
                Just _ -> error (AlreadyExists name)
                Nothing -> do
                    let (chart', ledger') = updateAdd chart ledger t name
                    put $ Book chart' ledger' copy
                    return Nothing
        Offset name contraName ->
            case allowOffset chart name contraName of
                Just e -> error e
                Nothing -> do
                  let (chart', ledger') = updateOffset chart ledger name contraName
                  put $ Book chart' ledger' copy
                  return Nothing
        Record (Single side name amount) -> 
            if not (isMember name ledger) then error (NotFound name) 
            else do
                let tAccount' = alter side amount (ledger Map.! name)
                let ledger' = Map.insert name tAccount' ledger
                put $ Book chart ledger' copy
                return Nothing
        Drop name ->    
            if not (isMember name ledger) then error (NotFound name) 
            else do
                let ledger' = Map.delete name ledger
                put $ Book chart ledger' copy
                return Nothing
        Copy -> do 
            put $ Book chart ledger (Just ledger)
            return Nothing            

-- Run a single ledger action
run :: Ledger -> Action -> Either Error Ledger
run (Ledger chartMap _ names) (Use chartAction) = 
    Right $ Ledger chartMap' accountMap' names
    where
        prims = toPrimitives chartAction 
        chartMap' = dispatchMany chartMap prims
        accountMap' = toAccountMap chartMap'
run ledger (Post prose d@(DoubleEntry {})) = run ledger $ Post prose (toBalanced d)
run ledger (Post _ (BalancedEntry singles)) = acceptMany singles ledger
run ledger (Close accName) = do
    actions <- closeActions (chart ledger) accName
    foldM run ledger actions
run ledger (Transfer fromName toName) =
    case Map.lookup fromName (accounts ledger) of
        Just tAccount -> run ledger $ Post "Transfer entry" (transferEntry fromName toName tAccount)
        Nothing -> Left $ AccountError (NotFound fromName)
run (Ledger cm am ds) (Deactivate name) = Right $ Ledger cm am (name:ds)
run ledger (End _) = Right ledger  -- assuming Finish does not change the ledger
run ledger _ = Right ledger  -- other items

-- update :: Action -> State Ledger (Maybe Error) 
