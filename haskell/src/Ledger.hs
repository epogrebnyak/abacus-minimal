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

update :: Primitive -> State Ledger (Maybe Error)  
update p = do
    chartMap <- gets chart
    accMap <- gets accounts
    names <- gets deactivated
    case p of
        Add t name -> 
            if name `elem` names then return $ Just $ AccountError (Dropped name) 
            else case Map.lookup name chartMap of
                Just _ -> return $ Just $ AccountError (AlreadyExists name)
                Nothing -> do
                    let (chartMap',  accMap') = updateAdd chartMap accMap t name
                    put $ Ledger chartMap' accMap' names
                    return Nothing
        Offset name contraName ->
            if name `elem` names then return $ Just $ AccountError (Dropped name) 
            else if name `notElem` Map.keys chartMap then return $ Just $ AccountError (NotFound name) 
            else if contraName `elem` names then return $ Just $ AccountError (AlreadyExists contraName) 
            else do
                let (chartMap',  accMap') = updateOffset chartMap accMap name contraName
                put $ Ledger chartMap' accMap' names
                return Nothing
        Record (Single side name amount) -> 
            if name `elem` names then return $ Just $ AccountError (Dropped name) 
            else if name `notElem` Map.keys accMap then return $ Just $ AccountError (NotFound name) 
            else do
                let accMap' = Map.insert name (alter side amount (accMap Map.! name)) accMap
                put $ Ledger chartMap accMap' names
                return Nothing
        Drop name ->    
            if name `elem` names then return $ Just $ AccountError (Dropped name) 
            else if name `notElem` Map.keys chartMap then return $ Just $ AccountError (NotFound name) 
            else do
                let names' = name:names
                put $ Ledger chartMap accMap names'
                return Nothing
        Copy -> return Nothing            





-- update (Ledger chartMap accMap names) (Offset name cotraName) = --affects accMap and chartMap
-- update (Ledger chartMap accMap names) (Record (SingleEntry s n a)) = --affects accMap  
-- update (Ledger chartMap accMap names) (Drop name) = --affects names
-- update l@(Ledger chartMap accMap names) Copy = Right l 

-- update ledger (Use chartAction) = Right $ Ledger chartMap' accountMap' names
--     where
--         prims = toPrimitives chartAction 
--         chartMap' = dispatchMany chartMap prims
--         -- update Ledger with new accounts
--         accoutMap' = toAccountMap chartMap'

-- Run a single ledger action
run :: Ledger -> Action -> Either Error Ledger
run (Ledger chartMap accountMap names) (Use chartAction) = 
    Right $ Ledger chartMap' accountMap names
    where
        prims = toPrimitives chartAction 
        chartMap' = dispatchMany chartMap prims
        -- update Ledger with new accounts
        accoutMap' = toAccountMap chartMap'
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
