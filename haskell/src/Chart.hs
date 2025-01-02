module Chart where 

import Types
import qualified Data.Map as Map

-- Create an empty T-account
emptyAccount :: Side -> TAccount
emptyAccount side = TAccount side 0 0

-- Determine normal side (debit or credit) for a T5 account type
which :: T5 -> Side
which Asset = Debit
which Expense = Debit
which _ = Credit

-- Reverse side
revert :: Side -> Side -- Q: is revert good name?
revert Debit = Credit
revert Credit = Debit

-- Aliases for ChartItem constructors
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
account t name contraNames = Add t name : offset name contraNames

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
    Just (Contra cname) -> revert <$> whichSide cname chartMap
    Nothing -> Nothing

-- Define empty ChartMap
emptyChartMap :: ChartMap
emptyChartMap = Map.empty

-- List all accounts from chart by specific type
byType :: ChartMap -> T5 -> [Name]
byType chartMap t = Map.keys $ Map.filter (== Regular t) chartMap

-- Return list of contra accounts for a specific regular account 
contras :: ChartMap -> Name -> [Name] 
contras chartMap name =
    case Map.lookup name chartMap of
        Just (Regular _) -> Map.keys $ Map.filter (== Contra name) chartMap
        _ -> []

-- Create a tuple with closeTo as second item
toPair :: Name -> Name -> (Name, Name)
toPair closeTo name = (name, closeTo) 

-- Return all contra accounts for a specific account type
-- May reuse for netting of permanent accounts
contraPairs :: ChartMap -> T5 -> [(Name, Name)]
contraPairs chartMap t = byType chartMap t >>= pairs 
   where pairs name = toPair name <$> contras chartMap name

-- Create complete list of closing pairs 
closingPairs :: ChartMap -> Name -> [(Name, Name)]
closingPairs chartMap accName = f Expense ++ f Income -- may change order
    where 
        accumulationPairs :: T5 -> [(Name, Name)]
        accumulationPairs t = toPair accName <$> byType chartMap t
        f t = contraPairs chartMap t ++ accumulationPairs t                 
 