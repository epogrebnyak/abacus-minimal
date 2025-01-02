module Chart where 

import Types
import qualified Data.Map as Map

-- Create an empty T-account
emptyAccount :: Side -> TAccount
emptyAccount side = TAccount side 0 0

-- Determine debit or credit side for a T5 account type
which :: T5 -> Side
which Asset = Debit
which Expense = Debit
which _ = Credit

-- Reverse side
toggle :: Side -> Side
toggle Debit = Credit
toggle Credit = Debit

-- Add primitive to chart
dispatch :: Primitive -> ChartMap -> ChartMap
dispatch (Add t name) chartMap = Map.insert name (Regular t) chartMap
dispatch (Offset base contra) chartMap = if Map.member base chartMap
    then Map.insert contra (Contra base) chartMap
    else chartMap
dispatch _ chartMap = chartMap  

-- Add several prims to Chart
dispatchMany :: ChartMap -> [Primitive] -> ChartMap
dispatchMany chartMap = foldl (flip dispatch) chartMap

-- Create ChartMap from a list of prims
fromChartItems :: [Primitive] -> ChartMap
fromChartItems = dispatchMany emptyChartMap

-- Get side of a given account
whichSide :: Name -> ChartMap -> Maybe Side
whichSide name chartMap = case Map.lookup name chartMap of
    Just (Regular t) -> Just (which t)
    Just (Contra cname) -> toggle <$> whichSide cname chartMap
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
contraPairs chartMap t = byType chartMap t >>= (\name -> toPair name <$> contras chartMap name)

-- Return closing pairs for regular account that close to accumulation account
accumulationPairs :: ChartMap -> T5 -> Name -> [(Name, Name)]
accumulationPairs chartMap t accName = toPair accName <$> byType chartMap t

-- Combine contraPairs and accumulationPairs
allPairs :: ChartMap -> Name -> T5 -> [(Name, Name)]
allPairs chartMap accName t = contraPairs chartMap t ++ accumulationPairs chartMap t accName

-- Create complete list of pairs for closing temporary accounts
closingPairs :: ChartMap -> Name -> [(Name, Name)]
closingPairs chartMap accName = [Expense, Income] >>= (allPairs chartMap accName)
