module Chart where 

import Bases
import qualified Data.Map as Map

-- Create an empty T-account
emptyAccount :: Side -> TAccount
emptyAccount side = (side, 0)

-- Determine normal side (debit or credit) for a T5 account type
which :: T5 -> Side
which Asset = Debit
which Expense = Debit
which _ = Credit

-- Reverse side
revert :: Side -> Side -- is revert good enough of a name?
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
        
-- Code under test
f :: Maybe Side
f = let chart = dispatchMany (account Income "sales" ["refunds"]) emptyChartMap 
   in whichSide "refunds" chart  
