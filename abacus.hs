import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

type Amount = Int  -- This could be Decimal E2
type Name = String
type Balances = Map.Map Name Amount
data Side = Debit | Credit deriving Show
data TAccount = TAccount Side Amount deriving Show

emptyTAccount :: Side -> TAccount
emptyTAccount side = TAccount side 0

type AccountMap = Map.Map Name TAccount
data T5 = Asset | Expense | Equity | Liability | Income deriving Show

-- Determine normal debit or credit for a T5 account type
which :: T5 -> Side
which Asset = Debit
which Expense = Debit
which _ = Credit

-- Reverse the Side
revert :: Side -> Side
revert Debit = Credit
revert Credit = Debit

-- Chart of accounts
data Role = Regular T5 | Contra Name deriving Show
type ChartMap = Map.Map Name Role

-- Insert a regular account into ChartMap
regular :: Name -> T5 -> ChartMap -> ChartMap
regular name t chartMap = Map.insert name (Regular t) chartMap

-- Insert a contra account into ChartMap only if base name exists
offset :: Name -> Name -> ChartMap -> ChartMap
offset basename contra chartMap
    | Map.member basename chartMap = Map.insert contra (Contra basename) chartMap
    | otherwise = chartMap

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
toAccountMap chartMap = Map.fromList [ (name, emptyTAccount side) | name <- Map.keys chartMap, Just side <- [whichSide name chartMap] ]

-- Create Ledger
fromChartMap :: ChartMap -> Ledger
fromChartMap chartMap = Ledger chartMap (toAccountMap chartMap) Nothing

-- Create empty Ledger
emptyLedger :: Ledger
emptyLedger = fromChartMap emptyChartMap

-- Example chart map
sample :: ChartMap
sample = offset "re" "ts" (regular "re" Equity emptyChartMap)

-- Example data
data ChartItem = Add T5 Name | Offset Name Name | Account T5 Name [Name] deriving Show

assets = map (Add Asset) 
capital = map (Add Equity)
expenses = map (Add Expense)
incomes = map (Add Income)
liabilities = map (Add Liability)

toChartPrimitives :: ChartItem -> [ChartItem]
toChartPrimitives (Account t name contras) = (Add t name) : (map (Offset name) contras)
toChartPrimitives ci = [ci]

chart = assets ["cash"] ++ 
  capital ["eq", "re"] ++ 
  expenses ["rent", "wages"] ++ 
  [Account Income "sales" ["refunds", "voids"]]

-- fromChartPrimitives :: [ChartItem] -> ChartMap
-- fromChartPrimitives xs = 
       
--     where items = concat (map toChartPrimitives xs) 






-- Main function to demonstrate the creation of a Ledger
main :: IO ()
main = do
    putStrLn (show chart)
    let ledger = fromChartMap sample
    putStrLn (show (whichSide "ts" sample))
    putStrLn "This is ledger:"
    putStrLn (show ledger)