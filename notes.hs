import qualified Data.Map as Map

type Dict k v = Map.Map k v
type Amount = Int  -- can be Decimal E2
type Name = String
type Balances = Dict Name Amount
data Side = Debit | Credit deriving Show
data TAccount = TAccount Side Amount deriving Show
type AccountMap = Dict Name TAccount
data T5 = Asset | Expense | Equity | Liability | Income deriving Show
data Chart = Chart Name (Dict Name T5) (Dict Name Name) deriving Show

emptyChart :: Name -> Chart
emptyChart re = Chart re (Map.fromList [(re, Equity)]) (Map.fromList [])

fromChart :: Chart -> AccountMap
-- TODO: create AccountMap form ledger
fromChart chart = Map.fromList []
data Ledger = Ledger Chart AccountMap (Maybe AccountMap) deriving Show

emptyLedger :: Name -> Ledger
emptyLedger re = let
    chart = emptyChart re
    in Ledger chart (fromChart chart) Nothing

main :: IO ()
main = do
    let ledger = emptyLedger "re"
    putStrLn "This is ledger:"
    putStrLn (show ledger)

-- Event = Set T5 Name | Offset Name Name | Add Side Name Amount | Drop Name | End
