import qualified Data.Map as Map

-- Type aliases for clarity
type Amount = Int  -- This could be Decimal E2 for more precision
type Name = String
type Balances = Map.Map Name Amount
data Side = Debit | Credit deriving Show
data TAccount = TAccount Side Amount deriving Show
type AccountMap = Map.Map Name TAccount
data T5 = Asset | Expense | Equity | Liability | Income deriving Show
data Chart = Chart Name (Map.Map Name T5) (Map.Map Name Name) deriving Show
data Ledger = Ledger Chart AccountMap (Maybe AccountMap) deriving Show

-- Create an empty Chart with given retained earnings account name
emptyChart :: Name -> Chart
emptyChart re = Chart re regulars Map.empty
    where regulars = Map.fromList [(re, Equity)] 

which :: T5 -> Side
which Asset = Debit
which Expense = Debit
which _ = Credit

revert :: Side -> Side
revert Debit = Credit
revert Credit = Debit

zero :: Amount
zero = 0

-- Get side of a given account name 
-- getSide :: Chart -> Name -> Maybe Side
-- getSide (Chart _ regulars _) name = case Map.lookup name regulars of
--     Just t5 -> Just $ reverse $ which t5
--    Nothing -> Nothing 

-- TODO: Implement the following function
-- Get list of contra accounts with their sides
-- getListContra :: Chart -> [(Name, Side)]
-- getListContra (Chart _ regulars contras) =
--     [(name, revert $ which t5) | (name, contraName) <- Map.toList contras, Just t5 <- [Map.lookup contraName regulars]]


-- Convert Chart to AccountMap
fromChart :: Chart -> AccountMap
fromChart (Chart _ regulars contras) = Map.fromList $ xs ++ ys
  where
    get = Map.toList
    xs = [(name, TAccount (which t5) zero) | (name, t5) <- get regulars]
    ys = []



-- Create an empty Ledger with the given retained earnings name
emptyLedger :: Name -> Ledger
emptyLedger re = let
    chart = emptyChart re
    in Ledger chart (fromChart chart) Nothing

-- Main function to demonstrate the creation of a Ledger
main :: IO ()
main = do
    let ledger = emptyLedger "re"
    putStrLn "This is ledger:"
    putStrLn (show ledger)

-- Event data type (placeholder for future implementation)
-- data Event = Set T5 Name | Offset Name Name | Add Side Name Amount | Drop Name | End
