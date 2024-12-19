import qualified Data.Map as Map

-- Type aliases for clarity
type Amount = Int  -- This could be Decimal E2 for more precision
type Name = String
type Balances = Map.Map Name Amount
data Side = Debit | Credit deriving Show
data TAccount = TAccount Side Amount deriving Show

emptyT :: Side -> TAccount
emptyT side = TAccount side 0

type AccountMap = Map.Map Name TAccount
data T5 = Asset | Expense | Equity | Liability | Income deriving Show


-- Chart of accounts
data Role = Regular T5 | Contra Name deriving Show
type ChartM = Map.Map Name Role

regular :: Name -> T5 -> ChartM -> ChartM
regular name t = Map.insert name (Regular t)

offset :: Name -> Name -> ChartM -> ChartM
offset name contra = Map.insert contra (Contra name) 

ask :: Name -> ChartM -> Maybe Side
ask name cm = case Map.lookup name cm of
    Just (Regular t) -> Just (which t)
    Just (Contra name) -> revert <$> ask name cm
    Nothing -> Nothing

z = offset "re" "ts" (emptyM "re")

data Chart = Chart Name ChartM deriving Show
data Ledger = Ledger Chart AccountMap (Maybe AccountMap) deriving Show

-- Create an empty Chart with given retained earnings account name
emptyM :: Name -> ChartM
emptyM re = regular re Equity Map.empty

emptyChart :: Name -> Chart
emptyChart re = Chart re (emptyM re)

which :: T5 -> Side
which Asset = Debit
which Expense = Debit
which _ = Credit

revert :: Side -> Side
revert Debit = Credit
revert Credit = Debit

-- This needs a fix
toAccountMap :: ChartM -> AccountMap
toAccountMap cm = Map.fromList [(name, fromRole role) | (name, role) <- Map.toList cm]
  where
    fromRole role = case role of
      Regular t -> Just (which t)
      Contra contraName -> case ask contraName cm of
        Just side -> Just (revert side)
        Nothing -> Nothing

-- Create an empty Ledger with the given retained earnings name
emptyLedger :: Name -> Ledger
emptyLedger re = Ledger (Chart re e) (toAccountMap e) Nothing
    where 
        e = emptyM re

-- Main function to demonstrate the creation of a Ledger
main :: IO ()
main = do
    let ledger = emptyLedger "re"
    putStrLn (show (ask "ts" z))
    putStrLn "This is ledger:"
    putStrLn (show ledger)

-- Event data type (placeholder for future implementation)
-- data Event = Set T5 Name | Offset Name Name | Add Side Name Amount | Drop Name | End
