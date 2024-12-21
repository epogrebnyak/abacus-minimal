import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Control.Monad (foldM)

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

-- Aliases for ChartItem in point-free form
assets = map (Add Asset)
capital = map (Add Equity)
expenses = map (Add Expense)
incomes = map (Add Income)
liabilities = map (Add Liability)
offset name = map (Offset name)

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

type Single = (Side, Name, Amount)
data Entry = Double Name Name Amount | Multiple [Single] deriving Show

toSingles :: Entry -> [Single]
toSingles (Multiple posts) = posts
toSingles (Double debitName creditName amount) = [(Debit, debitName, amount), (Credit, creditName, amount)]
  
isBalanced :: [Single] -> Bool
isBalanced posts = (total Debit == total Credit)
    where
        amounts s1 (s2, _, a) = (if s1 == s2 then a else 0)
        total side = sum $ map (amounts side) posts

post :: Side -> Amount -> TAccount -> TAccount
post Debit amount (Debit, balance) = (Debit, balance + amount)
post Credit amount (Debit, balance) = (Debit, balance - amount)
post Debit amount (Credit, balance) = (Credit, balance - amount)
post Credit amount (Credit, balance) = (Credit, balance + amount)

applySingle :: Single -> AccountMap -> Either Single AccountMap
applySingle (side, name, amount) accountMap = case Map.lookup name accountMap of
    Just tAccount -> Right $ Map.insert name (post side amount tAccount) accountMap
    Nothing -> Left (side, name, amount)

-- Affects only the account map, maybe should use Lens or State
acceptSingle :: Ledger -> Single -> Either Single Ledger
acceptSingle (Ledger chartMap accountMap storage) post =
    let newAccountMap = applySingle post accountMap in
    case newAccountMap of
        Left post -> Left post
        Right accountMap -> Right $ Ledger chartMap accountMap storage

acceptMany :: Ledger -> [Single] -> ([Single], Ledger)
acceptMany ledger posts
    | isBalanced posts = acceptUnbalanced ledger posts
    | otherwise = (posts, ledger)

acceptUnbalanced :: Ledger -> [Single] -> ([Single], Ledger)
acceptUnbalanced ledger posts = foldl next ([], ledger) posts
    where
        next (failed, ledger) post =
            case (acceptSingle ledger post) of
                Left post_ -> (post_ : failed, ledger)
                Right ledger_ -> (failed, ledger_)

postEntry :: Entry -> Ledger -> Either [Single] Ledger
postEntry entry ledger 
    | null failed = Right ledger_
    | otherwise = Left failed
  where
    (failed, ledger_) = acceptMany ledger (toSingles entry)
        
-- Print account names and balances each on new line
printAccountBalances :: Ledger -> IO ()
printAccountBalances (Ledger chartMap accountMap storage) = do
    let names = Map.keys accountMap
    let balances = mapMaybe (\name -> fmap (\(_, balance) -> (name, balance)) (Map.lookup name accountMap)) names
    mapM_ print balances

diagnose :: Either [Single] Ledger -> IO ()
diagnose (Left posts) = putStrLn ("Unable to post: " ++ show posts)
diagnose (Right ledger) = printAccountBalances ledger

chartItems = assets ["cash"] ++
  capital ["eq", "re"] ++
  [Offset "re" "ts"] ++
  expenses ["rent", "wages"] ++
  incomes ["sales"] ++
  offset "sales" ["refunds", "voids"]
sampleChart = fromChartItems chartItems
ledger = fromChartMap sampleChart
ledger2 = postEntry (Double "cash" "brrr" 5) ledger
ledger3 = postEntry (Multiple [(Debit, "cash", 1000), (Credit, "eq", 1000)]) ledger


-- Stages of accounting cycle and types of entries
data Activity = Opening | Business | Adjustment | Closing | PostClose

-- Irreducible actions that can be performed on a ledger
data Primitive = Insert ChartItem | 
                 Post Single |
                 Drop Name |
                 End Activity 

data Action = Act [Primitive] |
              Open Balances | 
              Enter Entry | 
              Transfer Name Name | 
              CloseTo Name 
              
balance :: Name -> AccountMap -> Maybe Amount
balance name accountMap = case Map.lookup name accountMap of
    Just (_, balance) -> Just balance
    Nothing -> Nothing


transfer :: Name -> Name -> AccountMap -> Maybe Double
transfer fromName toName accountMap = 
    let fromAccount = Map.lookup from accountMap in
    case fromAccount of
        Just (side, balance) -> 
            case Map.member toName accountMap of
                True -> Just (transferEntry side balance fromName toName)
                False -> Nothing
        Nothing -> Nothing
    where     
        transferEntry side balance from to = 
            case side of
                Debit -> Double to from balance
                Credit -> Double from to balance

convert :: Action -> Ledger -> [Action]
convert (Open balances) ledger = [] 
convert (CloseTo name) legder = [End Adjustment]
convert (Transfer from to) (Ledger _ accountMap _) =
    case (transfer from to accountMap) of
        Just double -> [Enter double, Drop from]
        Nothing -> []
convert a _ = [a]

apply :: Action -> Ledger -> Either String Ledger
apply (Open balances) _ = Left "Not implemented" 
apply (Enter entry) ledger = let res = postEntry entry ledger in 
    case res of
        Left posts -> Left ("Unable to post: " ++ show posts)
        Right ledger -> Right ledger
apply (Transfer from to) ledger = Right ledger -- postEntry (Double from to 0) ledger
apply (Drop name) (Ledger chartMap accountMap storage) = 
    case Map.lookup name accountMap of
        Just _ -> Right $ Ledger chartMap (Map.delete name accountMap) storage
        Nothing -> Left ("Account '" ++ name ++ "' not found ledger")
apply (CloseTo name) ledger = Left "I am not ready for this yet."
apply (End Adjustment) (Ledger chartMap accountMap _) = Right (Ledger chartMap accountMap (Just accountMap)) 
apply (End _) ledger = Right ledger
apply (Sequence actions) ledger = Right ledger
apply _ _ = Left "Not implemented"

-- Main function to demonstrate the creation of a Ledger
main :: IO ()
main = do
    print (whichSide "ts" sampleChart)
    print sampleChart
    putStrLn "This is incorrect posting:"
    diagnose ledger2
    putStrLn "This is correct posting:"
    diagnose ledger3
