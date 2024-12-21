import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

type Amount = Int  -- This could be Decimal E2
type Name = String
type Balances = Map.Map Name Amount
data Side = Debit | Credit deriving (Show, Eq)
data TAccount = TAccount Side Amount deriving Show

emptyAccount :: Side -> TAccount
emptyAccount side = TAccount side 0

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
data ChartItem = Add T5 Name | Offset Name Name deriving Show

-- Aliases for ChartItem in point-free form 
assets = map (Add Asset) 
capital = map (Add Equity)
expenses = map (Add Expense)
incomes = map (Add Income)
liabilities = map (Add Liability)
offset name = map (Offset name)

-- Add ChartItem to Chart as regular or contra account
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

data Post = Post Side Name Amount deriving Show
data Entry = Double Name Name Amount deriving Show
data Posting = D Name Name Amount | Multiple [Post] deriving Show

isBalanced :: [Post] -> Bool
isBalanced posts = (total Debit == total Credit)
    where
        amounts s1 (Post s2 _ a) = if s1 == s2 then a else 0
        total side = sum $ map (amounts side) posts

post :: Side -> Amount -> TAccount -> TAccount
post Debit amount (TAccount Debit balance) = TAccount Debit (balance + amount)
post Credit amount (TAccount Debit balance) = TAccount Debit (balance - amount)
post Debit amount (TAccount Credit balance) = TAccount Credit (balance - amount)
post Credit amount (TAccount Credit balance) = TAccount Credit (balance + amount)

applyPost :: Post -> AccountMap -> Either Post AccountMap 
applyPost (Post side name amount) accountMap = case Map.lookup name accountMap of
    Just tAccount -> Right $ Map.insert name (post side amount tAccount) accountMap
    Nothing -> Left (Post side name amount)

acceptPost :: Ledger -> Post -> Either Post Ledger
acceptPost (Ledger chartMap accountMap storage) post = 
    let newAccountMap = applyPost post accountMap in
    case newAccountMap of
        Left post -> Left post
        Right accountMap -> Right $ Ledger chartMap accountMap storage

acceptMany :: Ledger -> [Post] -> ([Post], Ledger)
acceptMany ledger posts 
    | isBalanced posts = acceptUnbalanced ledger posts
    | otherwise = (posts, ledger)

acceptUnbalanced ::  Ledger -> [Post] -> ([Post], Ledger)
acceptUnbalanced ledger posts = foldl next ([], ledger) posts
    where
        next (failed, ledger) post = 
            case (acceptPost ledger post) of
            Left post_ -> (post_ : failed, ledger)
            Right ledger_ -> (failed, ledger_)

postEntry :: Entry -> Ledger -> Either [Post] Ledger
postEntry (Double debitName creditName amount) ledger = 
    if null failed then Right ledger_ else Left failed
    where
        res = acceptMany ledger [Post Debit debitName amount, Post Credit creditName amount]    
        ledger_ = snd res
        failed = fst res

-- Print account names and balances each on new line 
printAccountBalances :: Ledger -> IO ()
printAccountBalances (Ledger chartMap accountMap storage) = do
    let names = Map.keys accountMap
    let balances = mapMaybe (\name -> fmap (\(TAccount _ balance) -> (name, balance)) (Map.lookup name accountMap)) names
    mapM_ print balances

diagnose :: Either [Post] Ledger -> IO ()
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
ledger2 = postEntry (Double "cash" "brrr" 100) ledger
ledger3 = postEntry (Double "cash" "eq" 100) ledger

-- Main function to demonstrate the creation of a Ledger
main :: IO ()
main = do
    print (whichSide "ts" sampleChart)
    print sampleChart
    putStrLn "This is incorrect posting:"
    diagnose ledger2
    putStrLn "This is correct posting:"
    diagnose ledger3

