module Print where 

import Types
import Ledger()
import qualified Data.Map as Map

explain :: Error -> String
explain (AccountError (NotFound name)) = "Account not found: " ++ name
explain (AccountError (NotUnique name)) = "Duplicate account name: " ++ name
explain (AccountError (NotEquity name)) = "Must be an equity account: " ++ name
explain (AccountError (Dropped name)) = "Account deactivated: " ++ name
explain (TransactionError (NotBalanced posts)) = "Entry not balanced: " ++ show posts -- may use sideSum

-- Print account names and balances by lines
printAccountBalances :: Ledger -> IO ()
printAccountBalances ledger = mapM_ putStrLn (accountStrings ledger)
  where
    accountStrings :: Ledger -> [String]
    accountStrings (Ledger _ accountMap _) = do
        (name, tAccount) <- Map.toList accountMap
        return $ name ++ ": " ++ show (accountBalance tAccount)

-- Diagnose the Ledger for errors or print accounts
diagnose :: Either Error Ledger -> IO ()
diagnose (Left e) = putStrLn $ explain e
diagnose (Right ledger) = printAccountBalances ledger
