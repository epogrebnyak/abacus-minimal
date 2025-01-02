module Main where

import Abacus 

main :: IO ()
main = do
  let result = process emptyLedger exampleStream 
  putStrLn "\nThe application goal is to process this chain of events into account balances:"
  mapM_ (\x -> putStrLn ("  " ++ show x)) exampleStream
  diagnose $ result
  print result
