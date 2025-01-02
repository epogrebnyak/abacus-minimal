module Main where

import Abacus 

main :: IO ()
main = do
  putStrLn "\nThe application goal is to process this chain of events into account balances:"
  -- print playWithThisLedger
  mapM_ (\x -> putStrLn ("  " ++ show x)) exampleStream
