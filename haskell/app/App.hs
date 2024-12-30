module Main where

import qualified Abacus (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Abacus.someFunc
