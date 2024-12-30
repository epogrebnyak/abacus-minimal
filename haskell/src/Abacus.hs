module Abacus (someFunc) where

import Bases

someFunc :: IO ()
someFunc = putStrLn "someFunc"

dr :: Name -> Amount -> SingleEntry
dr = Single Debit

cr :: Name -> Amount -> SingleEntry
cr = Single Credit
