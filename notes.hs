import qualified Data.Map as Map

Amount = Int 
AccountName = String
Side = Debit | Credit
TAccount = TAccount Side Amount   
T5 = Asset | Expense | Equity | Liability | Income
Definition = Regular T5 | Contra String
Event = Add T5 AccountName | Offset AccountName AccountName | Post Side AccountName Amount | Drop AccountName | PeriodEnd 
Balances = Map AccountName Amount
-- CompoundEvent = Initial Balances | Transfer AccountName AccountName | Close AccountName
-- typeclass: emit CompoundEvent -> [LedgerEvent]
Account = Account T5 AccountName [AccountName]
single t name = Account t name []
asset = single Asset
expense = single Expense
liability = single Liability
equity = single Equity
income = single Income
offset contra (Account t name cs) = Account t name contra:cs

ChartMap = Map AccountName Definition
TMap = Map AccountName TAccount
Ledger = Ledger ChartMap TMap (Maybe TMap)  
-- apply (Ledger chart accounts m) event:
-- match event   
-- History = Ledger [Event]

-- Add    modifies ChartMap and TMap 
-- Offset modifies ChartMap and TMap 
-- Post   modifies TMap 
-- Drop   modifies TMap
-- PeriodEnd  modifies Maybe TMap

-- empty -> Ledger [] [] Nothing
-- main = putStrln "This works!"

