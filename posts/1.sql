-- https://stackoverflow.com/questions/59432964/relational-data-model-for-double-entry-accounting
-- In your question you are effectively
-- raising several related issues:
-- 1. how to make an accounting system in general,
-- 2. how to make it for a bank,
-- 3. how to record transactions for current accounts,
-- 4. how make a batch job on accounts,
-- 5. make it all happen in a relational database.

-- I would say also making an accountant and a data engineer happy
-- at the same time and make database code readable by us all are 
-- additional challanges.

-- These are a bit hard to satisfy at once, because you have to 
-- make some trade-offs and sacrifices. Here what they are from my 
-- point of view:
-- - older banking systems would have had a giant monolith schema
--   under one roof, while newer ones are likely to separate 
--   the domains and products into separate databases with events running 
--   between them. In your example the currrent account product would be 
--   one system and a fee scheduler may be another. 

-- - There is a difference between a transaction system and analytical system,
--   the current account product is a transaction system that "really shows" 
--   where the client money is and gives extra information to the analytic system 
--   to compile the accounting report. The transaction system would need 
--   to follow own rules and contracts that satisfy should the accounting logic later, 
--   but not as double entry itself.

-- - You also have to make a decision on guarantees and reposnsibilities 
--   you want to have on a database level and what are on application level.
--   For example, on a database level you can well enforce strictly double entry system,
--   but if you want a multiple entry system (where entry has many parts), 
--   you may have to do it on an application level and extra checks within a database.

-- Have said that, I still think there is a lot of value to model a bank accounting system as 
-- a relational database and see how far you can go with it and how it may be different
-- from real systems.

-- I think the minimal example one may have is a table for account names
-- and a table for double entries. We can disregard the account types for now
-- and see the double entry in action.

-- The account balances table is a unique text name of the account
-- and the account balance interger here or maybe a decimal.
  
DROP TABLE IF EXISTS accounts;
CREATE TABLE accounts (
  name TEXT PRIMARY KEY NOT NULL,
  balance INTEGER NOT NULL DEFAULT 0
);

-- Let us put in several accounts with zero balances
INSERT INTO accounts (name) VALUES 
    ('owner'), -- equity
    ('cash'), -- asset
    ('interbank'), -- asset ('nostro' account)
    ('client.mary'), -- liabilty
    ('client.john'), -- liabilty
    ('fees'); -- income

DROP TABLE IF EXISTS entries;
CREATE TABLE entries (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT,
  debit TEXT,
  credit TEXT,
  amount INTEGER NOT NULL,
  FOREIGN KEY (debit) REFERENCES accounts(name),
  FOREIGN KEY (credit) REFERENCES accounts(name)
);

INSERT INTO entries (title, debit, credit, amount) VALUES     
-- A bank is created
    ('Initial equity investment', 'cash', 'owner', 100000),
-- A Client deposits cash to her account
    ('Client deposit', 'cash', 'client.mary', 6500),
-- The Bank charges fees once a month to all Clients accounts (sample batch job),
    ('Monthly fees', 'client.mary', 'fees', 25),
-- Note: here the bank is aggresive enough to charge the fees 
-- to the clients with zero account balance 
    ('Monthly fees', 'client.john', 'fees', 25),
-- A Client does some operation over the counter, and the Bank charges a fee (cash withdrawal + withdrawal fee),
    ('Cash withdrawal', 'client.mary', 'cash', 5000),
    ('Withdrawal fee', 'client.mary', 'fees', 15),
-- Mary sends some money from her account, to John's account, which is in the same bank
    ('Money transfer', 'client.mary', 'client.john', 1000);

-- Trial balance will show the components of account balance for these accounts
SELECT name, 
       SUM(CASE WHEN name = debit THEN amount ELSE 0 END) AS debit,
       SUM(CASE WHEN name = credit THEN amount ELSE 0 END) AS credit
  FROM accounts
  JOIN entries ON name = debit OR name = credit
  GROUP BY name;

-- Some of the accounts will have their balance as credits - debits 
-- ('credit-normal' accounts, bank equity, liabilities and income) 
-- and some will have the balance as debits - credits 
-- ('debit-normal' accounts - bank assets and expenses).
-- In our database we do not have this information about 
-- account types yet. 