-- https://stackoverflow.com/questions/59432964/relational-data-model-for-double-entry-accounting
-- In your question you are effectively
-- raising several related issues:

-- 1. how to make an accounting system in general,
-- 2. how to make it for a bank,
-- 3. how to record transactions for current accounts,
-- 4. how make a batch job on accounts,
-- 5. make it all happen in a relational database.

-- I would say also making an accountant and a data engineer happy
-- at the same time and make the excercise code readable by us all are 
-- additional challanges.

-- These requirements arehard to satisfy at once,and you have to 
-- make some trade-offs and sacrifices. Here what the trade-offs are from my 
-- point of view:
-- - Monolith vs separate services. Older banking systems would have had a giant monolith schema
--   under one roof, while newer ones are likely to separate 
--   the domains and products into separate systems with own databases and events running 
--   between them. In your example the currrent account product would be 
--   one system, will have a fee scheduler module inside it and the accounting
--   system for reporting would be another system.  

-- - There is a difference between a transaction system and an analytical system.
--   The current account banking product is a transaction system that "really shows" 
--   where the client money is, participates in event handling and provides information
--   to the analytic system for the accounting report. The transaction system 
--   would need to follow own rules and contracts that satisfy should the accounting 
--   logic later,  but not as a double entry explicitly.

-- - You also have to make a decision on guarantees and reposnsibilities 
--   you want to have on a database level and what are on application level.
--   For example, on a database level you can well enforce strictly double entry system
--   (always two parts in an entry), but if you want a multiple entry system (where entry has many parts), 
--   you may have to do it on an application level or add extra checks within a database.
--   When exchanging the data in event queue between transaction systems 
--   you might mark the event as recorded for accounting at current account 
--   product and at treasury or cash management product that handles bank own assets.

-- Have said that, I still think there is a lot of value to model a bank accounting system as 
-- a relational database and an accounting system and see how far you can go with it as an excercise
-- while keeking some note on how it may be different from real modern or legacy systems.

-- I think the minimal example one may have is a table for account names
-- and a table for double entries. We can disregard the account types for now
-- and see the double entry in action.

-- The account balances table is a unique text name of the account
-- and the account balance is an interger as shown here, maybe a decimal
-- and less likely a real number. Note for a example a programming language like 
-- Solidity used for blockain contracts does not have a native float type, just 
-- integers.
  
DROP TABLE IF EXISTS accounts;
CREATE TABLE accounts (
  name TEXT PRIMARY KEY NOT NULL,
  balance INTEGER NOT NULL DEFAULT 0
);

-- Let us put in several accounts with zero balances.
INSERT INTO accounts (name) VALUES 
    ('owner'), -- equity
    ('cash'), -- asset
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
-- A bank is created - shareholders did put the money into bank capital
    ('Initial equity investment', 'cash', 'owner', 100000),
-- A Client deposits cash to her account
    ('Client deposit', 'cash', 'client.mary', 6500),
-- The Bank charges fees once a month to all Clients accounts (sample batch job),
    ('Monthly fees', 'client.mary', 'fees', 25),
-- Note: the bank is aggresive enough to charge the fees 
-- to the clients with zero account balance 
    ('Monthly fees', 'client.john', 'fees', 25),
-- A Client does some operation over the counter, and the Bank charges a fee
-- (cash withdrawal + withdrawal fee),
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

-- This will result in 
-- sqlite3 < 1.sql

-- cash|106500|5000
-- client.john|25|1000
-- client.mary|6040|6500
-- fees|0|65
-- owner|0|100000

-- In our database we do not have this information about 
-- account types yet, just notes in comments.
-- Some of the accounts will have their balance as credits less debits 
-- ('credit-normal' accounts -- bank equity, liabilities and income) 
-- and some will have the balance as debits less credits 
-- ('debit-normal' accounts -- bank assets and expenses). The debits and credits 
-- are a smart trick to guarantee the accounting identity always holds,
-- but it is just a convention, a way to scale accounting records with
-- less errors. 

-- I find an extended form of accounting equation very useful, which 
-- does exist within the reporting period before we close 
-- the temporary accounts (income and expenses) to retained earnings: 
--
-- ASSETS + EXPENSES = EQUITY + LIABILITIES + INCOME                        (1)
--
-- let EQUITY = SHAREHOLDER + OTHER EQUITY (OE) + RETAINED EARNINGS         (2)
-- where SHAREHOLDER is the shareholder or owner equity 
-- and OTHER EQUITY is the sum of other equity components
-- like reserves, additional capital raised selling share above nominal price,
-- some revaluations, callable capital, etc. In bank especially 
-- the composition of capital is extremely important, because it is
-- used to calculate the capital adequacy ratio, that reflects
-- the bank's ability to absorb losses on bad loans. 
-- Before you go into exploring this topic 
-- you can assign OTHER EQUITY to zero and 
-- just have SHAREHOLDER and RETAINED EARNINGS in the EQUITY variable. 
-- Lets us mark EQUITY' = SHAREHOLDER + OE ("equity prime")                  (3)
-- and rewrite the equation (1) as:

-- ASSETS + EXPENSES = (EQUITY' + RETAINED EARNINGS) + LIABILITIES + INCOME    (4)

-- Whatever events we decide to put into accounting records
-- we must keep the equation balanced. Double entry that changes 
-- exectly two variables in the equation by the same amount would 
-- provide a guarantee the equation holds. You would get this result 
-- by adding 10 to liabilities and 10 to assets, but if you decide 
-- adding 5 to assets and 5 expenses the equation will break. 
-- We need some clever way to comminicate what we are changinging in the 
-- accounnting equation and this method of comminication must preserve 
-- the identity of the equation. Let's call this a "notation challenge". 
-- 
-- Let us consider each account is represented by a pair, or a tuple, of amounts,
-- (left, right), for the accounts on the left side of the equation (assets and expenses)
-- let us calulate the acocunt balance as balance (left - right), and for the right side
-- of the equation (equity, liabilities and income) let us calculate the balance as
-- (right - left) and let all accounts be (0, 0) at the beginning when we create 
-- a ledger of accounts for the new company company.

-- Our newly invented method of changing the accounting equation will be 
-- saying "Add the amount to the left side of the account X and
-- add the same amount to the right side of the account Y", for example
-- "Add 10 to the left side of the account 'cash' and add 10 to the right side of 
-- the account 'equity'", or process `dict(left='cash', right='equity', amount=10)`
-- or "Add 5 to the left side of the account 'inventory' and add 5 to the 
-- right side of account 'expenses'", or process `dict(left='inventory', right='expenses', amount=5)`.

-- One can see whatever you add through the (left, right, amount) tuples or dictionaries, 
-- the equation (4) will hold. 

-- Congratulations, we just invented debits and credits book-keeping system, 
-- as you can call "left" a "debit" and "right" a "credit". Note that debits and credits just satisfy the "notation challenge"
-- above and do not provide further guarantees that you need to check elsewhere. 

-- More specifically, can you add meaningless transaction this way?
-- Of course, something that changes to pair of accounts that where never meant to change together.
-- Can you add a wrong transaction this way? Sure, you meant to say credit income, debit cash
-- for accepting client payment, but you said it other way around credit cash, 
-- debit income and this looks something like a client refund. However, will the equation hold
-- anyways? Yes it will. Is it a good thing? Mostly yes, because there is 
-- one thing less, the accounting identity, to worry about.


-- Excercises:
-- - handle 
--   ('interbank'), -- asset, an account at a correspondent bank or at central bank

-- Extenstions:
-- - why is this not a complete accounting system?
