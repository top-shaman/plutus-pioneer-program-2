{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a 
--   w: allows us to write log messages
--   s: describes blockchain capabilities, i.e. what blockchain-specific actions will this contract perform, types of endpoints
--   e: error messages
--   a: result type
-- EmulatorTrace a

-- B
myContract1 :: Contract () BlockchainActions Text ()
-- Text is a Haskell Type for text that is more efficient than String
-- logInfo needs to be qualified with Contract because EmulatorTrace Monad has it too
-- by default in Haskell, a literal string in double quotes is just a String, so with
-- OverloadedStrings we can use them for otehr types of text. We activate TypeApplications
-- to list the @ symbol and can tell compiler it's a string
myContract1 = do
  void $ Contract.throwError "BOOM!" --we void because we don't care about result (only want sideFX)
  Contract.logInfo @String "Hello from the contract!"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

myContract2 :: Contract () BlockchainActions Void () -- e type of void means there can't be errors, no message type to log
myContract2 = Contract.handleError -- this will throw and catch error inside the Contract Monad
  (\err -> Contract.logError $ "Caught error: " ++ unpack err) -- uses method from Data.Text to convert message to string
  myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (Wallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

--exceptions can be thrown for other reasons, like submitting a transaction and conditions aren't satisfied
--next we'll look at the `s` parameter

--BlockchainActions has no endpoint support, so we must use a different type using the type operator `.\/`
-- must add DataKinds & TypeOperator pragmas
--to setup a new Endpoint we set it up as so:  `.\/ Endpoint "name" Type`
type MySchema = BlockchainActions .\/ Endpoint "foo" Int

myContract3 :: Contract () MySchema Text ()
myContract3 = do
  --we use endpoint Monadic function, which takes in name of previously defined endpoint and binds it to a variable
  n <- endpoint @"foo" -- contract will be blocked until Contract is invoked with an Int
  Contract.logInfo n

myTrace3 :: EmulatorTrace ()
myTrace3 = do
  -- we can't just simply activate the contract, because the endpoint will block it
  -- so we need the handle 
  h <- activateContractWallet (Wallet 1) myContract3
  callEndpoint @"foo" h 42

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

-- now lets look at the first parameter, w which must be of the class Monoid

myContract4 :: Contract [Int] BlockchainActions Text ()
myContract4 = do
  void $ Contract.waitNSlots 10 -- need to void so no return of Current Slot
  tell [1]
  void $ Contract.waitNSlots 10 
  tell [2]
  void $ Contract.waitNSlots 10
  tell [3]
  void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
  h <- activateContractWallet (Wallet 1) myContract4

  void $ Emulator.waitNSlots 5 --should be before the first tell
  xs <- observableState h -- accesses state via handle, returns a list of ints (the state)
  Extras.logInfo $ show xs

  void $ Emulator.waitNSlots 10
  ys <- observableState h 
  Extras.logInfo $ show ys

  void $ Emulator.waitNSlots 10
  zs <- observableState h 
  Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4