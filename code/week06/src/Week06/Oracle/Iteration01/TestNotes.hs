{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Test where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..))
import           Wallet.Emulator.Wallet

import           Week06.Oracle.Core
import           Week06.Oracle.Funds
import           Week06.Oracle.Swap

assetSymbol :: CurrencySymbol -- creates arbitrary Symbol for testing purposes
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"

test :: EmulatorTrace () -> IO () -- takes default log Message config, extended (emCfg) to configure distribution so that everyone has 100 million LL and 100 million USDT
test t = runEmulatorTraceIO' def emCfg t
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

checkOracle :: Oracle -> Contract () BlockchainActions Text a -- checks Oracle value & logs it
checkOracle oracle = do
    m <- findOracle oracle
    case m of -- if oracle isn't found, stop. if it is found, log it & wait for 1 slot & recurse
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    Contract.waitNSlots 1 >> checkOracle oracle

myTrace :: EmulatorTrace ()
myTrace = do
    let op = OracleParams
                { opFees = 1_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                }

    h1 <- activateContractWallet (Wallet 1) $ runOracle op -- starts oracle with initialized parameters, used getOracle which handles Oracle and tells state
    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1 -- gets value

    void $ activateContractWallet (Wallet 2) $ checkOracle oracle -- we check the Oracle to print the Oracle value

    callEndpoint @"update" h1 1_500_000 -- initializes Oracle with 1.5 USD/ADA
    void $ Emulator.waitNSlots 3

    void $ activateContractWallet (Wallet 1) ownFunds' -- checks initial balances on wallets 1,3,4,5
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    h3 <- activateContractWallet (Wallet 3) $ swap oracle -- starts swap on h3,h4,h5
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle

    callEndpoint @"offer" h3 10_000_000 -- offers 10 ADA for swap
    callEndpoint @"offer" h4 20_000_000 -- offers 20 ADA for swap
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 () -- h5 USES swap
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_700_000 -- updates Oracle Value to 1.7
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 () -- h5 uses swap again but pays at a different price
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_800_000 -- updates Oracle Value to 1.8, allows h1 to collect fees
    void $ Emulator.waitNSlots 3

    callEndpoint @"retrieve" h3 () -- retrieves all remaining swaps
    callEndpoint @"retrieve" h4 ()
    void $ Emulator.waitNSlots 3
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle

myTrace2 :: EmulatorTrace ()
myTrace2 = do
    let op = OracleParams
                { opFees = 1_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                }
    h1 <- activateContractWallet (Wallet 1) $ runOracle op
    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1

    callEndpoint @"update" h1 1_200_000
    void $ Emulator.waitNSlots 3

    void $ activateContractWallet (Wallet 1) ownFunds'
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'
    void $ activateContractWallet (Wallet 6) ownFunds'
    void $ activateContractWallet (Wallet 7) ownFunds'

    h3 <- activateContractWallet (Wallet 3) $ swap oracle
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle
    h6 <- activateContractWallet (Wallet 6) $ swap oracle
    h7 <- activateContractWallet (Wallet 7) $ swap oracle

    callEndpoint @"offer" h3 10_000_000
    callEndpoint @"offer" h4 20_000_000
    callEndpoint @"offer" h6 10_000_000_000_000
    callEndpoint @"update" h1 1_800_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_500_000
    void $ Emulator.waitNSlots 1

    callEndpoint @"retrieve" h3 ()
    callEndpoint @"retrieve" h4 ()
    callEndpoint @"retrieve" h6 ()
    void $ Emulator.waitNSlots 3

  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
