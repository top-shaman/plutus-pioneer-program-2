{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Plutus.Contract                     hiding (when)
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.Contracts.Currency           as Currency

import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))

import qualified Week06.Oracle.Core                  as Oracle
import           Week06.Oracle.PAB                   (OracleContracts (..))
import qualified Week06.Oracle.Swap                  as Oracle

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do -- similar to EmulatorTrace Monad, will eventually be merged
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver. Press enter to exit." -- logs that PAB is starting
    shutdown <- PAB.Server.startServerDebug -- startServerDebug is used to start server, and its return will later be invoked to end Monad

    cidInit <- Simulator.activateContract (Wallet 1) Init -- launches contract on Wallet 1
    cs      <- waitForLast cidInit -- this is how we get information out from a Contract, will block until Wallet 1 has minted + distributed USDTs, returns CS
    _       <- Simulator.waitUntilFinished cidInit -- waitUntilFinished waits until cidInit function has finished

    cidOracle <- Simulator.activateContract (Wallet 1) $ Oracle cs -- activate Oracle with cs handle
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle -- if we want to use Web Interface, we need to write file "oracle.cid" extracting
    oracle <- waitForLast cidOracle -- UUID (cidOracle) and write it into the file. then we use waitForLast to bind cidOracle's Oracle value into oracle

    forM_ wallets $ \w ->
        when (w /= Wallet 1) $ do
            cid <- Simulator.activateContract w $ Swap oracle
            liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid

    void $ liftIO getLine
    shutdown

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a -- given such a Contract instance Id, we use waitForState which functions similarly
waitForLast cid = -- to observableState function in EmulatorTrace Monad, taking Contract Instance, predicate which returns JSON expression, returns Maybe value
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x -- comes from Data.Aeson library to see if fromJSON succeeds
        _                       -> Nothing

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]

usdt :: TokenName
usdt = "USDT"

oracleParams :: CurrencySymbol -> Oracle.OracleParams
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000
    , Oracle.opSymbol = cs
    , Oracle.opToken  = usdt
    }

-- boilerplate to hookup the datatype (reified contract instances) with actual Contracts

handleOracleContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin OracleContracts))) effs
    )
    => ContractEffect (Builtin OracleContracts)
    ~> Eff effs
handleOracleContracts = handleBuiltin getSchema getContract where
    getSchema = \case
        Init     -> endpointsToSchemas @Empty -- empty Schema because it just has BlockchainActions
        Oracle _ -> endpointsToSchemas @(Oracle.OracleSchema .\\ BlockchainActions) --OracleSchema
        Swap _   -> endpointsToSchemas @(Oracle.SwapSchema   .\\ BlockchainActions) --SwapSchema
    getContract = \case
        Init        -> SomeBuiltin   initContract -- runs initContract
        Oracle cs   -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs -- uses OracleParams with argument cs (CurrencySymbol)
        Swap oracle -> SomeBuiltin $ Oracle.swap oracle -- uses oracle parameter

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin OracleContracts) []
    $ interpret handleOracleContracts

initContract :: Contract (Last CurrencySymbol) BlockchainActions Text ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey -- isolates own PKH
    cur   <- -- forges Currency Symbol in own wallet, 1_000_000 for each wallet
        mapError (pack . show)
        (Currency.forgeContract ownPK [(usdt, fromIntegral (length wallets) * amount)]
        :: Contract (Last CurrencySymbol) BlockchainActions Currency.CurrencyError Currency.OneShotCurrency)
    let cs = Currency.currencySymbol cur -- isolates CurrencySymbol
        v  = Value.singleton cs usdt amount -- isolates value of how much each wallet should get
    forM_ wallets $ \w -> do -- forM_ maps function across wallets, takes list of wallets (wallets) and with anonymous function \w, evenly distributes v to each
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Last $ Just cs -- afterwards, communicates CurrencySymbol that has been initialized
  where
    amount :: Integer
    amount = 100_000_000
