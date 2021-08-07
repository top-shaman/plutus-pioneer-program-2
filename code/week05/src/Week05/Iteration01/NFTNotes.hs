{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.NFTNotes where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Aeson                 (ToJSON, FromJSON)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (singleton)
import           Ledger.Constraints         as Constraints
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               as Value
import           Playground.Contract        (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH              (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types           (KnownCurrency (..))
import           Prelude                    (Semigroup (..))
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
--instead of using PubKeyHash as param, we need to use a UTxO
--since we want to name our NFT, let's create a TokenName parameter
mkPolicy :: TxOutRef -> TokenName -> Integer -> ScriptContext -> Bool
mkPolicy oref tn amt ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                           traceIfFalse "wrong amount minted" (checkMintedAmount amt)
  where
    info :: TxInfo -- we will still need TxInfo from the context, so we set up this helper function
    info = scriptContextTxInfo ctx
    
    hasUTxO :: Bool
    -- txInInfoOutRef is the TxOutRef so we check if the oref parameter equals any of the txInfoInputs in info
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info
    -- we have set temporal rules- we haven't set amtber of NFTs to be minted
    
    --I'm doing it a little differently than the example-- I want to mint a specific # of NFTs
    -- txInfoForge (TxInfo) gives us access to Value
    -- ownCurrencySymbol gives us access to CurrencySymbol of a Token based on the context
    checkMintedAmount :: Integer -> Bool
    checkMintedAmount amt = case flattenValue (txInfoForge info) of --since pattern matching triples is not defined so we have to
        [(cs, tn', amt')] -> cs == ownCurrencySymbol ctx && tn' == tn && amt' == amt -- check if each item matches specifically
        _                 -> False -- everything else fails

policy :: TxOutRef -> TokenName -> Integer -> Scripts.MonetaryPolicy
policy oref tn amt = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMonetaryPolicy $ mkPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

curSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
curSymbol oref tn amt = scriptCurrencySymbol $ policy oref tn amt

--we need new parameters: we need the TxOutRef (The UTxO we want to use) and TokenName
--UTxO can be looked up by the wallet itself, so it doesn't need to be passed in as a parameter
-- and for my own experimentation, lets also include one for amt
-- normally this can be deleted, but lets keep it so we can include the amt
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmt    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type NFTSchema =
    BlockchainActions
        .\/ Endpoint "mint" MintParams

mint :: MintParams -> Contract w NFTSchema Text ()
mint mp = do
    pk <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    -- we apply this to pk to get our own Address, apply utxoAt to this value, bind to 'orefs'
    -- must import qualified Data.Map as Map because we only want keys
    -- so we fmap Map.keys over utxoAt (...) because we want the keys (TxOutRefs), not the values (TxOutTx's)
    case Map.keys utxos of  -- we actually can get rid of orefs and just have conditional be on Map.keys utxos
        -- need to throw an error if no UTxOs found
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do -- this isolates head of Map.keys utxos as oref; we don't need 'orefs' because we're isolating head
            let val     = Value.singleton (curSymbol oref (mpTokenName mp) (mpAmt mp)) (mpTokenName mp) (mpAmt mp)
                lookups = Constraints.monetaryPolicy (policy oref (mpTokenName mp) (mpAmt mp)) <> Constraints.unspentOutputs utxos -- we can mappend Constraints.unspentOutputs utxos to lookups
                tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
                -- now we need additional constraint, because we need to consume this UTxO
                -- we must insist on consuming particular UTxO and then it's gone
                -- Constraints.mustSpendPubKeyOutput does just this, and needs 
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmt    = 1
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmt    = 1000
        }
    void $ Emulator.waitNSlots 1
    void $ Emulator.waitNSlots 1
