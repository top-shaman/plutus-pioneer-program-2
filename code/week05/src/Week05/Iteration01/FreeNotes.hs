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

module Week05.FreeNotes where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Aeson                 (ToJSON, FromJSON)
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
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-} -- needed to pre-compile in Template Haskell
mkPolicy :: ScriptContext -> Bool 
mkPolicy _ = True  -- validates to mint & burn in any situation

policy :: Scripts.MonetaryPolicy -- type of Monetary Policy
--We then use Template Haskell to create policy upon validation
policy = mkMonetaryPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy mkPolicy ||]) 

curSymbol:: CurrencySymbol
curSymbol = scriptCurrencySymbol policy -- turns policy into CurrencySymbol

--next we create dataType for Minting Parameters 
data MintParams = MintParams
  { mpTokenName :: !TokenName
  , mpAmount    :: !Integer  -- we want to set it up so that if positive -> minted, if negative -> burned
  } deriving (Generic, ToJSON, FromJSON, ToSchema)
--next step is to defined Schema, one of the parameters of the Contract Monad
type FreeSchema = 
  BlockchainActions -- we need this to gain access to slot, key, etc. 
    .\/ Endpoint "mint" MintParams -- now we name Endpoint "mint" using MintParams
--if we use FreeSchema as a parameter to Contract type, this endpoint will be available
mint :: MintParams -> Contract w FreeSchema Text ()
-- w : uses Writer feature of Contract Monad (tell), by keeping it parametric we indicate we don't use it
-- FreeSchema : we use FreeSchema, meaning we use BlockchainActions with access to "mint" endpoint
-- Text : error messages will be of type Text
-- () : unit return type
mint mp = do
  let val      = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp) -- creates value out of params
      lookups  = Constraints.monetaryPolicy policy
      -- to fulfill mustForgeValue conditions, validation nodes must run policy script so policy script must be included in tx
      -- when algo sees mustForgeValue constraint, it knows it must attach corresponding policy script to tx
      -- hint used by scriptTx is the MonetaryPolicy, which takes policy (actual script w/ hash of the script)
      tx       = Constraints.mustForgeValue val
      -- we don't explicitly define transactions; instead we define every necessary field for transaction
      -- i.e. every necessary field for transaction (fees, forge field, redeemers etc.) give details
      -- lots of repetition so we use Constraint
      -- with Constraints, we declaratively define what the tx should have
      -- then Plutus Library takes care of constructing transaction that fulfills these conditions if possible
      --   these conditions have names that start with "must"
      -- in this case, we want the forge value, i.e. val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx -- we use submitTxConstraintsWith to submit with constraints
  -- submitTx functions take conditions described and construct a transaction that fulfills these conditions
  -- the only condition we want is for the transaction to forge a new value, so function must balance output
  -- we need an input that covers transaction fees, which submitTxConstraintsWith looks up UTXOs and find one or 
  -- more that's big enough to cover the tx
  -- when we forge a token, it must go somewhere, which submitTxConstraintsWith automatically sends to wallet
  -- if negative, it will find an input for tokens from Wallet to go to
  -- if there aren't enough UTXOs in wallet to cover spent funds, it will fail
  -- also if there aren't enough tokens to burn in wallet, it will fail too

  -- as for @Void, Constraints functions are geared towards using specific validator script which has a Datum & Redeemer
  -- all Constraints functions are parametric in Datum & Redeemer, so we can directly use parameter for Datum & Redeemer
  -- so since we aren't using Datum/Redeemer types, we tell compiler we aren't going to use one so we use @Void
  void $ awaitTxConfirmed $ txId ledgerTx  --should always be a valid transaction, unless exception is thrown b/c not enough to burn
  -- eventually we will be able to test to see if validation has failed or other listeners
  Contract.logInfo @String $ printf "forged %s" (show val) -- logs Value
-- to run mint in the Playground, we must redefine another contract, endpoints
-- endpoints is the name of a contract the Playground will run
endpoints :: Contract () FreeSchema Text () 
endpoints = mint' >> endpoints -- define a helper function and recursively call endpoints to ensure mint endpoint is always available
  where
    mint' = endpoint @"mint" >>= mint -- we use `endpoint` function, which blocks until UI provides parameters

mkSchemaDefinitions ''FreeSchema -- needed for playground to provide necessary UI for schema

mkKnownCurrencies [] -- needed for playground to provide necessary UI for currencies

test :: IO ()
test = runEmulatorTraceIO $ do
  let tn = "ABC"
  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints
  callEndpoint @"mint" h1 $ MintParams
    { mpTokenName = tn
    , mpAmount    = 555
    }
  callEndpoint @"mint" h2 $ MintParams
    { mpTokenName = tn
    , mpAmount    = 444
    }
  void $ Emulator.waitNSlots 1
  callEndpoint @"mint" h1 $ MintParams
    { mpTokenName = tn
    , mpAmount    = -222
    }
  void $ Emulator.waitNSlots 1