{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-} -- add DeriveAnyClass
{-# LANGUAGE DeriveGeneric       #-} -- add DeriveGeneric
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week03.VestingNotes where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON) --to convert JSON data
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic) -- to import Generics
import           Plutus.Contract      hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema) -- add ToSchema
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

--let's think of Datum and Redeemer
--for Datum we should set Beneficiary & Deadline

data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash --PubKeyHash is the type of the public key that is most convenient
    , deadline    :: Slot
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool --replace VestingDatum, and change Redeemer to Unit
-- now we need to check to see if Beneficiary is querying & check timing
mkValidator dat () ctx =
    -- throw error when mismatch on Beneficiary & Deadline, so we can check with helper functions (checkSig & checkDeadline)
    traceIfFalse "benificiary's signature missing" checkSig      &&
    traceIfFalse "deadline not reached"            checkDeadline
  where
    --checking ScriptContext, we don't need to be concerned with ScriptPurpose,
    -- because this is in the context of Spending
    --looking at TxInfo, Timing (txInfoValidRange) & Signatures (txInfoSignatories) are relevant  
    --field name is long, so we might as well create a helper function to isolate TxInfo
    info :: TxInfo
    info = scriptContextTxInfo ctx
    --search through (txInfoSignatories info) for beneficiary dat, i.e. beneficiary information within VestingDatum
    --we use `elem` inline because it's more visually intuitive
    checkSig :: Bool
    checkSig = beneficiary dat `elem` txInfoSignatories info 
    --we set 
    checkDeadline :: Bool
    checkDeadline = from (deadline dat) `contains` txInfoValidRange info

    

--change all instances of 'Typed' to Vesting
data Vesting
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = VestingDatum -- change () to VestingDatum
    type instance RedeemerType Vesting = () -- change Redeemer to ()

inst :: Scripts.ScriptInstance Vesting
inst = Scripts.validator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @() --change Datum to VestingDatum and Redeemer to ()

validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

--we need parameters: beneficiary, deadline & amount, go to give constructor after changing VestingSchema
data GiveParams = GiveParams
  { gpBeneficiary :: !PubKeyHash
  , gpDeadline    :: !Slot
  , gpAmount      :: !Integer
  } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema = --change all instances of GiftSchema to VestingSchema
    BlockchainActions
        .\/ Endpoint "give" GiveParams -- change to Endpoint "give" GiveParams
        .\/ Endpoint "grab" () --doesn't need any parameters

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e () --change first arg to GiveParams
give gp = do -- change argument to gp
    let dat = VestingDatum-- create datum from GiveParams
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx = mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp -- change () datum to 'dat'
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s" -- change to indicate 1)value 2)beneficiary 3)deadline
      (gpAmount gp)             -- change 'amount' to (gpAmount gp)
      (show $ gpBeneficiary gp) --1st %s argument for printf
      (show $ gpDeadline gp)    --2nd %s argument for printf

-- grab becomes more complex because grabber needs to find UTXOs that he or she can actually consume
-- we need to create a helper function to look at entire UTXO & apply filter to entries, only keeps
--  entries that are available now (isSuitable)
-- isSuitable gets address(PubKeyHash) of wallet & current time, as well as transaction output to consider
--  then checks whether it's available
grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e () -- get rid of Integer
grab = do -- get rid of variable
    --we have to get current time & public key hash into this function
    now   <- currentSlot -- 
    pkh   <- pubKeyHash <$> ownPubKey
    --if no UTXO is available, we shouldn't even try to submit transaction + send local message saying nothing's available
    utxos <- Map.filter (isSuitable pkh now) <$> utxoAt scrAddress -- uses helper function isSuitable to confirm availability, so fees are avoided
    if Map.null utxos -- checks if nothing is returned 
        then logInfo @String $ "no gifts available" -- says nothing is available
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                        Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <> -- change Redeemer to (), says I want to consume all available scripts
                        mustValidateIn (from now) -- this constraint ensures txValidSlotRange is adhered to, instead of 'always'
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: PubKeyHash -> Slot -> TxOutTx -> Bool -- gets wallet, time, and output to consider
    isSuitable pkh now o = case txOutDatumHash $ txOutTxOut o of -- checks if Datum hash is actually available
        Nothing -> False
        Just h  -> case Map.lookup h $ txData $ txOutTxTx o of -- serializes Datum hash
            Nothing        -> False
            Just (Datum e) -> case PlutusTx.fromData e of -- if it succeeds, we see if beneficiary of UTXO is indeed Public Key Hash & deadline has passed
                Nothing -> False
                Just d  -> beneficiary d == pkh && deadline d <= now

endpoints :: Contract () VestingSchema Text () -- change to VestingSchema
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >> grab -- change to >> 

mkSchemaDefinitions ''VestingSchema -- change to VestingSchema

mkKnownCurrencies []