{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- allows us to use PlutusTx.makeLift
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week03.ParameterizedNotes where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

-- now change VestingDatum to VestingParam
data VestingParam = VestingParam
    { beneficiary :: PubKeyHash
    , deadline    :: Slot
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingParam
PlutusTx.makeLift ''VestingParam

{-# INLINABLE mkValidator #-}
-- since we want to introduce Parameters, we don't need a complex Datum
-- change datum to () and add Vesting Parameter ahead of the 3 validator arguments
mkValidator :: VestingParam -> ()-> () -> ScriptContext -> Bool
mkValidator param () () ctx = -- change 'dat' to `param`
    traceIfFalse "beneficiary's signature missing" checkSig      &&
    traceIfFalse "deadline not reached"            checkDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkSig :: Bool
    checkSig = beneficiary param `elem` txInfoSignatories info

    checkDeadline :: Bool
    checkDeadline = from (deadline param) `contains` txInfoValidRange info

data Vesting
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = () -- change Datum to ()
    type instance RedeemerType Vesting = ()

inst :: VestingParam -> Scripts.ScriptInstance Vesting -- now we need to prepend Scripts with VestingParameter
inst p = Scripts.validator @Vesting -- add argument p, but normally we can't have a changing argument in Template Haskell
                                    -- since everything in the brackets needs to be known at compile time. So we use the
                                    -- PlutusTx.applyCode to turn the templateHaskell into a function that can be applied
                                    -- during runtime. So we use the Lift Class
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    --1. mkValidator takes an additional arguement, so it's not suitable for Scripts.validator function
    --2. we compile it into a Plutus Script function that takes 4 arguments
    --3. we use (PlutusTx.liftCode p) to turn Haskell value into Plutus Script value
    --4. then we apply PlutusTx.applyCode to apply mkValidator (w/ 4 arguments) to the Plutus Script value of p
    --5. so the result is a Plutus Script code with 3 arguments, which Scripts.validator will accept
    --6. we need an instance, so below where we instantiated PlutusTx.unstableMakeIsData, we use PlutusTx.makeLift ''
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @() -- no longer needs type of Datum, only ()

validator :: VestingParam -> Validator -- add VestingParam to type
validator = Scripts.validatorScript . inst -- we must compose (i.e. add . ) to allow additional parameter

scrAddress :: VestingParam -> Ledger.Address -- add VestingParam to type
scrAddress = scriptAddress . validator -- same as validator, must compose to allow additional parameter

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !Slot
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
    BlockchainActions
        .\/ Endpoint "give" GiveParams
        .\/ Endpoint "grab" Slot -- in order to construct the address we've grabbed from, we need to have
                                 -- parameters (beneficiary & deadline). Beneficiary is the wallet itself
                                 -- so we don't need to pass that in, but we do need to pass in the time
                                 -- i.e. slot

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give gp = do
    let p = VestingParam
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx  = mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp -- change Datum to ()
    ledgerTx <- submitTxConstraints (inst p) tx -- need to pass in `p` to inst
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Slot -> Contract w s e () -- add Slot to type, since we need deadline
grab d = do -- need additional parameter for deadline, from new parameter
    now   <- currentSlot
    pkh   <- pubKeyHash <$> ownPubKey
    if now < d -- need to check BEFORE if it's before the deadline
        then logInfo @String $ "too early"
        else do
            let p = VestingParam
                        { beneficiary = pkh
                        , deadline    = d
                        }
            utxos <-  utxoAt (scrAddress p) -- we can get rid of filter because `if now < d` does the trick
            if Map.null utxos
                then logInfo @String $ "no gifts available"
                else do
                    let orefs   = fst <$> Map.toList utxos
                        lookups = Constraints.unspentOutputs utxos      <>
                                  Constraints.otherScript (validator p) -- validator needs parameter p now
                        tx :: TxConstraints Void Void
                        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                                mustValidateIn (from now)
                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "collected gifts"
  where
    isSuitable :: PubKeyHash -> Slot -> TxOutTx -> Bool
    isSuitable pkh now o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> False
        Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing        -> False
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> False
                Just d  -> beneficiary d == pkh && deadline d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab -- chnge `>>` to `>>=` because grab has a parameter now

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
