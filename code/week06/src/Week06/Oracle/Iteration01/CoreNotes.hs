{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , oracleInst
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..))
import qualified Prelude                   as Prelude -- allows us to use PAB

data Oracle = Oracle -- Parameterized contract, will live in this datatype Oracle
    { oSymbol   :: !CurrencySymbol -- Currency Symbol of the NFT we will use for Oracle input and output. Will be an emptystring
    , oOperator :: !PubKeyHash     -- Owner of Oracle, who can make updates. Use can be used by anyone, update can only be used by Operator, identified by PubKeyHash
    , oFee      :: !Integer        -- Fees in Lovelace used whenever someone uses Oracle
    , oAsset    :: !AssetClass     -- identifies target of Oracle, in this case the AssetClass representing a USD token
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle -- needs to be serializable and liftable

data OracleRedeemer = Update | Use -- must support 2 operations, Update & Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer -- implement Redeemer Type

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName  -- we want to use emptyByteString as token name for NFT
oracleTokenName = TokenName emptyByteString

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass -- a way to idenitfy AssetClass of our NFT, to be compared with USD token's asset (oAsset)
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName) --extracts Symbol from oracle, then provides empty TokenName

{-# INLINABLE oracleValue #-} -- helper function, given TxOut, and a way to turn a DatumHash into Maybe Datum, turns it into a Maybe Integer
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer -- looks up Oracle input and Datum of Oracle, and turns it into an Integer
oracleValue o f = do  -- could use Ratio but it doesn't implement well to/from JSON so Integer * 1x10^6 works better
    dh      <- txOutDatum o --datum of Oracle is bound to 'dh' if it succeeds
    Datum d <- f dh -- f (which takes Datum) turns dh into datum if it succeeds
    PlutusTx.fromData d -- Maybe returns d upon success

{-# INLINABLE mkOracleValidator #-} --takes parameter (oracle), Integer value of Datum (from oracleValue), Redeemer (OracleRedeemer), and Context, returns Bool
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool -- we need to check both if both input and output hold NFT
mkOracleValidator oracle x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  && -- we check both input & output at the same time
    traceIfFalse "token missing from output" outputHasToken &&
    case r of                                                  -- based on if inputHasToken/outputHasToken, we determine if Update or Use
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) && -- must check if Operator has actually signed transaction
                  traceIfFalse "invalid output datum"       validOutputDatum -- we want to check we want to check if it's carrying a valid Datum
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              && -- we want to check if Datum is the same as the outputDatum
                  traceIfFalse "fees not paid"              feesPaid -- we want to check that the fees have been paid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut -- Oracle output we are trying to consume,
    ownInput = case findOwnInput ctx of --given ScriptContext, gives us input i.e. output we are trying to consume
        Nothing -> traceError "oracle input missing" --shouldn't happen because we should know we're consuming
        Just i  -> txInInfoResolved i
    -- now we must check if Oracle output's token is present
    inputHasToken :: Bool  -- we use assetClassValueOf to check input value and oracle parameter to return if they're both theyre
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: TxOut -- uses getContuingOutputs which takes context and returns all outputs which go to Oracle Address;
    ownOutput = case getContinuingOutputs ctx of -- we want there to only be one. if there's one, we want a return, of not we want error
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool -- uses assetClassValueOf similar to input but checks output
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe Integer -- takes oracleValue function (which takes TxOut & function that turns DatumHash into Maybe Datum, returns a Maybe Integer)
    outputDatum = oracleValue ownOutput (`findDatum` info) -- uses function on ownOutput with helper function (`findDatum` info) will give us new Oracle value

    validOutputDatum :: Bool -- we want to make sure it's not nothing
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool --
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in -- this uses `geq` function to take a value and see if it's greater than or equal to another value, returns true if output is greater than or equal to
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle)) -- input value + oracle fee. we use `geq` so users can leave a tip for the Oracle Operator

data Oracling -- combines Redeemer & Datum types
instance Scripts.ScriptType Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

oracleInst :: Oracle -> Scripts.ScriptInstance Oracling -- usual Template Haskell to parameterize Oracle and lift parameter into Template Haskell
oracleInst oracle = Scripts.validator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . oracleInst

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator
-- end of on-chain

-- off-chain, open API. different from previous examples because we have on-chain code that isn't reflective of off-chain code
data OracleParams = OracleParams -- we will use two operations: startOracle & updateOracle
    { opFees   :: !Integer
    , opSymbol :: !CurrencySymbol
    , opToken  :: !TokenName
    } deriving (Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle -- we don't know NFT yet, we will use this function to mint it
startOracle op = do -- we use use plutus-use-cases package module that provides a forgeContract function to mint NFTs, but it's not completely compatible
    pkh <- pubKeyHash <$> Contract.ownPubKey --our address
 -- the problem with forgeContract is that it doesn't allow error messages. there is a constraint on the e (Text doesn't implement e)
 -- Plutus.Contract.Types.mapError allows us to switch type of error messages, if we give a function that converts first message type to second
 -- Currency module uses CurrencyError for its e type, so we'll use (pack . show) to turn error text into a string and packs it into Text
 -- so we use MapError with the CurrencyError converted to Text via (pack . show), and we apply this to our forgeContract function, giving
 -- it a type of Contract w s CurrencyError OneShotCurrency, so show would know which datatype to convert to String
    osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency) -- generates Contract w/ NFT
    let cs     = Currency.currencySymbol osc -- extracts CurrencySymbol from NFT Contract
        oracle = Oracle -- constructs Oracle value with datatype
            { oSymbol   = cs -- extracted CurrencySymbol
            , oOperator = pkh -- our address
            , oFee      = opFees op -- opFees field from OracleParams
            , oAsset    = AssetClass (opSymbol op, opToken op) -- symbol and token into an AssetClass
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text () -- takes oracle, value we want to update it to, returns contract
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1 -- helper definition takes Datum, and assetClassValue to return constraint
    case m of
        Nothing -> do -- two cases: if we've found UTxO or not
            ledgerTx <- submitTxConstraints (oracleInst oracle) c -- constraint, extracted by mustPayToTheScript applied to assetClassValue of oracle & 1
            awaitTxConfirmed $ txId ledgerTx -- wait for confirmation
            logInfo @String $ "set initial oracle value to " ++ show x -- log that initial value was set
        Just (oref, o,  _) -> do -- in other case we need the return of `findOracle`
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <> -- this lookup creates a map of one key-value pair as UTxO
                          Constraints.scriptInstanceLookups (oracleInst oracle) <> -- this lookup provides instances, uses Instance for input
                          Constraints.otherScript (oracleValidator oracle) -- this lookup provides instance, uses Validator for output
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update) -- we want to create an Oracle output the new address
                                                              -- takes output we want to consume (oref) as well as Redeemer, which comes from toData of Update
                                                              -- for this to work we must create a series of lookups
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx -- this submits Tx constraints w/ datatype Oracling, providing lookups and the tx itself
                                                                     -- it's a "nudge" to tell compiler which Script to apply this to
                                                              -- so the goal of this function is to say: we want to consume an existing output w/ NFT and we
                                                              -- want to create a new output w/ NFT, as well as input to pay Tx fees, determined by imbalance
            awaitTxConfirmed $ txId ledgerTx -- waits for tx to be confirmed
            logInfo @String $ "updated oracle value to " ++ show x

findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle) -- we use utxoAt to find all UTxOs sitting at oracle address, and we use `f` to find the correct one
    return $ case Map.toList utxos of -- we convert Map to List of key-value pairs, and distinguish 2 cases: if we've found exactly one pair, or everything else
        [(oref, o)] -> do -- if there's one pair, we apply oracleValue to txOutTxOut of oracle (to get input) and function which uses Map.lookup of argument dh
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o -- to look through txData field of the tx (txOutTxTx of oracle)
            return (oref, o, x) -- if it is there, it will return value, i.e. the Datum. if not, it will return Nothing
        _           -> Nothing
  where
    f :: TxOutTx -> Bool -- checks UTxO and see if NFT is contained in it once, because as an NFT it must exist once
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text () -- for use in Emulator Trace Monad, playground, or PAB; combines both into one contract
runOracle op = do -- uses startOracle function to start oracle, minting NFT
    oracle <- startOracle op
    tell $ Last $ Just oracle -- writes Oracle value so we can communicate parameter value to the outside world. tell expects Monoid, gives last value of Monoid
    go oracle -- as soon as we have NFT and Just Oracle value, it blocks at update endpoint until an Integer is provided with Oracle UTxO
  where       -- sets Integer as new value, and runs startOracle again
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle
