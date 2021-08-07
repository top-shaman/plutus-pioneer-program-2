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

module Week06.Oracle.Swap
    ( SwapSchema
    , swap
    ) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), (<$>))

import           Week06.Oracle.Core
import           Week06.Oracle.Funds

{-# INLINABLE price #-} --amount of USD tokens will be determined by Oracle Value, will change over time determined by exchange rate
price :: Integer -> Integer -> Integer -- $1 will equal 1000000 USD tokens
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer -- gets Ada value, and from this we can get LL value
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE mkSwapValidator #-} -- takes 4 parameters: Oracle & the address of the Oracle; needs to be explicitly handed in b/c Validator needs it compiled
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool -- needs PubKeyHash (Datum), no explicit Redeemer & usual ScriptContext
mkSwapValidator oracle addr pkh () ctx = -- has 3 inputs: Oracle (to check exchange rate), swap output (holds LL), and address of funds from buyer
                                         -- has 3 outputs: Oracle output (oracleValidator takes care of this), seller must get Tokens, buyer gets Lovelace
                                         -- we should create a second use case, that seller can receive their assets, so it has 2 use cases total
    txSignedBy info pkh || -- if it's signed by seller, they just get their hands back on the Lovelace
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs && -- or if there are exactly two inputs (oracle & swap UTxO)
     traceIfFalse "price not paid"                     sellerPaid) -- we also want to check the seller has actually been paid!

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInput :: TxOut
    oracleInput =
      let
        ins = [ o
              | i <- txInfoInputs info -- takes all inputs from info field and constructs a list of outputs to consume
              , let o = txInInfoResolved i -- we use a let expression to compute corresponding output with txInInfoResolved based on
              , txOutAddress o == addr     -- the condition txOutAddress o == address, and we keep all those o's
              ]
      in
        case ins of -- we use this to make sure there's exactly one oracle input, and if there's exactly one, we extract that value
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

    oracleValue' = case oracleValue oracleInput (`findDatum` info) of --helper function checks exchange rate, using helper function we used in Core Module
        Nothing -> traceError "oracle value not found" -- if there's no value, returns nothing, but if it succeeds we extract x
        Just x  -> x

    hasTwoScriptInputs :: Bool -- we check all inputs & filter out the Just values of the Hash of the Address of the resolved Inputs in the Info field
    hasTwoScriptInputs =       -- we set xs as the list of values from this
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2

    minPrice :: Integer -- this computes minimum price
    minPrice = -- if we don't find input from context, then we return an error. of there is a lovelace value, then...
      let
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        price lovelaceIn oracleValue' -- ... we use oracleValue' and this lovelace value to compute the price

    sellerPaid :: Bool -- makes sure that the price paid is greater than the minPrice
    sellerPaid =
      let
        pricePaid :: Integer -- calculates total value of tx paid to seller
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
      in
        pricePaid >= minPrice

data Swapping -- datatype combines Datum & Redeemer like Oracling in Core.hs
instance Scripts.ScriptType Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

swapInst :: Oracle -> Scripts.ScriptInstance Swapping -- we use the usual TemplateHaskell to compile Oracle parameter into Script
swapInst oracle = Scripts.validator @Swapping         -- we don't need address explicitly, because it can be computed. It just can't be used in on-chain code
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

-- we do usual Contract procedure: get validator, get address
swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . swapInst

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

-- off-chain contract
offerSwap :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text () -- takes oracle and amount as parameters
offerSwap oracle amt = do -- we look up our own public key (pkh) and isolate the UTxO, locks amount in the swap contract
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt --isolate amount as constraint
    ledgerTx <- submitTxConstraints (swapInst oracle) tx -- submits swapInst of oracle & tx into ledgerTx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"
-- finds list of all UTxOs resting at addresses that satsify specific predicte
findSwaps :: HasBlockchainActions s => Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do -- takes oracle and function that returns result of helper function `f`
    utxos <- utxoAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos -- makes a list of all UTxOs, finds all Just's and makes a list of all valid UTxOs
  where
    f :: TxOutTx -> Maybe PubKeyHash -- takes one of the UTxOs we're looking at, looks up Datum Hash, and if it succeeds it returns
    f o = do
        dh        <- txOutDatumHash $ txOutTxOut o -- extracts Datumhash from UTxO extracted from oracleValue
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o -- makes a map out of the lookup from datahash, extracted from Datum, extracted from tx of oracleValue
        PlutusTx.fromData d -- this deserializes Maybe Datum to Maybe PubKeyHash
    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash) -- takes pair of Key & Value from UTxO & returns triple of Maybe (Key, Value, PubKeyHash)
    g (oref, o) = do
        pkh <- f o -- gets PKH if it's there (Datum)
        guard $ p pkh -- guard exists to take something of type Boolean, fails if false and continues if true
        return (oref, o, pkh) -- if successful, returns Reference, Value & Datum

retrieveSwaps :: HasBlockchainActions s => Oracle -> Contract w s Text () -- if seller changes their mind and wants their swap back
retrieveSwaps oracle = do
    pkh <- pubKeyHash <$> ownPubKey -- as predicate, checks if swap belong to themselves
    xs <- findSwaps oracle (== pkh) -- finds all UTxO's belonging to user
    case xs of
        [] -> logInfo @String "no swaps found" -- if empty list, do nothing
        _  -> do -- if populated, we construct a transaction with UTxOs belonging to user
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <> -- creates Map of UTxO's from xs, applies constraint
                          Constraints.otherScript (swapValidator oracle) -- provides validator of swap, parameterized by the Oracle
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs] -- takes all Constraints, builds
                          -- and for each Constraint, combines into list with mconcat, i.e. all the UTxOs that belong to seller
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx -- to submit tx, we need all UTxOs in constraints, as well as the validator of swap
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

useSwap :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text ()
useSwap oracle = do -- looks up swaps we can afford on a first-come-first-serve basis
    funds <- ownFunds -- this function looks up our own funds, which is of type Value
    let amt = assetClassValueOf funds $ oAsset oracle -- given all funds, we check how many Tokens we're using
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle -- finds Oracle using findOracle function from Core Module, finds UTxO and Value
    case m of
        Nothing           -> logInfo @String "oracle not found" -- if oracle not found, log
        Just (oref, o, x) -> do -- if not, findOracle returns UTxO and current exchange rate
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- pubKeyHash <$> Contract.ownPubKey -- binds our own PKH to pkh
            swaps <- findSwaps oracle (/= pkh) -- finds all swaps we don't own
            case find (f amt x) swaps of -- Data.List given predicate of a list, it maybe finds element that satisfies predicate
                Nothing                -> logInfo @String "no suitable swap found" -- f takes amount we have, exchange rate, & if swap is suitable
                Just (oref', o', pkh') -> do -- if a swap is suitable, it constructs transaction
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle) -- adds Oracle Fee to value of transaction (o) and previous fees
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x -- price to pay, converted into value
                        lookups = Constraints.otherScript (swapValidator oracle)                     <> -- must be provide validator of swap contract
                                  Constraints.otherScript (oracleValidator oracle)                   <> -- must provide validator of oracle contract
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData Use) <> -- oracle input, Use's Redeemer
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ())  <> -- swap input, trivial Redeemer
                                  Constraints.mustPayToOtherScript -- we want to pay to Oracle, so we use provide Oracle hash as Datum to signify exchange
                                    (validatorHash $ oracleValidator oracle) -- rate change and the cumulative value
                                    (Datum $ PlutusTx.toData x)
                                    v                                                                      <>
                                  Constraints.mustPayToPubKey pkh' p -- we also must pay seller of lovelace with price calculated by p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x -- given exchange rate & output, returns price of lovelaces contained and exchange rate

    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt -- checks if price of swap is covered by the number of tokens we own

type SwapSchema =
    BlockchainActions
        .\/ Endpoint "offer"    Integer -- only typed Endpoint
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()

swap :: Oracle -> Contract (Last Value) SwapSchema Text () -- bundle of all swap methods
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle -- `select` is used to await endpoint input
  where
    offer :: Contract (Last Value) SwapSchema Text ()
    offer = h $ do
        amt <- endpoint @"offer"
        offerSwap oracle amt

    retrieve :: Contract (Last Value) SwapSchema Text ()
    retrieve = h $ do
        endpoint @"retrieve"
        retrieveSwaps oracle

    use :: Contract (Last Value) SwapSchema Text ()
    use = h $ do
        endpoint @"use"
        useSwap oracle

    funds :: Contract (Last Value) SwapSchema Text ()
    funds = h $ do -- blocks until endpoint is invoked, then
        endpoint @"funds"
        v <- ownFunds
        tell $ Last $ Just v

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError
