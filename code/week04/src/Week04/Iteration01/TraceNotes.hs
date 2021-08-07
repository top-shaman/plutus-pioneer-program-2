{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.TraceNotes where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Ledger
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace -- runs EmulatorTrace with defined Trace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints --activateContractWallet takes a wallet and the contract to activate
                                                      -- contract called "endpoints" (it's called endpoints if we want to
                                                      -- work within the playground); returns a Handle
    h2 <- activateContractWallet (Wallet 2) endpoints --creates another Wallet
    callEndpoint @"give" h1 $ GiveParams              --need to specify endpoints using TypeLevel Haskell so we use @"give"
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2 
        , gpDeadline    = Slot 20                     -- since we specified it was the @"give" endpoint, and set the handle
        , gpAmount      = 1000                        -- to Wallet 1, which has 3 args
        }
    void $ waitUntilSlot 20 -- needs to be prepended with void so that return value (i.e. Slot #) isn't returned
    callEndpoint @"grab" h2 () -- doesn't need to be fed any parameter data so () works
    s <- waitNSlots 1
    Extras.logInfo $ "reached slot " ++ show s