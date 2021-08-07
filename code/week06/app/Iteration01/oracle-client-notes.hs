{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString        (ByteString)
import Data.ByteString.Char8  (unpack)
import Data.Proxy             (Proxy (..))
import Data.Text              (pack)
import Data.UUID
import Network.HTTP.Req
import Text.Regex.TDFA

main :: IO ()
main = do
    uuid <- read <$> readFile "oracle.cid"
    putStrLn $ "oracle contract instance id: " ++ show uuid
    go uuid Nothing
  where
    go :: UUID -> Maybe Integer -> IO a
    go uuid m = do
        x <- getExchangeRate -- checks exchange rate from coinmarketcap.com and inserts it into Maybe Integer parameter
        let y = Just x -- when it changes, updates, every 5 seconds
        when (m /= y) $
            updateOracle uuid x
        threadDelay 5_000_000
        go uuid y

updateOracle :: UUID -> Integer -> IO () -- makes POST request and packs uuid in endpoint/update, makes ReqBodyJson of exchange rate. doesn't need to be Haskell
updateOracle uuid x = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "update")
        (ReqBodyJson x) -- value to be updated to
        (Proxy :: Proxy (JsonResponse ())) -- just says to expect a JSON format response
        (port 8080) -- tells port
    liftIO $ putStrLn $ if responseStatusCode v == 200 -- if goes well logs messages accordingly to console
        then "updated oracle to " ++ show x
        else "error updating oracle"

getExchangeRate :: IO Integer -- coinmarketcap provides their own API, but we just use raw HTTP address from whole website
getExchangeRate = runReq defaultHttpConfig $ do
    v <- req
        GET
        (https "coinmarketcap.com" /: "currencies" /: "cardano")
        NoReqBody
        bsResponse
        mempty
    let priceRegex      = "priceValue___11gHJ\">\\$([\\.0-9]*)" :: ByteString -- we use Regex to grab exchange rate, but this is fragile/insecure
        (_, _, _, [bs]) = responseBody v =~ priceRegex :: (ByteString, ByteString, ByteString, [ByteString]) -- grabs exchange rate
        d               = read $ unpack bs :: Double -- unpacks double into Integer
        x               = round $ 1_000_000 * d -- converts to integer
    liftIO $ putStrLn $ "queried exchange rate: " ++ show d -- logs to console
    return x
