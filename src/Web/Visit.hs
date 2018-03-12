{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Visit where

import           Control.Monad.Trans                   (liftIO)
import qualified Data.Text.Lazy                        as TL
import           Database.Persist.Postgresql           (fromSqlKey)
import           Tola.Types.ChargeRequest
import           Tola.Types.Common
import           Web.Crypto                            (idToHex)
import           Web.Scotty.Trans
import           Web.ScottyHelpers
import           Web.Types.ChargeRequestClientResponse
import           Web.Types.WebApp

homeWeb :: WebApp
homeWeb = getAndPostAndHead "/" $ do
  writeLog "request to /"
  req <- liftIO $ mkChargeRequest' (mkTarget "0000") (mkAmount 23) (mkMsisdn "0292883") (mkArbitraryReference "someref")
  insertChargeRequest req
  res <- makeChargeRequest req
  json res


chargeRequestWeb :: WebApp
chargeRequestWeb = getAndHead "/api/charge/:msisdn/:amount/:arbitref" $ do
  addHeader "Access-Control-Allow-Origin" "*"
  -- reqid <- header "X-RequestId"
  amount' <- mkAmount . (toRational :: Double -> Rational) <$> param "amount"
  msisdn' <- mkMsisdn <$> param "msisdn"
  arbitref <- mkArbitraryReference <$> param "arbitref"
  let target = mkTarget "850702"
  cr <- liftIO $ mkChargeRequest' target amount' msisdn' arbitref
  cridKey <- insertChargeRequest cr
  let crid = fromIntegral $ fromSqlKey cridKey
  crid' <- liftIO $ (idToHex 10000 :: Integer -> IO String) crid
  addScotchHeader "ChargeRequestId" (TL.pack crid')
  resp <- makeChargeRequest cr
  updateChargeRequestWithResponse crid resp
  json $ mkChargeRequestClientResponse (mkSourceReferenceFromString crid') resp

