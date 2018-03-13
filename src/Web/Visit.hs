{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Visit where

import           Control.Monad.Trans                   (liftIO)
import qualified Data.Aeson                            as A
import qualified Data.ByteString.Char8                 as Char8
import           Data.Monoid                           ((<>))
import           Data.String                           (fromString)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.Text.Lazy                        as TL
import           Database.Persist.Postgresql           (fromSqlKey)
import           Network.HTTP.Types.Status             (mkStatus, status500)
import           Tola.Types.ChargeRequest
import           Tola.Types.Common
import           Web.Crypto                            (fromHexId, idToHex)
import           Web.Scotty.Trans
import           Web.ScottyHelpers
import           Web.Types.ChargeRequestClientResponse
import           Web.Types.WebApp

-- Tola

notificationWeb path headerName insert =
  postAndHead (fromString $ "/tola/" <> path <> "/") $ do
    mcn <- fmap A.eitherDecode body
    case mcn of
      Left err -> do
            status (mkStatus 500 $ T.encodeUtf8 $ T.pack err)
            writeLog $ Char8.pack err
            json (mkSuccessResponse False)
      Right cn -> do
        (cnid :: Integer) <- fromIntegral . fromSqlKey <$> insert cn
        addScotchHeader headerName (TL.pack $ show cnid)
        json $ mkSuccessResponse True

lodgementNotificationWeb :: WebApp
lodgementNotificationWeb = notificationWeb
  "lodgement_notification"
  "LodgementNotificationId"
  insertLodgementNotificationAndupdateChargeRequest

disbursementNotificationWeb :: WebApp
disbursementNotificationWeb = notificationWeb
  "disbursement_notification"
  "DisbursementNotificationId"
  insertDisbursementNotificationAndupdateChargeRequest


-- API

homeWeb :: WebApp
homeWeb = getAndHeadAccessOrigin "/" $ do
  writeLog "request to /"
  req <- liftIO $ mkChargeRequest' (mkTarget "0000") (mkAmount 23) (mkMsisdn "0292883") (mkArbitraryReference "someref")
  insertChargeRequest req
  res <- makeChargeRequest req
  json res

chargeRequestWeb :: WebApp
chargeRequestWeb = getAndHeadAccessOrigin "/api/charge/:msisdn/:amount/:arbitref" $ do
  amount' <- mkAmount . (toRational :: Double -> Rational) <$> param "amount"
  msisdn' <- mkMsisdn <$> param "msisdn"
  arbitref <- mkArbitraryReference <$> param "arbitref"

  let target' = mkTarget "850702"
  cr <- liftIO $ mkChargeRequest' target' amount' msisdn' arbitref
  cridKey <- insertChargeRequest cr
  let crid = fromIntegral $ fromSqlKey cridKey
  resp <- makeChargeRequest cr
  updateChargeRequestWithResponse crid resp

  crid' <- liftIO $ (idToHex 10000 :: Integer -> IO String) crid
  addScotchHeader "ChargeRequestId" (TL.pack crid')
  json $ mkChargeRequestClientResponse (mkSourceReferenceFromString crid') resp

checkChargeRequestWeb :: WebApp
checkChargeRequestWeb = getAndHeadAccessOrigin "/api/check_charge/:chargeRequestId" $ do
  eCreqId <- fromHexId 10000 <$> param "chargeRequestId"
  case eCreqId of
    Left e -> jsonError e
    Right (_, creqId) -> do
      mcreq <- getChargeRequestStatus creqId
      maybe (jsonError "No ChargeRequest was found") json mcreq

jsonError e = status status500 >> json (mkApiError e)
