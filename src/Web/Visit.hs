{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Visit(
    doMigrationsWeb
  , lodgementNotificationWeb
  , disbursementNotificationWeb
  , chargeRequestWeb
  , checkChargeRequestWeb
  , echoWeb
  , homeWeb, tolaRootWeb
)
where

import           Control.Monad.IO.Class              (liftIO)
import qualified Data.Aeson                          as A
import           Data.Monoid                         ((<>))
import           Data.String                         (fromString)
import           Data.Text                           (pack)
import qualified Data.Text.Encoding                  as E
import qualified Data.Text.Lazy                      as TL
import qualified Data.Time.Clock                     as Clock
import           Network.HTTP.Types.Status           (mkStatus, status404,
                                                      status500)
import qualified Tola.ChargeRequest                  as TChargeRequest
import qualified Tola.Common                         as Tola
import qualified Tola.TolaInterface                  as TolaInterface
import           Web.Api.ChargeRequestClientResponse (mkChargeRequestClientResponse)
import qualified Web.Localization                    as L
import           Web.Model
import           Web.WebM


doMigrationsWeb :: WebMApp ()
doMigrationsWeb =
  getAndHead "/do_migrations" $
    doMigrations >> text "done!"

homeWeb :: WebMApp ()
homeWeb = getAndPostAndHead "/" $ status status404 >> text ""

tolaRootWeb :: WebMApp ()
tolaRootWeb = getAndPostAndHead "/tola" $ status status404 >> text ""


echoWeb :: WebMApp ()
echoWeb = getAndHead "/tola/echo/:message" $ text =<< param "message"

-- Tola API

notificationWeb path headerName insert =
  post (fromString $ "/tola/" <> path <> "/") $ do
    mcn <- fmap A.eitherDecode body
    case mcn of
      Left err -> status (mkStatus 500 $ E.encodeUtf8 $ pack err) >> json (Tola.mkSuccessResponse False)
      Right cn -> do
        (cnid :: Integer) <- fromIntegral . fromSqlKey <$> insert cn
        addScotchHeader headerName (TL.pack $ show cnid)
        json $ Tola.mkSuccessResponse True

lodgementNotificationWeb :: WebMApp ()
lodgementNotificationWeb = notificationWeb "lodgement_notification" "LodgementNotificationId" insertLodgementNotificationAndupdateChargeRequest

disbursementNotificationWeb :: WebMApp ()
disbursementNotificationWeb = notificationWeb "disbursement_notification" "DisbursementNotificationId" insertDisbursementNotificationAndupdateChargeRequest


-- Client API

chargeRequestWeb :: WebMApp ()
chargeRequestWeb = getAndHead "/api/charge/:msisdn/:amount/:arbitref" $ do
  addHeader "Access-Control-Allow-Origin" "*"
  -- reqid <- header "X-RequestId"
  amount' <- Tola.mkAmount . (toRational :: Double -> Rational) <$> param "amount"
  msisdn' <- Tola.mkMsisdn <$> param "msisdn"
  arbitref <- Tola.mkArbitraryReference <$> param "arbitref"
  now <- liftIO Clock.getCurrentTime
  cridKey <- addChargeRequest amount' msisdn'
  crid <- liftIO $ (L.idToHex 10000 :: Integer -> IO String) . fromIntegral . fromSqlKey $ cridKey
  addScotchHeader "ChargeRequestId" (TL.pack crid)
  let target = Tola.mkTarget "850702"
  secret <- readSecret
  let cr = TChargeRequest.mkChargeRequest secret target amount' msisdn' now arbitref
  resp <- runTola (`TolaInterface.makeChargeRequest` cr)
  updateChargeRequestWithResponse cridKey resp
  json $ mkChargeRequestClientResponse (Tola.mkSourceReferenceFromString crid) resp

checkChargeRequestWeb :: WebMApp ()
checkChargeRequestWeb = getAndHead "/api/check_charge/:chargeRequestId" $ do
  addHeader "Access-Control-Allow-Origin" "*"
  eCreqId <- L.fromHexId 10000 <$> param "chargeRequestId"
  case eCreqId of
    Left e -> jsonError e
    Right (_, creqId) -> do
      mcreq <- getChargeRequestStatus creqId
      maybe (jsonError "No ChargeRequest was found") json mcreq

jsonError e = status status500 >> json (Tola.mkApiError e)
