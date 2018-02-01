{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Visit(
    doMigrationsWeb
  , lodgementNotificationWeb
  , chargeRequestWeb
  , echoWeb
  , homeWeb, tolaRootWeb
)
where

import           Control.Monad.IO.Class              (liftIO)
import qualified Data.Aeson                          as A
import qualified Data.Aeson.Types                    as AT
import qualified Data.ByteString                     as BS
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text, pack, unpack)
import qualified Data.Text.Encoding                  as E
import qualified Data.Text.Lazy                      as TL
import qualified Data.Time.Clock                     as Clock
import           Network.HTTP.Types.Status           (mkStatus, status404)
-- import qualified Tola.LodgementNotification             as Tola -- TODO: rename to disbursement notification
import qualified Tola.ChargeRequest                  as TChargeRequest
import qualified Tola.Common                         as Tola
import qualified Tola.LodgementNotification          as Tola
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

lodgementNotificationWeb :: WebMApp ()
lodgementNotificationWeb =
  post "/tola/lodgement_notification/" $ do
    mcn :: Either String Tola.LodgementNotification <- fmap A.eitherDecode body
    case mcn of
      Left err -> status (mkStatus 500 $ E.encodeUtf8 $ pack err) >> json (Tola.mkSuccessResponse False)
      Right cn -> do
        cnid <- fromIntegral . fromSqlKey <$> insertLodgementNotificationAndupdateChargeRequest cn
        addScotchHeader "LodgementNotificationId" (TL.pack $ show cnid)
        json $ Tola.mkSuccessResponse True


-- Client API

chargeRequestWeb :: WebMApp ()
chargeRequestWeb = getAndHead "/api/charge/:msisdn/:amount" $ do
  addHeader "Access-Control-Allow-Origin" "*"
  reqid <- header "X-RequestId"
  amount' <- Tola.mkAmount . (toRational :: Double -> Rational) <$> param "amount"
  msisdn' <- Tola.mkMsisdn <$> param "msisdn"
  now <- liftIO Clock.getCurrentTime
  cridKey <- addChargeRequest amount' msisdn'
  crid <- liftIO $ L.idToHex 10000  . fromIntegral . fromSqlKey $ cridKey
  addScotchHeader "ChargeRequestId" (TL.pack crid)
  let target = Tola.mkTarget "850702"
  secret <- readSecret
  let cr = TChargeRequest.mkChargeRequest secret target amount' msisdn' now (Tola.mkSourceReference . pack $ crid)
  resp <- runTola (`TolaInterface.makeChargeRequest` cr)
  updateChargeRequestWithResponse cridKey resp
  json (mkChargeRequestClientResponse (Tola.mkSourceReferenceFromString crid) resp)

