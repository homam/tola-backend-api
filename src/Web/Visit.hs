{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Visit(
    doMigrationsWeb
  , lodgementNotificationWeb
  , chargeRequestWeb
  , echoWeb
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
import           Network.HTTP.Types.Status           (status500)
-- import qualified Tola.LodgementNotification             as Tola -- TODO: rename to disbursement notification
import qualified Tola.ChargeRequest                  as TChargeRequest
import qualified Tola.Common                         as Tola
import qualified Tola.LodgementNotification          as Tola
import qualified Tola.TolaInterface                  as TolaInterface
import           Web.Api.ChargeRequestClientResponse (mkChargeRequestClientResponse)
import           Web.Localization                    (decrypt', encrypt,
                                                      encrypt', toLocalMSISDN)
import           Web.Model
import           Web.WebM


doMigrationsWeb :: WebMApp ()
doMigrationsWeb =
  getAndHead "/do_migrations" $
    doMigrations >> text "done!"

echoWeb :: WebMApp ()
echoWeb = getAndHead "/tola/echo/:message" $ text =<< param "message"

-- Tola API

lodgementNotificationWeb :: WebMApp ()
lodgementNotificationWeb =
  postAndHead "/tola/lodgement_notification/" $ do

    mcn :: Maybe Tola.LodgementNotification <- fmap A.decode body
    case mcn of
      Nothing -> status status500 >> json (Tola.mkSuccessResponse False)
      Just cn -> do
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
  let crid = fromIntegral . fromSqlKey $ cridKey
  addScotchHeader "ChargeRequestId" (TL.pack $ show crid)
  let target = Tola.mkTarget "800000"
  secret <- readSecret
  let cr = TChargeRequest.mkChargeRequest secret target amount' msisdn' now (Tola.mkSourceReference $ pack $ show crid)
  resp <- runTola (`TolaInterface.makeChargeRequest` cr)
  updateChargeRequestWithResponse cridKey resp
  json (mkChargeRequestClientResponse (Tola.mkSourceReferenceFromInt crid) resp)

