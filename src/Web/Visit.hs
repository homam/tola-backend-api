{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
import           Network.HTTP.Types                    (parseQueryText)
import           Network.HTTP.Types.Status             (mkStatus, status500)
import           Tola.Types.ChargeRequest
import           Tola.Types.Common
import           Web.Crypto                            (decryptSXCode,
                                                        fromHexId, idToHex)
import           Web.Scotty.Trans
import           Web.ScottyHelpers
import           Web.Types.ChargeRequestClientResponse
import           Web.Types.WebApp
--
import           Control.Monad.Catch
import           Control.Monad.IO.Class                (MonadIO)
import           Data.Maybe                            (fromMaybe, listToMaybe)
import           Database.Persist.Class                (Key, ToBackendKey)
import           Database.Persist.Sql                  (SqlBackend)
import qualified Network.Wai                           as W

-- Tola

notificationWeb ::
  (A.FromJSON t,
   MonadTolaDatabase (ActionT e m),
   ToBackendKey SqlBackend b,
   MonadLogger (ActionT e m), MonadIO m,
   ScottyError e
  )
  =>  String
  -> TL.Text
  -> (t -> ActionT e m (Key b))
  -> ScottyT e m ()
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
  text ":)"
  -- req <- liftIO $ mkChargeRequest' (mkTarget "0000") (mkAmount 23) (mkMsisdn "0292883") (mkArbitraryReference "someref")
  -- _ <- insertChargeRequest req
  -- res <- makeChargeRequest (mkMockableChargeRequest MockSuccess req)
  -- json res

doMigrationsWeb :: WebApp
doMigrationsWeb = getAndHead "/do_migrations" $ doMigrations >>= text . TL.fromStrict . T.concat . map (T.append "\n")

allCampaignsWeb :: WebApp
allCampaignsWeb = getAndHead "/all_campaigns" $ getAllCampaigns >>= json

chargeRequestWeb :: WebApp
chargeRequestWeb = getAndHeadAccessOrigin "/api/charge/:msisdn/:amount/:arbitref" $ do
  amount' <- mkAmount . (toRational :: Double -> Rational) <$> param "amount"
  msisdn' <- mkMsisdn . sanitizeMsisdn <$> param "msisdn"
  arbitref <- mkArbitraryReference <$> param "arbitref"
  mock <- fromMaybe MockSuccess . maybeRead <$> (param "mock" `rescue` const (return ""))
  target' <- mkTarget <$> (param "target" `rescue` const (return "777200")) -- (return "850702"))
  -- Unknown campaign is 2
  campaignId' <- mkOuiSysCampaignId . fromMaybe 2 . maybeRead <$> param "cid" `rescue` const (return "2") -- either (const $ fst Campaigns.unknownCampaignId) mkCampaignId . decryptSXCode <$> param "sxcode" `rescue` const (return $ snd Campaigns.unknownCampaignId)
  qs <- queryStringParams

  -- let target' = mkTarget "777200"
  cr <- liftIO $ mkChargeRequest' target' amount' msisdn' arbitref
  cridKey <- insertChargeRequest cr campaignId' qs
  let crid = fromIntegral $ fromSqlKey cridKey
  catch (
    makeChargeRequest (mkMockableChargeRequest mock cr) >>= \case
      Right resp -> do
        updateChargeRequestWithResponse crid resp
        crid' <- liftIO $ (idToHex 10000 :: Integer -> IO String) crid
        addScotchHeader "ChargeRequestId" (TL.pack crid')
        json $ mkChargeRequestClientResponse (mkSourceReferenceFromString crid') resp
      Left err ->
        jsonError err
    )
    (jsonError . (displayException :: SomeException -> String))

    where

      sanitizeMsisdn = T.pack . go . T.unpack . T.replace " " "" where
        go full@('+' : '2' : '5' : '4' : _) = full
        go full@('2':'5':'4':_)             = full
        go ('0':xs)                         = go xs
        go x                                = "254" ++ x

checkChargeRequestWeb :: WebApp
checkChargeRequestWeb = getAndHeadAccessOrigin "/api/check_charge/:chargeRequestId" $ do
  eCreqId <- fromHexId 10000 <$> param "chargeRequestId"
  case eCreqId of
    Left e -> jsonError e
    Right (_, creqId) -> do
      mcreq <- getChargeRequestStatus creqId
      maybe (jsonError "No ChargeRequest was found") json mcreq

jsonError :: (ScottyError e, Monad m) => String -> ActionT e m ()
jsonError e = status status500 >> json (mkApiError e)

queryStringParams :: Monad m => ActionT TL.Text m [(T.Text, T.Text)]
queryStringParams = parseEncodedParams . W.rawQueryString <$> request where
  parseEncodedParams :: Char8.ByteString -> [(T.Text, T.Text)]
  parseEncodedParams bs =
    [ (k, fromMaybe "" v)
    | (k, v) <- parseQueryText bs
    ]

app :: WebApp
app   =  homeWeb
      >> lodgementNotificationWeb
      >> disbursementNotificationWeb
      >> chargeRequestWeb
      >> checkChargeRequestWeb
      >> doMigrationsWeb
      >> allCampaignsWeb


maybeRead :: Read a => String -> Maybe a 
maybeRead = fmap fst . listToMaybe . reads
