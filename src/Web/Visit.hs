{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Visit(
    doMigrationsWeb
  , msisdnSubmissionWeb
  , pinSubmissionWeb
  , msisdnExistsWeb
  , SubmissionResult (..)
  , lodgementRequestWeb
  , chargeRequestWeb
  , chargeNotificationWeb
)
where

import           Control.Arrow
import           Control.Monad             (join)
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.Aeson                as A
import qualified Data.Aeson.Types          as AT
import qualified Data.ByteString           as BS
import qualified Data.HashMap.Strict       as M
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack, unpack)
import qualified Data.Text.Encoding        as E
import qualified Data.Text.Lazy            as TL
import qualified Data.Time.Clock           as Clock
import           GHC.Generics
import           Network.HTTP.Types.Status (status500)
import qualified Network.URI               as U
import           Numeric                   (readHex, showHex)
import qualified Sam.Robot                 as S
import qualified Tola.ChargeNotification   as Tola
import qualified Tola.ChargeRequest        as TChargeRequest
import qualified Tola.Common               as Tola
import qualified Tola.LodgementRequest     as Tola
import qualified Tola.TolaInterface        as TolaInterface
import qualified Web.JewlModel             as JM
import           Web.Localization          (decrypt', encrypt, encrypt',
                                            toLocalMSISDN)
import           Web.Model
import           Web.WebM


doMigrationsWeb :: WebMApp ()
doMigrationsWeb =
  getAndHead "/do_migrations" $
    doMigrations >> text "done!"


toSubmissionResult :: Text -> Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> SubmissionResult
toSubmissionResult submissionId res = SubmissionResult {
    submissionId = submissionId
  , isValid = const False ||| const True $ res
  , errorText = Just . submissionErrorToText ||| const Nothing $ res
  } where
    submissionErrorToText (S.NetworkError e)    = pack $ show e
    submissionErrorToText (S.ValidationError _) = "Validation Failed"

data SubmissionResult = SubmissionResult {
      isValid      :: Bool
    , errorText    :: Maybe Text
    , submissionId :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance A.ToJSON SubmissionResult
instance A.FromJSON SubmissionResult

data FinalResult = FinalResult { finalUrl :: Text, finalSubmissionResult :: SubmissionResult }
instance A.ToJSON FinalResult where
  toJSON (FinalResult u s) = if isValid s then AT.Object (M.insert "finalUrl" v o) else A.toJSON s where
    AT.Object o = A.toJSON s
    v           = A.toJSON u

msisdnSubmissionWeb :: WebMApp ()
msisdnSubmissionWeb =
  getAndHead "/submit_msisdn/:domain/:country/:handle/:offer" $
    join $ msisdnSubmissionAction <$> param "domain" <*> param "country" <*> param "handle" <*> param "offer"
    <*> (pack <$> (toLocalMSISDN <$> (unpack <$> param "country") <*> (unpack <$> param "msisdn")))

msisdnSubmissionAction :: Text -> Text -> Text -> Int -> Text -> WebMAction ()
msisdnSubmissionAction domain country handle offer msisdn = do
  res <- liftIO $ S.runSubmission $ S.submitMSISDN (unpack domain) (unpack handle) (unpack country) offer (unpack msisdn)
  sid <- fromIntegral . fromSqlKey <$> addMSISDNSubmission domain country handle offer msisdn res
  let psid = pack . encrypt' . show $ sid
  addScotchHeader "SubmissionId" (TL.fromStrict psid)
  json $ toSubmissionResult psid res

pinSubmissionWeb :: WebMApp ()
pinSubmissionWeb =
  getAndHead "/submit_pin/" $ do
    msid <- decrypt' <$> param "sid"
    case msid of
      Left e    -> text (TL.pack $ show e)
      Right sid -> join $ pinSubmissionAction <$> (return $ read sid) <*> param "pin"

pinSubmissionAction :: Int -> Text -> WebMAction ()
pinSubmissionAction sid pin = do
  submission <- getMSISDNSubmission sid
  case (U.parseURI . unpack) =<< mSISDNSubmissionFinalUrl =<< submission of
    Just url -> do
      res <- liftIO $ S.runSubmission $ S.submitPIN (E.encodeUtf8 pin) url
      sid <- fromIntegral . fromSqlKey <$> addPINSubmission sid pin res
      let epsid =  encrypt' . show $ sid
      addScotchHeader "SubmissionId" (TL.pack epsid)
      json FinalResult { finalUrl = "http://gr.mobiworldbiz.com/?uid=fdf098fcc6&uip=2.84.0.0", finalSubmissionResult = toSubmissionResult (pack epsid) res }
    Nothing -> status status500 >> text ("No MSISDN Submission was Found for the Given sid: " <> TL.pack (show sid))

msisdnExistsWeb :: WebMApp ()
msisdnExistsWeb =
  getAndHead "/check_msisdn_active_subscription/:country/" $ do
    msisdn' <- param "msisdn"
    country' <- param "country"
    res <- JM.runJewlDb $ JM.msisdnStatus country' msisdn'
    json res

lodgementRequestWeb :: WebMApp ()
lodgementRequestWeb =
  postAndHead "/tola/lodgement_request/" $ do
    mlr :: Maybe Tola.LodgementRequest <- fmap A.decode body
    case mlr of
      Nothing -> status status500 >> json (Tola.mkSuccessResponse False)
      Just lr -> do
        lrid <- fromIntegral . fromSqlKey <$> addTolaRequest lr
        addScotchHeader "LodgementRequestId" (TL.pack $ show lrid)
        json $ Tola.mkSuccessResponse True

chargeRequestWeb :: WebMApp ()
chargeRequestWeb = getAndHead "/api/charge/:msisdn/:amount" $ do
  amount' <- Tola.mkAmount . (toRational :: Double -> Rational) <$> param "amount"
  msisdn' <- Tola.mkMsisdn <$> param "msisdn"
  now <- liftIO Clock.getCurrentTime
  cridKey <- addChargeRequest amount' msisdn'
  let crid = fromIntegral . fromSqlKey $ cridKey
  addScotchHeader "ChargeRequestId" (TL.pack $ show crid)
  let target = Tola.mkTarget "800000"
  let cr = TChargeRequest.mkChargeRequest (Tola.mkSecret "secret") target amount' msisdn' now (Tola.mkSourceReference $ pack $ show crid)
  resp <- runTola (`TolaInterface.makeChargeRequest` cr)
  updateChargeRequestWithResponse cridKey resp
  json resp --TODO: add crid to JSON response

chargeNotificationWeb :: WebMApp ()
chargeNotificationWeb =
  postAndHead "/tola/charge_notification/" $ do
    mcn :: Maybe Tola.ChargeNotification <- fmap A.decode body
    case mcn of
      Nothing -> status status500 >> json (Tola.mkSuccessResponse False)
      Just cn -> do
        cnid <- fromIntegral . fromSqlKey <$> insertChargeNotificationAndupdateChargeRequest cn
        addScotchHeader "ChargeNotificationId" (TL.pack $ show cnid)
        json $ Tola.mkSuccessResponse True

