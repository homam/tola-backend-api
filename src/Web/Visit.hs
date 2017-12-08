{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Visit(
    doMigrationsWeb
  , msisdnSubmissionWeb
  , pinSubmissionWeb
  , msisdnExistsWeb
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
import           GHC.Generics
import           Network.HTTP.Types.Status (status500)
import qualified Network.URI               as U
import qualified Sam.Robot                 as S
import qualified Web.JewlModel             as JM
import           Web.Model
import           Web.WebM

doMigrationsWeb :: WebMApp ()
doMigrationsWeb =
  getAndHead "/do_migrations" $
    doMigrations >> text "done!"


toSubmissionResult :: Int -> Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> SubmissionResult
toSubmissionResult k res = SubmissionResult {
    submissionId = k
  , isValid = const False ||| const True $ res
  , errorText = Just . submissionErrorToText ||| const Nothing $ res
  } where
    submissionErrorToText (S.NetworkError e)    = pack $ show e
    submissionErrorToText (S.ValidationError _) = "Validation Failed"

data SubmissionResult = SubmissionResult {
      isValid      :: Bool
    , errorText    :: Maybe Text
    , submissionId :: Int
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
    join $ msisdnSubmissionAction <$> param "domain" <*> param "country" <*> param "handle" <*> param "offer" <*> param "msisdn"

msisdnSubmissionAction :: Text -> Text -> Text -> Int -> Text -> WebMAction ()
msisdnSubmissionAction domain country handle offer msisdn = do
  res <- liftIO $ S.runSubmission $ S.submitMSISDN (unpack domain) (unpack handle) (unpack country) offer (unpack msisdn)
  sid <- fromIntegral . fromSqlKey <$> addMSISDNSubmission domain country handle offer msisdn res
  addScotchHeader "SubmissionId" (TL.pack $ show sid)
  json $ toSubmissionResult sid res


pinSubmissionWeb :: WebMApp ()
pinSubmissionWeb =
  getAndHead "/submit_pin/" $
    join $ pinSubmissionAction <$> param "sid" <*> param "pin"

pinSubmissionAction :: Int -> Text -> WebMAction ()
pinSubmissionAction sid pin = do
  submission <- getMSISDNSubmission sid
  case (U.parseURI . unpack) =<< mSISDNSubmissionFinalUrl =<< submission of
    Just url -> do
      res <- liftIO $ S.runSubmission $ S.submitPIN (E.encodeUtf8 pin) url
      psid <- fromIntegral . fromSqlKey <$> addPINSubmission sid pin res
      addScotchHeader "SubmissionId" (TL.pack $ show psid)
      json FinalResult { finalUrl = "http://gr.mobiworldbiz.com/?uid=fdf098fcc6&uip=2.84.0.0", finalSubmissionResult = toSubmissionResult psid res }
    Nothing -> status status500 >> text ("No MSISDN Submission was Found for the Given sid: " <> TL.pack (show sid))

msisdnExistsWeb :: WebMApp ()
msisdnExistsWeb =
  getAndHead "/check_msisdn_active_subscription/:country/" $ do
    msisdn' <- param "msisdn"
    country' <- param "country"
    res <- JM.runJewlDb $ JM.msisdnStatus country' msisdn'
    json res
