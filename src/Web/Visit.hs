{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

module Web.Visit(
    doMigrationsWeb
  , msisdnSubmissionWeb
  , pinSubmissionWeb
)
where

import           Control.Monad        (join)
import           Data.Text            (Text, unpack, pack)
import qualified Data.Text.Lazy       as TL
import           Web.Model
import           Web.WebM
import Control.Monad.IO.Class (liftIO)
import qualified Sam.Robot as S
import qualified Data.ByteString as BS
import qualified Network.URI as U
import Control.Arrow
import qualified Data.Aeson           as A
import qualified  Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as M
import           GHC.Generics
import qualified Data.Text.Encoding as E
import Data.Monoid ((<>))
import Network.HTTP.Types.Status (status500)

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
    submissionErrorToText (S.NetworkError e) = pack $ show e
    submissionErrorToText (S.ValidationError _) = "Validation Failed"

data SubmissionResult = SubmissionResult {
      isValid :: Bool
    , errorText :: Maybe Text
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
