{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Web.Model
    (
      module Web.Model
    ) where

import           Control.Monad.IO.Class      (MonadIO (..), liftIO)
import           Control.Monad.Logger        (runNoLoggingT)
import           Control.Monad.Trans.Reader  (ReaderT (..))
import           Data.Text                   (Text, pack)
import qualified Data.Time                   as Time
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import           Control.Monad.Reader        (asks)
import           Control.Monad.Reader.Class  (MonadReader)
import           Control.Monad.Trans.Class   (MonadTrans, lift)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.Pool                   (Pool)
import           Web.AppState

import           Control.Arrow
import qualified Data.Aeson                  as A
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text.Encoding          as E
import           Data.Time                   (UTCTime)
import qualified Data.Time.Clock.POSIX       as POSIX
import qualified Database.Redis              as R
import qualified Network.URI                 as U
import qualified Sam.Robot                   as S
import qualified Tola.ChargeNotification     as TChargeNotification
import qualified Tola.ChargeRequest          as TChargeRequest
import qualified Tola.ChargeResponse         as TChargeResponse
import           Tola.Common
import           Tola.LodgementRequest
import           Tola.TolaInterface          (TolaApi)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MSISDNSubmission sql=msisdn_submissions json
  Id
  submissionId Int Maybe
  creationTime Time.UTCTime default=now() MigrationOnly
  country Text
  handle Text
  domain Text
  offer Int
  msisdn Text
  isValid Bool
  errorText Text Maybe
  finalUrl Text Maybe
  deriving Show

PINSubmission sql=pin_submissions json
  Id
  submissionId Int Maybe
  creationTime Time.UTCTime default=now() MigrationOnly
  msisdnSubmissionId MSISDNSubmissionId
  pin Text
  isValid Bool
  errorText Text Maybe
  finalUrl Text Maybe
  deriving Show

DBLodgementRequest sql=lodgement_requests json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  amount Amount sqltype=numeric(14,5)
  msisdn Msisdn
  date UTCTime
  rawRequest Text sqltype=json

DBChargeNotification sql=charge_notification json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  successful Bool
  errorMessage Text Maybe
  amount Amount sqltype=numeric(14,5)
  msisdn Msisdn
  customerReference CustomerReference
  operatorReference OperatorReference
  sourceReference SourceReference
  rawNotification Text Maybe sqltype=json


DBChargeRequest sql=charge_request json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  chargeNotificationId DBChargeNotificationId Maybe
  amount Amount sqltype=numeric(14,5)
  msisdn Msisdn
  state TChargeRequest.ChargeRequestState sqltype=chargerequeststate
  reference SourceReference Maybe
  responseErrorCode Int Maybe
  responseErrorMessage Text Maybe
  rawResponse Text Maybe sqltype=json
|]

newtype AppStateM a = AppStateM {
    runAppStateM :: ReaderT AppState IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

newSubmissionId
  :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => t m Int
newSubmissionId =
  liftIO (round . (*1000000) . fromRational . toRational <$> POSIX.getPOSIXTime)

toSqlJSON :: A.ToJSON a => a -> Text
toSqlJSON = E.decodeUtf8 . BL.toStrict . A.encode

doMigrationsWithPool :: Pool SqlBackend -> IO ()
doMigrationsWithPool pool = flip runSqlPersistMPool pool $
    runMigration migrateAll

runDb :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => ReaderT SqlBackend IO b -> t m b
runDb query = do
  run <- lift $ asks runSql
  liftIO (run query)

runRedisCommand :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => R.Redis b -> t m b
runRedisCommand command = do
  run <- lift $ asks runRedis
  liftIO (run command)

runApp :: (BaseBackend backend ~ SqlBackend, IsPersistBackend backend, MonadBaseControl IO m, MonadIO m) => ConnectionString -> (Pool backend -> IO a) -> m a
runApp connStr appf =
  runNoLoggingT $
    withPostgresqlPool connStr 10 $
    \pool -> liftIO $ appf pool

doMigrations :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => t m ()
doMigrations = runDb (runMigration migrateAll)

addMSISDNSubmission :: (MonadTrans t, MonadReader AppState m, MonadIO (t m))
  => Text -> Text -> Text -> Int -> Text -> Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> t m (Key MSISDNSubmission)
addMSISDNSubmission domain country handle offer msisdn res = do
  submissionId <- newSubmissionId
  let obj = addValidationRes res $ MSISDNSubmission (Just submissionId) country handle domain offer msisdn
  runDb (insert obj)
  -- sid <- runDb (insert obj)
  -- runRedisCommand (R.setOpts (E.encodeUtf8 $ pack $ show $ fromIntegral $ fromSqlKey sid) (E.encodeUtf8 $ toJsonText (sid, obj) ) (R.SetOpts (Just 60) Nothing Nothing))
  -- return sid

addPINSubmission :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => Int -> Text -> Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> t m (Key PINSubmission)
addPINSubmission msisdnSubmissionKey pin res = do
  submissionId <- newSubmissionId
  runDb (insert $ addValidationRes res $ PINSubmission (Just submissionId) (toSqlKey $ fromIntegral msisdnSubmissionKey) pin)

getMSISDNSubmission :: (MonadIO (t m), MonadReader AppState m, MonadTrans t, ToBackendKey SqlBackend MSISDNSubmission, Integral a) => a -> t m (Maybe MSISDNSubmission)
getMSISDNSubmission sid = runDb (get $ toSqlKey . fromIntegral  $ sid)

addValidationRes :: Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> (Bool -> Maybe Text -> Maybe Text -> a) -> a
addValidationRes res f = f (const False ||| const True $ res) (Just . submissionErrorToText ||| const Nothing $ res) (const Nothing ||| Just . pack . ($ "") . U.uriToString id $ res)
  where
    submissionErrorToText (S.NetworkError e)     = pack $ show e
    submissionErrorToText (S.ValidationError bs) = E.decodeUtf8 bs

addTolaRequest :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => LodgementRequest -> t m (Key DBLodgementRequest)
addTolaRequest req = runDb (insert $ DBLodgementRequest (amount req) (msisdn req) (date req) (toSqlJSON req))

addChargeRequest :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => Amount -> Msisdn -> t m (Key DBChargeRequest)
addChargeRequest a m = runDb (insert $ DBChargeRequest Nothing a m TChargeRequest.ChargeRequestCreated Nothing Nothing Nothing Nothing)

updateChargeRequestWithResponse :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => Key DBChargeRequest -> TChargeResponse.ChargeResponse -> t m ()
updateChargeRequestWithResponse chargeRequestId  = runDb . update chargeRequestId . fields where
  fields (TChargeResponse.SuccessChargeResponse ref) =
    [ DBChargeRequestState =. TChargeRequest.SuccessChargeResponseReceived
    , DBChargeRequestReference =. Just ref
    ]
  fields (TChargeResponse.FailureChargeResponse c m) =
    [ DBChargeRequestState =. TChargeRequest.FailChargeResponseReceived
    , DBChargeRequestResponseErrorCode =. Just c
    , DBChargeRequestResponseErrorMessage =. Just m
    ]

insertChargeNotificationAndupdateChargeRequest :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) =>
  TChargeNotification.ChargeNotification -> t m (Key DBChargeNotification)
insertChargeNotificationAndupdateChargeRequest n =
  runDb $ do
    notificationId <- insert $ fromChargeNotification n
    updateWhere
      [DBChargeRequestReference ==. Just (TChargeNotification.sourcereference d)]
      [
          DBChargeRequestChargeNotificationId =. Just notificationId
        , DBChargeRequestState =. status n
      ]
    return notificationId
    where
      fromChargeNotification (TChargeNotification.SuccessChargeNotification _) = mkDBChargeNotification True Nothing
      fromChargeNotification (TChargeNotification.FailureChargeNotification e _) = mkDBChargeNotification False (Just e)

      status (TChargeNotification.SuccessChargeNotification _) = TChargeRequest.SuccessChargeNotificationReceived
      status (TChargeNotification.FailureChargeNotification e _) = TChargeRequest.FailChargeNotificationReceived

      mkDBChargeNotification success errorText =
        DBChargeNotification
          success
          errorText
          (TChargeNotification.amount d)
          (TChargeNotification.msisdn d)
          (TChargeNotification.customerreference d)
          (TChargeNotification.operatorreference d)
          (TChargeNotification.sourcereference d)
          (Just $ toSqlJSON d)
      d = TChargeNotification.details n



--

runTola :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => (TolaApi -> IO b) -> t m b
runTola f = do
  ti <- lift $ asks tolaApi
  liftIO (f ti)
