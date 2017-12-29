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
import qualified Tola.ChargeRequest          as TChargeRequest
import           Tola.Common
import           Tola.LodgementRequest
import           Tola.TolaInterface          (TolaInterface)


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

DBChargeRequest sql=charge_request json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  amount Amount sqltype=numeric(14,5)
  msisdn Msisdn
  state TChargeRequest.ChargeRequestState
  reference Text Maybe
|]

newtype AppStateM a = AppStateM {
    runAppStateM :: ReaderT AppState IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

newSubmissionId
  :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => t m Int
newSubmissionId =
  liftIO (round . (*1000000) . fromRational . toRational <$> POSIX.getPOSIXTime)

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

addMSISDNSubmission :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => Text -> Text -> Text -> Int -> Text -> Either (S.SubmissionError S.HttpException BS.ByteString) U.URI -> t m (Key MSISDNSubmission)
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
addTolaRequest req = runDb (insert $ DBLodgementRequest (amount req) (msisdn req) (date req) (E.decodeUtf8 $ BL.toStrict $ A.encode req))

addChargeRequest :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => Amount -> Msisdn -> t m (Key DBChargeRequest)
addChargeRequest a m = runDb (insert $ DBChargeRequest a m TChargeRequest.ChargeRequestCreated Nothing)

updateChargeRequest = ()

--

runTola :: (MonadReader AppState m, Monad (t m), MonadTrans t) => (TolaInterface -> t m b) -> t m b
runTola f = do
  ti <- lift $ asks tolaInterface
  f ti
