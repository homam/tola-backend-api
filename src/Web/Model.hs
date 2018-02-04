{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Web.Model
    (
      module Web.Model
    ) where

import           Control.Monad.IO.Class        (MonadIO (..), liftIO)
import           Control.Monad.Logger          (runNoLoggingT)
import           Control.Monad.Trans.Reader    (ReaderT (..))
import           Data.Text                     (Text)
import qualified Data.Time                     as Time
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import           Control.Monad.Reader          (asks)
import           Control.Monad.Reader.Class    (MonadReader)
import           Control.Monad.Trans.Class     (MonadTrans, lift)
import           Control.Monad.Trans.Control   (MonadBaseControl)

import           Data.Pool                     (Pool)
import           Web.AppState

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text.Encoding            as E
import           Data.Time                     (UTCTime)
import qualified Data.Time.Clock.POSIX         as POSIX
import qualified Database.Redis                as R
import qualified Tola.ChargeRequest            as TChargeRequest
import qualified Tola.ChargeResponse           as TChargeResponse
import           Tola.Common
import qualified Tola.DisbursementNotification as TDisbursementNotification
import qualified Tola.LodgementNotification    as TLodgementNotification
import           Tola.TolaInterface            (TolaApi)
import qualified Web.Localization              as L


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DBLodgementNotification sql=lodgement_notifications json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  amount Amount sqltype=numeric(14,5)
  msisdn Msisdn
  reference Reference
  customerReference CustomerReference
  operatorReference OperatorReference
  sourceReference SourceReference
  date UTCTime
  rawNotification Text sqltype=json

DBDisbursementNotification sql=disbursement_notifications json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  success Bool
  errorMessage Text Maybe
  amount Amount sqltype=numeric(14,5)
  msisdn Msisdn
  customerReference CustomerReference
  operatorReference OperatorReference
  sourceReference SourceReference
  date UTCTime
  rawNotification Text sqltype=json

DBChargeRequest sql=charge_request json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  lodgementNotificationId DBLodgementNotificationId Maybe
  disbursementNotificationId DBDisbursementNotificationId Maybe
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
  liftIO ((round :: Double -> Int) . (*1000000) . fromRational . toRational <$> POSIX.getPOSIXTime)

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

runApp :: (BaseBackend backend ~ SqlBackend, IsPersistBackend backend, MonadBaseControl IO m, MonadIO m) =>
  ConnectionString -> (Pool backend -> IO a) -> m a
runApp connStr appf =
  runNoLoggingT $
    withPostgresqlPool connStr 10 $
    \pool -> liftIO $ appf pool

doMigrations :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => t m ()
doMigrations = runDb (runMigration migrateAll)

-- addTolaRequest :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => TLodgementNotification.LodgementNotification -> t m (Key DBLodgementNotification)
-- addTolaRequest req = runDb (insert $ DBLodgementNotification (amount req) (msisdn req) (date req) (toSqlJSON req))

addChargeRequest :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => Amount -> Msisdn -> t m (Key DBChargeRequest)
addChargeRequest a m = runDb (insert $ DBChargeRequest Nothing Nothing a m TChargeRequest.ChargeRequestCreated Nothing Nothing Nothing Nothing)

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

insertLodgementNotificationAndupdateChargeRequest :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) =>
  TLodgementNotification.LodgementNotification -> t m (Key DBLodgementNotification)
insertLodgementNotificationAndupdateChargeRequest n =
  case toSqlKey . fromIntegral . snd <$> L.fromHexId 10000 (unpack $ unSourceReference $ TLodgementNotification.sourcereference n) of
    Left _ -> runDb $ insert lodgementNotification
    Right creqid -> runDb $ do
      notificationId <- insert $ lodgementNotification
      update
        creqid
        [
            DBChargeRequestLodgementNotificationId =. Just notificationId
          , DBChargeRequestState =. TChargeRequest.SuccessLodgementNotificationReceived
        ]
      return notificationId
  where
    lodgementNotification =
      DBLodgementNotification
        (TLodgementNotification.amount n)
        (TLodgementNotification.msisdn n)
        (TLodgementNotification.reference n)
        (TLodgementNotification.customerreference n)
        (TLodgementNotification.operatorreference n)
        (TLodgementNotification.sourcereference n)
        (TLodgementNotification.date n)
        (toSqlJSON n)

insertDisbursementNotificationAndupdateChargeRequest :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) =>
  TDisbursementNotification.DisbursementNotification -> t m (Key DBDisbursementNotification)
insertDisbursementNotificationAndupdateChargeRequest n =
  case toSqlKey . fromIntegral . snd <$> L.fromHexId 10000 (unpack $ unSourceReference $ TDisbursementNotification.sourcereference d) of
    Left _ -> runDb $ insert disbursementNotification
    Right creqid -> runDb $ do
      notificationId <- insert $ disbursementNotification
      update
        creqid
        [
            DBChargeRequestDisbursementNotificationId =. Just notificationId
          , DBChargeRequestState =. if s then TChargeRequest.SuccessDisbursementNotificationReceived else TChargeRequest.FailDisbursementNotificationReceived
        ]
      return notificationId
  where
    disbursementNotification =
      DBDisbursementNotification
        s
        e
        (TDisbursementNotification.amount d)
        (TDisbursementNotification.msisdn d)
        (TDisbursementNotification.customerreference d)
        (TDisbursementNotification.operatorreference d)
        (TDisbursementNotification.sourcereference d)
        (TDisbursementNotification.date d)
        (toSqlJSON n)

    d = TDisbursementNotification.details n
    (s, e) = TDisbursementNotification.successAndError n

getChargeRequest :: GetDB DBChargeRequest DBChargeRequest
getChargeRequest = getDbByIntId

--
getChargeRequestStatus :: GetDB DBChargeRequest TChargeRequest.ChargeRequestStatus
getChargeRequestStatus = fmap (fmap go) . getChargeRequest where
  go o = TChargeRequest.mkChargeRequestStatus
    (dBChargeRequestState o)
    (dBChargeRequestReference o)
    (dBChargeRequestResponseErrorMessage o)

--

type GetDB r r' = forall t m a. (MonadIO (t m), MonadReader AppState m, MonadTrans t, ToBackendKey SqlBackend r, Integral a)
  => a -> t m (Maybe r')

getDbByIntId :: GetDB r r
getDbByIntId creqid = runDb (get $ toSqlKey . fromIntegral $ creqid)
--

runTola :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) =>
  (TolaApi -> IO b) -> t m b
runTola f = do
  ti <- lift $ asks tolaApi
  liftIO (f ti)

readSecret :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) =>
  t m Secret
readSecret = lift $ asks secret

-- | Log a Text
--
-- > someRouteWeb :: WebMApp ()
-- > someRouteWeb = getAndHead "/" $ do
-- > ua <- header "user-agent"
-- > logText . ("User-Agent: " <>) $ TL.toStrict (fromMaybe "N/A" ua)
-- > ...
--
logText :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => Text -> t m ()
logText m = do
  l <- lift $ asks logIO
  liftIO (l $ E.encodeUtf8 m)
