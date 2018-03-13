{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Tola.Database.MonadTolaDatabase (
  module Tola.Database.MonadTolaDatabase
, withDbPool
) where

import           Control.Monad.IO.Class              (MonadIO (..), liftIO)
import           Control.Monad.Reader                (asks)
import           Control.Monad.Reader.Class          (MonadReader)
import           Control.Monad.Trans.Class           (MonadTrans, lift)
import           Control.Monad.Trans.Reader          (ReaderT (..))
import           Data.Pool                           (Pool)
import           Database.Persist                    hiding (Update)
import           Database.Persist.Postgresql         hiding (Update)
import           Tola.Database.Helpers
import           Tola.Database.Model
import qualified Tola.Types.ChargeRequest            as ChargeRequest
import qualified Tola.Types.ChargeResponse           as ChargeResponse
import           Tola.Types.Common
import qualified Tola.Types.DisbursementNotification as DisbursementNotification
import qualified Tola.Types.LodgementNotification    as LodgementNotification

type TolaPool = Pool SqlBackend

class HasDbPool t where
  dbPool :: t -> TolaPool

class MonadTolaDatabase m where
  doMigrations :: m ()
  insertChargeRequest :: ChargeRequest.ChargeRequest -> m (Key DBChargeRequest)
  updateChargeRequestWithResponse :: Integer -> ChargeResponse.ChargeResponse -> m ()
  insertLodgementNotificationAndupdateChargeRequest :: LodgementNotification.LodgementNotification -> m (Key DBLodgementNotification)
  insertDisbursementNotificationAndupdateChargeRequest :: DisbursementNotification.DisbursementNotification -> m (Key DBDisbursementNotification)
  getChargeRequestStatus :: Integer -> m (Maybe ChargeRequest.ChargeRequestStatus)


--

insertChargeRequest' :: Insert ChargeRequest.ChargeRequest DBChargeRequest
insertChargeRequest' req = insert $
  DBChargeRequest   Nothing
                    Nothing
                    (ChargeRequest.amount req)
                    (ChargeRequest.msisdn req)
                    ChargeRequest.ChargeRequestCreated
                    Nothing
                    Nothing
                    Nothing
                    Nothing

updateChargeRequestWithResponse' :: Update Integer ChargeResponse.ChargeResponse
updateChargeRequestWithResponse' chargeRequestId =
  update (toSqlKey $ fromIntegral chargeRequestId) . fields
 where
  fields (ChargeResponse.SuccessChargeResponse ref) =
    [ DBChargeRequestState =. ChargeRequest.SuccessChargeResponseReceived
    , DBChargeRequestReference =. Just ref
    ]
  fields (ChargeResponse.FailureChargeResponse c m) =
    [ DBChargeRequestState =. ChargeRequest.FailChargeResponseReceived
    , DBChargeRequestResponseErrorCode =. Just c
    , DBChargeRequestResponseErrorMessage =. Just m
    ]

insertDisbursementNotificationAndupdateChargeRequest':: Insert DisbursementNotification.DisbursementNotification DBDisbursementNotification
insertDisbursementNotificationAndupdateChargeRequest' n = getChargeRequestBySourceReference (DisbursementNotification.sourcereference d)
    >>= \case
          Nothing                -> insert disbursementNotification -- just insert the notification
          Just (Entity creqid _) -> do
            notificationId <- insert $ disbursementNotification
            update
              creqid
              [ DBChargeRequestDisbursementNotificationId =. Just notificationId
              , DBChargeRequestState =. if s
                then ChargeRequest.SuccessDisbursementNotificationReceived
                else ChargeRequest.FailDisbursementNotificationReceived
              ]
            return notificationId
 where
  disbursementNotification = DBDisbursementNotification
    s
    e
    (DisbursementNotification.amount d)
    (DisbursementNotification.msisdn d)
    (DisbursementNotification.customerreference d)
    (DisbursementNotification.operatorreference d)
    (DisbursementNotification.sourcereference d)
    (DisbursementNotification.date d)
    (toSqlJSON n)

  d      = DisbursementNotification.details n
  (s, e) = DisbursementNotification.successAndError n

insertLodgementNotificationAndupdateChargeRequest' :: Insert LodgementNotification.LodgementNotification DBLodgementNotification
insertLodgementNotificationAndupdateChargeRequest' n = getChargeRequestBySourceReference (LodgementNotification.reference n)
    >>= \case
          Nothing                -> insert lodgementNotification -- just insert the notification
          Just (Entity creqid _) -> do
            notificationId <- insert lodgementNotification
            update
              creqid
              [ DBChargeRequestLodgementNotificationId =. Just notificationId
              , DBChargeRequestState
                =. ChargeRequest.SuccessLodgementNotificationReceived
              ]
            return notificationId
 where
  lodgementNotification = DBLodgementNotification
    (LodgementNotification.amount n)
    (LodgementNotification.msisdn n)
    (LodgementNotification.reference n)
    (LodgementNotification.customerreference n)
    (LodgementNotification.operatorreference n)
    (LodgementNotification.sourcereference n)
    (LodgementNotification.date n)
    (toSqlJSON n)

--

getChargeRequest :: Get Integer DBChargeRequest
getChargeRequest = getById

getChargeRequestBySourceReference :: Get SourceReference (Entity DBChargeRequest)
getChargeRequestBySourceReference sref =
  selectFirst [DBChargeRequestReference ==. Just sref] [Desc DBChargeRequestId]

getChargeRequestStatus' :: Get Integer ChargeRequest.ChargeRequestStatus
getChargeRequestStatus' = fmap (fmap go) . getChargeRequest
 where
  go o = ChargeRequest.mkChargeRequestStatus
    (dBChargeRequestState o)
    (dBChargeRequestReference o)
    (dBChargeRequestResponseErrorMessage o)


doMigrations' :: (backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m) => ReaderT backend m ()
doMigrations' = runMigration migrateAll

--

{-
type GetDB r r' = forall t m a
   . (HasDbPool r, MonadIO (t m), MonadReader r m, MonadTrans t, ToBackendKey SqlBackend r, Integral a)
  => a -> t m (Maybe r')

getDbByIntId :: GetDB r r
getDbByIntId creqid = runDb (get $ toSqlKey . fromIntegral $ creqid)
-}

runDb ::
     (HasDbPool r, MonadIO (t m), MonadReader r m, MonadTrans t)
  => ReaderT SqlBackend IO b
  -> t m b
runDb query = do
  pool <- lift $ asks dbPool
  liftIO $ runSqlPool query pool
