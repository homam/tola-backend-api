{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Control.Monad.Trans.Reader          (ReaderT (..))
import           Data.Aeson                          ((.=))
import qualified Data.Aeson                          as A
import qualified Data.Map                            as M
import           Data.Pool                           (Pool)
import           Data.Text                           (Text, unpack)
import           Database.Persist                    hiding (Update)
import           Database.Persist.Postgresql         hiding (Update)
import           Database.Persist.Postgresql.Json    (Json (..))
import           Database.Persist.Sql                (showMigration)
import qualified Pixels.Types
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
  doMigrations :: m [Text]
  insertChargeRequest :: ChargeRequest.ChargeRequest -> m (Key DBChargeRequest)
  updateChargeRequestWithResponse :: Integer -> ChargeResponse.ChargeResponse -> m ()
  insertLodgementNotificationAndupdateChargeRequest :: LodgementNotification.LodgementNotification -> m (Key DBLodgementNotification)
  insertDisbursementNotificationAndupdateChargeRequest :: DisbursementNotification.DisbursementNotification -> m (Key DBDisbursementNotification)
  getChargeRequestStatus :: Integer -> m (Maybe ChargeRequest.ChargeRequestStatus)
  getAllCampaigns :: m [String]


--

-- insertChargeRequest' :: Insert ChargeRequest.ChargeRequest DBChargeRequest
insertChargeRequest' req = do
  campaignId <- fmap getCampaignId <$> selectFirst [DBCampaignOuisysCampaignId ==. ChargeRequest.ouiSysCampaignId req] []
  insert $
    DBChargeRequest   Nothing
                      Nothing
                      (ChargeRequest.amount req)
                      (ChargeRequest.msisdn req)
                      ChargeRequest.ChargeRequestCreated
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      campaignId -- (Just $ toSqlKey $ fromIntegral $ unOuiSysCampaignId $ ChargeRequest.ouiSysCampaignId req)
                      (Just $ Json $ A.object $ map (\(k, v) -> k .= v) $ ChargeRequest.queryString req)
  where
  getCampaignId :: Entity DBCampaign -> Key DBCampaign
  getCampaignId (Entity k _) = k

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


doMigrations' :: (backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m) => ReaderT backend m [Text]
doMigrations' = showMigration migrateAll

-- Campaigns and Pixels

getCampaign :: Get Integer DBCampaign
getCampaign = getById


{-
getAllCampaigns' :: (BaseBackend backend ~ SqlBackend, PersistQueryRead backend
      , MonadIO m
      , MonadThrow m, MonadBase IO m, MonadBaseControl IO m
      , HasPersistBackend backend) =>
     ReaderT backend m [String]

getAllCampaigns' =
  runResourceT (rawQuery sql [] $$ CL.map cast =$ CL.consume)
  where
  sql :: Text
  sql
    = "select C.id as CampaignId, C.name as CampaignName, C.is_active as CampaignIsActive, P.id as PixelId, P.url as PixelUrl from campaigns as C left join pixel_templates as P on C.pixel_template_id = P.id"
  cast (PersistInt64 campaignId : PersistText campaignName : _) = unpack campaignName
  cast _                           = error "Unable to parse query result"
-}

getAllCampaigns' :: MonadIO m => ReaderT SqlBackend m [String]
getAllCampaigns' = map (\(_cr, Entity _ c, _p) -> unpack $ dBCampaignName c ) <$> getPixelsToBeFired


-- getPixelsToBeFired
--   :: MonadIO m => ReaderT SqlBackend m [(Entity DBCampaign, Maybe (Entity DBPixelTemplate))]
-- getPixelsToBeFired =
--   rawSql
--     "select ??, ?? from campaigns left join pixel_templates on campaigns.pixel_template_id = pixel_templates.id"
--     []

getPixelsToBeFired ::
     MonadIO m
  => ReaderT SqlBackend m [(Entity DBChargeRequest, Entity DBCampaign, Entity DBPixelTemplate)]
getPixelsToBeFired =
    rawSql "select ??, ??, ?? from charge_request  \n\
      \ inner join campaigns on charge_request.campaign_id = campaigns.id \n\
      \ inner join pixel_templates on campaigns.pixel_template_id = pixel_templates.id \n\
      \ where charge_request.state = 'SuccessDisbursementNotificationReceived' \n\
      \   and charge_request.creation_time > now() - 24 * interval '1 hour' \n\
      \   and (select count(*) from pixels where pixels.charge_request_id = charge_request.id) = 0 \n\
      \ ; \n\
      \ "
      []

getAllPixelsToBeFired' ::
     MonadIO m
  => ReaderT SqlBackend m [(Text, Key DBChargeRequest, Url, Msisdn, Pixels.Types.QueryString)]
getAllPixelsToBeFired' = map go <$> getPixelsToBeFired where
  go (Entity crKey cr, Entity _ c, Entity _ px) =
    ( dBCampaignName c
    , crKey
    , mkUrl $ dBPixelTemplateUrl px
    , dBChargeRequestMsisdn cr
    , maybe M.empty jsonToMap $ (dBChargeRequestQueryString cr)
    )


jsonToMap :: Json -> Pixels.Types.QueryString
jsonToMap (Json v) = Pixels.Types.queryStringFromJson v


insertAPixel' :: Insert (Key DBChargeRequest, Url, Bool, Int, Text) DBPixel
insertAPixel' (chargeRequestId, url, success', responseStatusCode, response) = insert $
  DBPixel
    chargeRequestId
    url
    (Just success')
    (Just responseStatusCode)
    (Just response)
    Nothing -- headers
    Nothing -- pixel amount

class Monad m => MonadPixelsDatabase m where
  getAllPixelsToBeFired :: m [(Text, Key DBChargeRequest, Url, Msisdn, Pixels.Types.QueryString)]
  insertAPixel :: (Key DBChargeRequest, Url, Bool, Int, Text) -> m (Key DBPixel)

--

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


runDb' ::
     ( MonadBaseControl IO m
     , HasDbPool r
     , MonadReader r m
     )
  => ReaderT SqlBackend m b
  -> m b
runDb' query = do
  pool <- asks dbPool
  runSqlPool query pool
