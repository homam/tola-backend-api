{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Tola.Database.MonadTolaDatabase where

import           Control.Monad.IO.Class              (MonadIO (..), liftIO)
import           Control.Monad.Logger                (runNoLoggingT)
import           Control.Monad.Reader                (asks)
import           Control.Monad.Reader.Class          (MonadReader)
import           Control.Monad.Trans.Class           (MonadTrans, lift)
import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Control.Monad.Trans.Reader          (ReaderT (..))
import qualified Data.Aeson                          as A
import qualified Data.ByteString.Lazy                as BL
import           Data.Pool                           (Pool)
import           Data.Text                           (Text)
import qualified Data.Text.Encoding                  as E
import qualified Data.Time                           as Time
import qualified Data.Time.Clock.POSIX               as POSIX
import           Database.Persist
import           Database.Persist.Postgresql
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
  insertChargeRequest :: ChargeRequest.ChargeRequest -> m (Key DBChargeRequest)

insertChargeRequest' :: forall r (t :: (* -> *) -> * -> *) (m :: * -> *)
   . (MonadTrans t, MonadReader r m, MonadIO (t m), HasDbPool r)
  => ChargeRequest.ChargeRequest -> t m (Key DBChargeRequest)
insertChargeRequest' req = runDb $
  insert $ DBChargeRequest  Nothing
                            Nothing
                            (ChargeRequest.amount req)
                            (ChargeRequest.msisdn req)
                            ChargeRequest.ChargeRequestCreated
                            Nothing
                            Nothing
                            Nothing
                            Nothing


runDb ::
     (HasDbPool r, MonadIO (t m), MonadReader r m, MonadTrans t)
  => ReaderT SqlBackend IO b
  -> t m b
runDb query = do
  pool <- lift $ asks dbPool
  liftIO $ runSqlPool query pool


withDbPool ::
     ( BaseBackend backend ~ SqlBackend
     , IsPersistBackend backend
     , MonadBaseControl IO m
     , MonadIO m
     )
  => ConnectionString
  -> (Pool backend -> IO a)
  -> m a
withDbPool connStr appf =
  runNoLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ appf pool
