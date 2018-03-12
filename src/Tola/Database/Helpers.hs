{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Tola.Database.Helpers where

import           Control.Monad.IO.Class      (MonadIO (..), liftIO)
import           Control.Monad.Logger        (runNoLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader  (ReaderT (..))
import qualified Data.Aeson                  as A
import qualified Data.ByteString.Lazy        as BL
import           Data.Pool                   (Pool)
import qualified Data.Text                   as E
import qualified Data.Text.Encoding          as E
import           Database.Persist
import           Database.Persist.Postgresql

type Get i o = forall (m :: * -> *) backend
  .  (BaseBackend backend ~ SqlBackend, PersistQueryRead backend, MonadIO m, PersistQueryRead backend)
  => i -> ReaderT backend m (Maybe o)

type Insert i o = forall (m :: * -> *) backend
  .  (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m, PersistQueryRead backend)
  => i -> ReaderT backend m (Key o)

type UpdateWithKey k i = forall (m :: * -> *) backend
   . (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Key k -> i -> ReaderT backend m ()


getById :: (Integral i, PersistEntityBackend o ~ SqlBackend, PersistEntity o, ToBackendKey SqlBackend o) => Get i o
getById creqid = get $ toSqlKey . fromIntegral $ creqid

--

toSqlJSON :: A.ToJSON a => a -> E.Text
toSqlJSON = E.decodeUtf8 . BL.toStrict . A.encode

--


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
