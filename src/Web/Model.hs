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

import           Control.Monad.IO.Class           (MonadIO (..), liftIO)
import           Control.Monad.Logger             (runStderrLoggingT)
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Data.Text                        (Text)
import qualified Data.Time                        as Time
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Reader             (asks)
import           Control.Monad.Reader.Class       (MonadReader)
import           Control.Monad.Trans.Class        (MonadTrans, lift)

import           Data.Pool                        (Pool)
import           Web.AppState

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Visit json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  campaignId Text
  landingPage Text
  deriving Show
|]

newtype AppStateM a = AppStateM {
    runAppStateM :: ReaderT AppState IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)


doMigrations :: (MonadReader AppState m, MonadIO m) => ReaderT AppState m ()
doMigrations = runDb (runMigration migrateAll)

doMigrationsWithPool pool = flip runSqlPersistMPool pool $
    runMigration migrateAll

runDb :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => ReaderT SqlBackend IO b -> t m b
runDb query = do
  pool <- lift $ asks getPool
  liftIO (runSqlPool query pool)

runApp :: (BaseBackend backend ~ SqlBackend, IsPersistBackend backend, MonadBaseControl IO m, MonadIO m) => ConnectionString -> (Pool backend -> IO a) -> m a
runApp connStr app =
  runStderrLoggingT $
    withPostgresqlPool connStr 10 $
    \pool -> liftIO $ app pool

addVisit :: (MonadTrans t, MonadReader AppState m, MonadIO (t m)) => Text -> Text -> t m (Key Visit)
addVisit campaignId landingPage = runDb (insert $ Visit campaignId landingPage)
