{-# LANGUAGE RankNTypes #-}

module Web.AppState (
  AppState (..)
) where

import Data.Text
import qualified Database.Redis as R
import qualified Database.Persist.Postgresql as DB
import           Control.Monad.Trans.Reader  (ReaderT (..))

data AppState = AppState {
    echo :: Text -> IO ()
  , runRedis :: forall a. R.Redis a -> IO a
  , runSql :: forall b. ReaderT DB.SqlBackend IO b -> IO b
}
