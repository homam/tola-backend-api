{-# LANGUAGE RankNTypes #-}

module Web.AppState (
  AppState (..)
) where

import           Control.Monad.Trans.Reader  (ReaderT (..))
import           Data.ByteString             (ByteString)
import           Data.Text
import qualified Database.Persist.Postgresql as DB
import qualified Database.PostgreSQL.Simple  as PS
import qualified Database.Redis              as R
import           System.Log.FastLogger       (ToLogStr)
import           Tola.Common                 (Secret)
import qualified Tola.TolaInterface          as Tola


data AppState = AppState {
    echo     :: Text -> IO ()
  , runRedis :: forall a. R.Redis a -> IO a
  , runSql   :: forall b. ReaderT DB.SqlBackend IO b -> IO b
  , runJewl  :: forall b. (PS.Connection -> IO b) -> IO b
  , tolaApi  :: Tola.TolaApi
  , logIO    :: ByteString -> IO () -- forall msg. ToLogStr msg => msg -> IO ()
  , secret   :: Secret
}
