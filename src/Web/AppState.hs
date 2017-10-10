module Web.AppState (
  AppState (..)
) where

import Data.Text
import           Database.Persist.Postgresql

data AppState = AppState {
    echo :: Text -> IO ()
  , getPool :: ConnectionPool
}
