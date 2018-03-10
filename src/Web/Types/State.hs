module Web.Types.State where

import           Tola.RealTolaApi
import           Web.Types.Logger

data AppState = AppState {
  appWriteLog      :: Logger
, appTolaApiConfig :: TolaApiConfig
}

instance HasLogger AppState where
  writeLog = appWriteLog

instance HasTolaApiConfig AppState where
  tolaApiConfig = appTolaApiConfig
