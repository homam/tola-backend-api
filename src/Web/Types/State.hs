module Web.Types.State where

import           Tola.RealTolaApi
import           Web.Logger
import           Web.Types.Logger

data AppState = AppState {
  appVaultLoggerKey :: VaultLoggerKey
, appTolaApiConfig  :: TolaApiConfig
}

instance HasTolaApiConfig AppState where
  tolaApiConfig = appTolaApiConfig
