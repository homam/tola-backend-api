module Web.Types.State where

import           Tola.RealTolaApi
import           Web.Logging.Logger

data AppState = AppState {
  appVaultLoggerKey :: VaultLoggerKey
, appTolaApiConfig  :: TolaApiConfig
}

instance HasTolaApiConfig AppState where
  tolaApiConfig = appTolaApiConfig

instance HasVaultLoggerKey AppState where
  vaultLoggerKey = appVaultLoggerKey
