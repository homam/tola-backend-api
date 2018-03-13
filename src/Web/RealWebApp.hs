{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Web.RealWebApp where

import           Control.Monad.Reader
import qualified Data.Text.Lazy                       as TL
import qualified Data.Vault.Lazy                      as V
import           Tola.Database.MonadTolaDatabase
import           Tola.MonadTolaApi
import           Tola.RealTolaApi
import           Tola.Types.Common                    (MACed (..), ToMACed (..),
                                                       mkSecret)
import           Web.Logging.DetailedLoggerMiddleware (simpleStdoutLogType, withDetailedLoggerMiddleware)
import           Web.Logging.Logger
import           Web.Logging.MonadLogger
import           Web.Scotty.Trans
import           Web.Types.WebApp

data AppState = AppState {
    appVaultLoggerKey :: VaultLoggerKey
  , appTolaApiConfig  :: TolaApiConfig
  , appDbPool         :: TolaPool
  }
instance HasTolaApiConfig AppState where
  tolaApiConfig = appTolaApiConfig
instance HasVaultLoggerKey AppState where
  vaultLoggerKey = appVaultLoggerKey
instance HasDbPool AppState where
  dbPool = appDbPool




newtype RealWebAppT m a = RealWebAppT { unRealWebAppT ::  ReaderT AppState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState, MonadTrans)

type WebMAction m a = ActionT TL.Text (RealWebAppT m) a
type RealWebApp m a = ScottyT TL.Text (RealWebAppT m) ()

instance MonadLogger (ActionT TL.Text (RealWebAppT IO)) where
  writeLog = writeLog'

instance MonadTolaDatabase (ActionT TL.Text (RealWebAppT IO)) where
  doMigrations = runDb doMigrations'
  insertChargeRequest = runDb . insertChargeRequest'
  updateChargeRequestWithResponse i = runDb . updateChargeRequestWithResponse' i
  insertLodgementNotificationAndupdateChargeRequest = runDb . insertLodgementNotificationAndupdateChargeRequest'
  insertDisbursementNotificationAndupdateChargeRequest = runDb . insertDisbursementNotificationAndupdateChargeRequest'
  getChargeRequestStatus = runDb . getChargeRequestStatus'

instance MonadTolaApi (ActionT TL.Text (RealWebAppT IO)) where
  makeChargeRequest req = do
    config <- lift $ asks appTolaApiConfig
    makeChargeRequest'' config =<< toMACed req

instance ToMACed (ActionT TL.Text (RealWebAppT IO)) where
  toMACed r = (`MACed` r) . _tolaSecret <$> lift (asks appTolaApiConfig)


runWeb ::
     AppState
  -> (RealWebAppT IO) a -> IO a
runWeb appState app =
  runReaderT (unRealWebAppT app) appState

-- runWebServer :: Int -> RealWebApp IO b -> IO () -- RealWebApp IO b = ScottyT TL.Text (RealWebAppT IO) ()
runWebServer db secret url (authUsername, authPassword) port app = do
  loggerVaultKey <- V.newKey
  withDetailedLoggerMiddleware
    loggerVaultKey
    ( \logger -> withDbPool
      db
      ( \pool -> scottyT
        port
        ( runWeb
            (appState loggerVaultKey pool)
        )
        (middleware logger >> app)
      )
    )
    simpleStdoutLogType
    where
      appState loggerVaultKey pool = AppState {
          appVaultLoggerKey = loggerVaultKey
        , appTolaApiConfig = tolaApiConfig'
        , appDbPool = pool
      }
      tolaApiConfig' = TolaApiConfig {
          tolaApiBasicAuth = (authUsername, authPassword)
        , tolaApiUrl       = url -- "https://httpbin.org/post" --"https://requestb.in/13gb79v1"
        , _tolaSecret = secret
      }
