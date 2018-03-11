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
import           Tola.MonadTolaApi
import           Tola.RealTolaApi
import           Web.Logging.DetailedLoggerMiddleware (simpleStdoutLogType, withDetailedLoggerMiddleware)
import           Web.Logging.Logger
import           Web.Logging.MonadLogger
import           Web.Scotty.Trans
import           Web.Types.State
import           Web.Types.WebApp


newtype RealWebAppT m a = RealWebAppT { unRealWebAppT ::  ReaderT AppState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState, MonadTrans)

type WebMAction m a = ActionT TL.Text (RealWebAppT m) a
type RealWebApp m a = ScottyT TL.Text (RealWebAppT m) ()

instance MonadLogger (ActionT TL.Text (RealWebAppT IO)) where
  writeLog = writeLog'

instance MonadTolaApi (ActionT TL.Text (RealWebAppT IO)) where
  makeChargeRequest req = do
    config <- lift $ asks appTolaApiConfig
    makeChargeRequest'' config req


runWeb ::
     VaultLoggerKey
  -> TolaApiConfig
  -> (RealWebAppT IO) a -> IO a
runWeb loggerVaultKey tolaApiConfig' app = do
  let appState = AppState {
      appVaultLoggerKey = loggerVaultKey
    , appTolaApiConfig = tolaApiConfig'
    }
  runReaderT (unRealWebAppT app) appState

runWebServer :: Int -> RealWebApp IO b -> IO () -- RealWebApp IO b = ScottyT TL.Text (RealWebAppT IO) ()
runWebServer port app = do
  loggerVaultKey <- V.newKey
  withDetailedLoggerMiddleware
    loggerVaultKey
    ( \ logger ->
      scottyT
        port
        (runWeb loggerVaultKey
                TolaApiConfig {
                  tolaApiBasicAuth = ("", "")
                , tolaApiUrl =  "https://httpbin.org/post" --"https://requestb.in/13gb79v1"
                }
        )
        (middleware logger >> app)
    )
    simpleStdoutLogType
