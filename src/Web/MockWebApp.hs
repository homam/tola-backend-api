{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Web.MockWebApp where

import           Control.Monad.Reader
import qualified Data.Text.Lazy                       as TL
import qualified Data.Vault.Lazy                      as V
import           Tola.MonadTolaApi
import           Web.Logging.DetailedLoggerMiddleware (simpleStdoutLogType, withDetailedLoggerMiddleware)
import           Web.Logging.Logger
import           Web.Logging.MonadLogger
import           Web.Scotty.Trans
import           Web.Types.WebApp

newtype AppState a = AppState a

newtype MockWebAppT m a = MockWebAppT { unMockWebAppT ::  ReaderT (AppState ()) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (AppState ()), MonadTrans)

type WebMAction m a = ActionT TL.Text (MockWebAppT m) a
type MockWebApp m a = ScottyT TL.Text (MockWebAppT m) ()

instance MonadLogger (ActionT TL.Text (MockWebAppT IO)) where
  writeLog = writeLog'

-- instance MonadTolaApi (ActionT TL.Text (MockWebAppT IO)) where
--   makeChargeRequest req = do
--     config <- lift $ asks appTolaApiConfig
--     makeChargeRequest'' config req


runWeb ::
     VaultLoggerKey
  -> (MockWebAppT IO) a -> IO a
runWeb loggerVaultKey app = do
  let appState = AppState ()
  runReaderT (unMockWebAppT app) appState

runWebServer :: Int -> MockWebApp IO b -> IO () -- MockWebApp IO b = ScottyT TL.Text (MockWebAppT IO) ()
runWebServer port app = do
  loggerVaultKey <- V.newKey
  withDetailedLoggerMiddleware
    loggerVaultKey
    ( \ logger ->
      scottyT
        port
        (runWeb loggerVaultKey)
        (middleware logger >> app)
    )
    simpleStdoutLogType
