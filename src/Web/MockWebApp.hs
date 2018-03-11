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
--
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Data.Time.Clock                      (getCurrentTime)
--
import           Network.Wai
import           Test.Hspec
import           Test.Hspec.Wai


data AppState s = AppState { appStateVaultKeyLogger :: VaultLoggerKey, appStateSync :: MVar s }
instance HasVaultLoggerKey (AppState s) where
  vaultLoggerKey = appStateVaultKeyLogger


newtype MockWebAppT r m a = MockWebAppT { unMockWebAppT ::  ReaderT r m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader r, MonadTrans)

type WebMAction r m a = ActionT TL.Text (MockWebAppT r m) a
type MockWebApp r m a = ScottyT TL.Text (MockWebAppT r m) ()

instance MonadLogger (ActionT TL.Text (MockWebAppT (AppState ()) IO)) where
  writeLog = writeLog'

instance MonadTolaApi (ActionT TL.Text (MockWebAppT (AppState ()) IO)) where
  makeChargeRequest req = do
    sync <- lift $ asks appStateSync
    liftIO $ forkIO $ do
          threadDelay 10000 -- artificial delay to simulate async callback
          nowl <- getCurrentTime
          {-
          ref <- mkSourceReference . pack . toHex <$> getTime 1000
          let lnotification = fromChargeRequest
                secret
                sourceRef
                (mkOperatorReference "operator.ref")
                (mkCustomerReference "custoemr.ref")
                Nothing
                nowl
                req
          -- Send lnotification callback back to our server
          hspec $ testAddLodgementNotificationForCharge appSpec lnotification
          -}
          error ""
    undefined
--     makeChargeRequest'' config req


runWeb ::
     r
  -> (MockWebAppT r IO) a -> IO a
runWeb appState app =
  runReaderT (unMockWebAppT app) appState

runWebServer :: MVar () -> MockWebApp (AppState ()) IO b -> IO Application
runWebServer sync app = do
  loggerVaultKey <- V.newKey
  withDetailedLoggerMiddleware
    loggerVaultKey
    ( \ logger ->
      scottyAppT
        (runWeb $ AppState loggerVaultKey sync)
        (middleware logger >> app)
    )
    simpleStdoutLogType

--

withAppT ::  MockWebApp (AppState ()) IO () -> SpecWith Application -> Spec
withAppT a = with run
 where
  run = do
    -- db             <- liftIO $ Env.getEnv "db"
    -- jewlDb         <- liftIO $ Env.getEnv "jewel_connection_string"
    -- secret         <- liftIO $ fmap mkSecret' (Env.getEnv "tola_secret")
    sync <- newEmptyMVar
    runWebServer
              sync
              a

