{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Web.Apps.MockWebApp where

import           Control.Concurrent.Lifted
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.ByteString.Char8                (ByteString)
import qualified Data.Text.Lazy                       as TL
import           Data.Time.Clock                      (getCurrentTime)
import qualified Data.Vault.Lazy                      as V
import           Database.Persist.Postgresql          (ConnectionString)
import           Network.Wai.Handler.Warp             (Port)
import           Tola.Database.MonadTolaDatabase
import           Tola.Imports
import           Tola.MonadTolaApi
import           Tola.RealTolaApi
import qualified Tola.Types.ChargeRequest             as ChargeRequest
import           Tola.Types.ChargeResponse
import           Tola.Types.Common
import qualified Tola.Types.DisbursementNotification  as DisbursementNotification
import qualified Tola.Types.LodgementNotification     as LodgementNotification
import           Web.Crypto                           (getTime, toHex)
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


newtype MockWebAppT m a = MockWebAppT { unMockWebAppT ::  ReaderT AppState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState, MonadTrans, MonadThrow, MonadCatch)


instance MonadBase IO (MockWebAppT IO) where
  liftBase = liftBaseDefault

instance MonadBaseControl IO (MockWebAppT IO) where
  type StM (MockWebAppT IO) a = a
  liftBaseWith f = MockWebAppT $ liftBaseWith $ \q -> f (q . unMockWebAppT)
  restoreM =  MockWebAppT . restoreM


type WebMAction m a = ActionT TL.Text (MockWebAppT m) a
type MockWebApp m a = ScottyT TL.Text (MockWebAppT m) ()

instance MonadLogger (ActionT TL.Text (MockWebAppT IO)) where
  writeLog = writeLog'

instance MonadTolaDatabase (ActionT TL.Text (MockWebAppT IO)) where
  doMigrations = runDb doMigrations'
  insertChargeRequest = runDb . insertChargeRequest'
  updateChargeRequestWithResponse i = runDb . updateChargeRequestWithResponse' i
  insertLodgementNotificationAndupdateChargeRequest = runDb . insertLodgementNotificationAndupdateChargeRequest'
  insertDisbursementNotificationAndupdateChargeRequest = runDb . insertDisbursementNotificationAndupdateChargeRequest'
  getChargeRequestStatus = runDb . getChargeRequestStatus'
  getAllCampaigns = runDb getAllCampaigns'

instance MonadTolaApi (ActionT TL.Text (MockWebAppT IO)) where
  makeChargeRequest (ChargeRequest.MockableChargeRequest mock req) = do
    config <- lift $ asks appTolaApiConfig
    let secret = _tolaSecret config
    sourceRef <- liftIO $ mkSourceReference . pack . (toHex :: Integer -> String) <$> getTime (1000000 :: Double)

    case mock of
      ChargeRequest.MockChargeResponseError -> return $ Right $ mkFailureChargeResponse 666 "MockedFailure of ChargeResponse"
      _ -> do
        void $ fork $ do
              liftIO $ threadDelay 5000000 -- artificial delay to simulate async callback
              nowl <- liftIO getCurrentTime
              -- ref <- mkSourceReference . pack . (toHex :: Integer -> String) <$> getTime (1000 :: Double)

              let lnotification = LodgementNotification.fromChargeRequest
                    secret
                    sourceRef
                    (mkOperatorReference "operator.ref")
                    (mkCustomerReference "custoemr.ref")
                    Nothing
                    nowl
                    req

              -- Send lnotification callback back to our server
              -- hspec $ testAddLodgementNotificationForCharge (withAppT sync myApp) lnotification
              void $ insertLodgementNotificationAndupdateChargeRequest lnotification

              liftIO $ threadDelay 10000000

              nowd <- liftIO getCurrentTime

              let disbursementNotificationCtor = case mock of
                                          ChargeRequest.MockDisbursementNotificationError -> DisbursementNotification.fromChargeRequestAndError "Mocked Failure of DisbursementNotification"
                                          _ -> DisbursementNotification.fromChargeRequest

              let dnotification = disbursementNotificationCtor
                    secret
                    (mkOperatorReference "operator.ref")
                    sourceRef
                    nowd
                    req

              -- Send dnotification callback back to our server
              void $ insertDisbursementNotificationAndupdateChargeRequest dnotification

        return $ Right $ mkSuccessChargeResponse sourceRef

instance ToMACed (ActionT TL.Text (MockWebAppT IO)) where
  toMACed r = (`MACed` r) . _tolaSecret <$> lift (asks appTolaApiConfig)


runWeb ::
     AppState
  -> (MockWebAppT IO) a -> IO a
runWeb appState app =
  runReaderT (unMockWebAppT app) appState

runWebServer :: forall e
   . ConnectionString
  -> Secret
  -> String
  -> (ByteString, ByteString)
  -> Port
  -> ScottyT e (MockWebAppT IO) ()
  -> IO ()
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
        , appTolaApiConfig  = tolaApiConfig'
        , appDbPool         = pool
      }
      tolaApiConfig' = TolaApiConfig {
          tolaApiBasicAuth = (authUsername, authPassword)
        , tolaApiUrl       = url
        , _tolaSecret      = secret
      }
