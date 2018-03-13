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
import           Web.Scotty.Trans                     (ActionT, ScottyT,
                                                       middleware, scottyAppT)
--
import           Control.Concurrent
import           Data.Time.Clock                      (getCurrentTime)
import           Tola.Imports
import           Tola.Types.ChargeResponse
import           Tola.Types.Common
import qualified Tola.Types.DisbursementNotification  as DisbursementNotification
import qualified Tola.Types.LodgementNotification     as LodgementNotification
import           Web.Crypto
--
import           Network.Wai
import           Test.Hspec
import           Test.Hspec.Wai
--
import qualified Data.ByteString.Char8                as Char8
--
import           Web.Testing.Helpers
import           Web.Visit
--
import qualified Data.Aeson                           as A
import           Data.Monoid                          ((<>))
import           Tola.Database.MonadTolaDatabase


data AppState s = AppState {
    appStateVaultKeyLogger :: VaultLoggerKey
  , appStateTolaSecret     :: Secret
  , appStateSync           :: MVar s
  , appStateDbPool         :: TolaPool
  }
instance HasVaultLoggerKey (AppState s) where
  vaultLoggerKey = appStateVaultKeyLogger
instance HasTolaSecret (AppState s) where
  tolaSecret = appStateTolaSecret
instance HasDbPool (AppState s) where
  dbPool = appStateDbPool


newtype MockWebAppT r m a = MockWebAppT { unMockWebAppT ::  ReaderT r m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader r, MonadTrans)

type WebMAction r m a = ActionT TL.Text (MockWebAppT r m) a
type MockWebApp r m a = ScottyT TL.Text (MockWebAppT r m) ()

instance MonadLogger (ActionT TL.Text (MockWebAppT (AppState ()) IO)) where
  writeLog = liftIO . Char8.putStrLn . (">>> " <>) -- writeLog'

instance MonadTolaDatabase (ActionT TL.Text (MockWebAppT (AppState ()) IO)) where
  doMigrations = runDb doMigrations'
  insertChargeRequest = runDb . insertChargeRequest'
  updateChargeRequestWithResponse i = runDb . updateChargeRequestWithResponse' i
  insertLodgementNotificationAndupdateChargeRequest = runDb . insertLodgementNotificationAndupdateChargeRequest'
  insertDisbursementNotificationAndupdateChargeRequest = runDb . insertDisbursementNotificationAndupdateChargeRequest'
  getChargeRequestStatus = runDb . getChargeRequestStatus'


instance MonadTolaApi (ActionT TL.Text (MockWebAppT (AppState ()) IO)) where
  makeChargeRequest req = do
    sync <- lift $ asks appStateSync
    secret <- lift $ asks appStateTolaSecret
    sourceRef <- liftIO $ mkSourceReference . pack . (toHex :: Integer -> String) <$> getTime (1000000 :: Double)
    liftIO $ forkIO $ do
          threadDelay 10000 -- artificial delay to simulate async callback
          nowl <- getCurrentTime
          ref <- mkSourceReference . pack . (toHex :: Integer -> String) <$> getTime (1000 :: Double)

          let lnotification = LodgementNotification.fromChargeRequest
                secret
                sourceRef
                (mkOperatorReference "operator.ref")
                (mkCustomerReference "custoemr.ref")
                Nothing
                nowl
                req

          -- Send lnotification callback back to our server
          hspec $ testAddLodgementNotificationForCharge (withAppT sync myApp) lnotification

          nowd <- getCurrentTime

          let dnotification = DisbursementNotification.fromChargeRequest
                secret
                (mkOperatorReference "operator.ref")
                sourceRef
                nowd
                req

          -- Send dnotification callback back to our server
          hspec $ testAddDisbursementNotificationForCharge (withAppT sync myApp) dnotification
          putMVar sync ()
    return $ mkSuccessChargeResponse sourceRef


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
    ( \logger -> withDbPool
      "host=localhost dbname=tola" --TODO: get connection string from Env
      ( \pool -> scottyAppT
        (runWeb $ AppState loggerVaultKey (mkSecret "tola_secret") sync pool) --TODO: get secret from Env
        (middleware logger >> app)
      )
    )
    simpleStdoutLogType


myApp :: MockWebApp (AppState ()) IO ()
myApp =  homeWeb
      >> lodgementNotificationWeb
      >> disbursementNotificationWeb
      >> chargeRequestWeb
      >> checkChargeRequestWeb
      >> doMigrationsWeb
--

withAppT :: MVar () ->  MockWebApp (AppState ()) IO () -> SpecWith Application -> Spec
withAppT sync a = with run
 where
  run =
    -- db             <- liftIO $ Env.getEnv "db"
    -- jewlDb         <- liftIO $ Env.getEnv "jewel_connection_string"
    -- secret         <- liftIO $ fmap mkSecret' (Env.getEnv "tola_secret")
    runWebServer
              sync
              a

testAddChargeRequest :: forall a
  . (SpecWith Application -> SpecWith a) -> SpecWith a
testAddChargeRequest appSpec =
  describe "Testing Add Charge Request"
    $ appSpec
    $ it "must return '{ success: true }' JSON" addChargeRequestTest
  where
    addChargeRequestTest :: WaiSession ()
    addChargeRequestTest = do
      _ <- getResponseBody <$> testGet200
        "/api/charge/300000001/25.6/50% OFF ENDS NOW"
      return ()

testChargeRequestAndNotification :: IO ()
testChargeRequestAndNotification = do
  sync <- newEmptyMVar
  let
    appSpec :: SpecWith Application -> Spec
    appSpec = withAppT sync myApp
  hspec $ testAddChargeRequest appSpec
  takeMVar sync -- wait for charge notification callback to complete

testAddLodgementNotificationForCharge appSpec notification =
  describe "Testing Add Lodgement Notification"
    $ appSpec
    $ it "must return '{ success: true }' JSON"
    $ do
      let body = A.encode notification
      void $ getResponseBody <$> testPost200 "/tola/lodgement_notification/" body
      return ()

testAddDisbursementNotificationForCharge appSpec notification =
  describe "Testing Add Disbursement Notification"
    $ appSpec
    $ it "must return '{ success: true }' JSON"
    $ do
      let body = A.encode notification
      void $ getResponseBody <$> testPost200 "/tola/disbursement_notification/" body
      return ()

testAddLodgementNotification appSpec =
  describe "Testing Add Lodgement Request"
    $ appSpec
    $ it "must return '{ success: true }' JSON"
    $ addLodgementNotificationTest
    where
      addLodgementNotificationTest :: WaiSession ()
      addLodgementNotificationTest = do
        r <- getResponseBody <$> testPost200 "/tola/lodgement_notification/" "{\"accountname\":\"Joe Blogs\",\"amount\":1000,\"amounttype\":\"unit\",\"channel\":\"KENYA.SAFARICOM\",\"currency\":\"KES\",\"customerreference\":\"123456789\",\"date\":\"2015-06-23T09:45:31Z\",\"mac\":\"a9993e364706816aba3e25717850c26c9cd0d89d\",\"msisdn\":\"25412345678\",\"operatorreference\":\"ABCDEF123456\",\"reference\":\"1.123.1435455096.1\",\"sourcereference\":\"113121316\",\"target\":\"800123\",\"type\":\"lodgement\"}"
        maybe
          (liftIO $ expectationFailure $ "Unable to parse the response from check_msisdn_active_subscription \n" <> show r)
          (const $ return ())
          (A.decode r :: Maybe SuccessResponse)


testMigrations appSpec =
  describe "Testing Migrations"
    $ appSpec
    $ it "must Migrate Database Schema"
    $ testGet200 "/do_migrations" `shouldRespondWith` 200

main :: IO ()
main = do
  sync <- newEmptyMVar
  let
    appSpec :: SpecWith Application -> Spec
    appSpec = withAppT sync myApp

  hspec $ do
    testMigrations appSpec
    testAddLodgementNotification appSpec

  testChargeRequestAndNotification
