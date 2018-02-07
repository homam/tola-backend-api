{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web (main)
where

import           Control.Concurrent.MVar
import           Control.Monad                      (void)
import qualified Data.Aeson                         as A
import qualified Data.ByteString.Char8              as Char8
import qualified Data.ByteString.Lazy               as BL
import qualified Data.List                          as List
import           Data.Maybe                         (maybe)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text, pack, unpack)
import qualified Data.Text.Encoding                 as E
import qualified Database.Redis                     as R
import           Network.Wai                        (Application, Middleware,
                                                     Request, requestHeaders)
import qualified Network.Wai.Test                   as WT
import qualified System.Environment                 as Env
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.Hspec.Wai
import           Tola.Common
import qualified Web.JewlModel                      as JM
import qualified Web.Scotty.Trans                   as Trans
import           Web.Visit
import qualified Web.WebM                           as W
--
import           Control.Concurrent
import qualified Data.CaseInsensitive               as CI
import           Data.Time.Clock                    (getCurrentTime)
import qualified Tola.ChargeRequest                 as TChargeRequest
import qualified Tola.ChargeResponse                as TChargeResponse
import qualified Tola.DisbursementNotification      as TDisbursementNotification
import qualified Tola.LodgementNotification         as TLodgementNotification
import qualified Tola.TolaInterface                 as TolaInterface
import           Web.Localization                   (getTime, toHex)
import qualified Web.Utils.DetailedLoggerMiddleware as DLogger

-- test utilities
printSResponseHeaders (WT.SResponse s h b) = print h

printSResponseBody (WT.SResponse s h b) = print b

getHeader name (WT.SResponse _ h _) = snd <$> List.find ((== name) . fst) h

getHeaderM name r = case getHeader name r of
  Just v  -> return v
  Nothing -> error "Header not found"

getResponseBody (WT.SResponse s h b) = b


-- Wai Utility

myMiddleware :: Middleware
myMiddleware app req = app (addHeader ("RequestId", "1") req)

addHeader :: (Char8.ByteString, Char8.ByteString) -> Request -> Request
addHeader (k, v) req =
  req { requestHeaders = (CI.mk k, v) : requestHeaders req }


-- WebMApp

myApp :: W.WebMApp ()
myApp =
  doMigrationsWeb >> lodgementNotificationWeb >> disbursementNotificationWeb >> chargeRequestWeb


withAppT :: TolaInterface.TolaApi -> W.WebMApp () -> SpecWith Application -> Spec
withAppT mockApi a =
  with (run W.simpleStdoutLogType)
  where
    run logType = do
      db     <- liftIO $ Env.getEnv "db"
      jewlDb <- liftIO $ Env.getEnv "jewel_connection_string"
      secret <- liftIO $ fmap mkSecret' (Env.getEnv "tola_secret")
      W.runWebM logType R.defaultConnectInfo (Char8.pack jewlDb) (Char8.pack db) secret mockApi a


test200 :: Text -> (Char8.ByteString -> WaiSession WT.SResponse) -> WaiSession WT.SResponse
test200 url f = do
  r <- f (E.encodeUtf8 url)
  liftIO $ printSResponseBody r
  shouldRespondWith (return r) 200
  return r

testPost200 :: Text -> BL.ByteString -> WaiSession WT.SResponse
testPost200 url body = test200 url (`post` body)

testGet200 :: Text -> WaiSession WT.SResponse
testGet200 url = test200 url get

addLodgementNotificationTest :: WaiSession ()
addLodgementNotificationTest = do
  r <- getResponseBody <$> testPost200 "/tola/lodgement_notification/" "{\"accountname\":\"Joe Blogs\",\"amount\":1000,\"amounttype\":\"unit\",\"channel\":\"KENYA.SAFARICOM\",\"currency\":\"KES\",\"customerreference\":\"123456789\",\"date\":\"2015-06-23T09:45:31Z\",\"mac\":\"a9993e364706816aba3e25717850c26c9cd0d89d\",\"msisdn\":\"25412345678\",\"operatorreference\":\"ABCDEF123456\",\"reference\":\"1.123.1435455096.1\",\"sourcereference\":\"113121316\",\"target\":\"800123\",\"type\":\"lodgement\"}"
  maybe
    (liftIO $ expectationFailure $ "Unable to parse the response from check_msisdn_active_subscription \n" <> show r)
    (const $ return ())
    (A.decode r :: Maybe SuccessResponse)


addChargeRequestTest :: WaiSession ()
addChargeRequestTest = do
  r <- getResponseBody <$> testGet200
    "/api/charge/300000001/25.6/50% OFF ENDS NOW"
  return ()

--

testMigrations appSpec =
  describe "Testing Migrations"
    $ appSpec
    $ it "must Migrate Database Schema"
    $ testGet200 "/do_migrations" `shouldRespondWith` 200

testAddLodgementNotification appSpec =
  describe "Testing Add Lodgement Request"
    $ appSpec
    $ it "must return '{ success: true }' JSON"
    $ addLodgementNotificationTest

testAddChargeRequest appSpec =
  describe "Testing Add Charge Request"
    $ appSpec
    $ it "must return '{ success: true }' JSON"
    $ addChargeRequestTest

testAddLodgementNotificationForCharge appSpec notification =
  describe "Testing Add Lodgement Notification"
    $ appSpec
    $ it "must return '{ success: true }' JSON"
    $ do
      void $ getResponseBody <$> testPost200 "/tola/lodgement_notification/" (A.encode notification)
      return ()

testAddDisbursementNotificationForCharge appSpec notification =
  describe "Testing Add Disbursement Notification"
    $ appSpec
    $ it "must return '{ success: true }' JSON"
    $ do
      void $ getResponseBody <$> testPost200 "/tola/disbursement_notification/" (A.encode notification)
      return ()


testChargeRequestAndNotification :: TolaInterface.TolaApi -> IO ()
testChargeRequestAndNotification mockApi = do
  sync <- newEmptyMVar
  sourceRef <- mkSourceReference . pack . toHex <$> getTime 1000000
  secret <- liftIO $ fmap mkSecret' (Env.getEnv "tola_secret")
  let
    -- | Mocked Tola API
    mockApi' :: TolaInterface.TolaApi
    mockApi' = mockApi {
      TolaInterface.makeChargeRequest = \req -> do
        _ <- forkIO $ do
          threadDelay 10000 -- artificial delay to simulate async callback
          nowl <- getCurrentTime
          ref <- mkSourceReference . pack . toHex <$> getTime 1000
          let lnotification = TLodgementNotification.fromChargeRequest
                secret
                sourceRef
                (mkOperatorReference "operator.ref")
                (mkCustomerReference "custoemr.ref")
                Nothing
                nowl
                req
          -- Send lnotification callback back to our server
          hspec $ testAddLodgementNotificationForCharge appSpec lnotification

          threadDelay 10000 -- artificial delay to simulate async callback
          nowd <- getCurrentTime
          let dnotification = TDisbursementNotification.fromChargeRequest
                secret
                (mkOperatorReference "operator.ref")
                sourceRef
                nowd
                req
          -- Send dnotification callback back to our server
          hspec $ testAddDisbursementNotificationForCharge appSpec dnotification

          putMVar sync ()

        return $ TChargeResponse.mkSuccessChargeResponse sourceRef
      }

    appSpec :: SpecWith Application -> Spec
    appSpec = withAppT mockApi' myApp

  hspec $ testAddChargeRequest appSpec -- charge request by client
  takeMVar sync -- wait for charge notification callback to complete

main :: IO ()
main = do
  let
    -- | Mocked Tola API
    mockApi :: TolaInterface.TolaApi
    mockApi = TolaInterface.TolaApi {
      TolaInterface.makeChargeRequest = error "Not implemented"
    }

    appSpec :: SpecWith Application -> Spec
    appSpec = withAppT mockApi myApp

  hspec $ do
    testMigrations appSpec
    testAddLodgementNotification appSpec

  testChargeRequestAndNotification mockApi


  -- forkIO $ (hspec testAddLodgementNotification) >> putMVar sync ()

    -- testCheckMSISDN "GR" "6972865341"
    -- testAddMSISDNSubmission sync "306972865341"
    -- testAddPINSubmission sync "1234"
