{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web (main)
where

import           Control.Concurrent.MVar
import           Control.Monad           (void)
import qualified Data.Aeson              as A
import qualified Data.ByteString.Char8   as Char8
import qualified Data.ByteString.Lazy    as BL
import qualified Data.List               as List
import           Data.Maybe              (maybe)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, pack, unpack)
import qualified Data.Text.Encoding      as E
import qualified Database.Redis          as R
import           Network.Wai             (Application)
import qualified Network.Wai.Test        as WT
import qualified System.Environment      as Env
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.Hspec.Wai
import qualified Tola.Common             as Tola
import qualified Web.JewlModel           as JM
import           Web.Localization        (decrypt')
import qualified Web.Scotty.Trans        as Trans
import           Web.Visit
import qualified Web.WebM                as W
--
import           Control.Concurrent
import qualified Tola.ChargeRequest      as TChargeRequest
import qualified Tola.ChargeResponse     as TChargeResponse
import qualified Tola.TolaInterface      as TolaInterface

-- test utilities
printSResponseHeaders (WT.SResponse s h b) = print h

printSResponseBody (WT.SResponse s h b) = print b

getHeader name (WT.SResponse _ h _) = snd <$> List.find ((== name) . fst) h

getHeaderM name r = case getHeader name r of
  Just v  -> return v
  Nothing -> error "Header not found"

getResponseBody (WT.SResponse s h b) = b



-- WebMApp

myApp :: W.WebMApp ()
myApp =
  doMigrationsWeb >> lodgementRequestWeb >> chargeRequestWeb >> chargeNotificationWeb




withAppT :: W.ScottyT e W.WebM () -> SpecWith Application -> Spec
withAppT = with . Trans.scottyAppT (\a -> do
  db <- liftIO $ Env.getEnv "db"
  jewlDb <- liftIO $ Env.getEnv "jewel_connection_string"
  W.runWebM R.defaultConnectInfo (Char8.pack jewlDb) (Char8.pack db) mockedTolaInterface a)

test200
  :: Text -> (Char8.ByteString -> WaiSession WT.SResponse) -> WaiSession WT.SResponse
test200 url f = do
  r <- f (E.encodeUtf8 url)
  shouldRespondWith (return r) 200
  liftIO $ printSResponseBody r
  return r

testPost200 :: Text -> BL.ByteString -> WaiSession WT.SResponse
testPost200 url body = test200 url (`post` body)

testRequest200 :: Text -> WaiSession WT.SResponse
testRequest200 url = test200 url get

addSubmissionTest :: Text -> WaiSession Text
addSubmissionTest url = do
  r <- get $ E.encodeUtf8 url
  shouldRespondWith (return r) 200
  liftIO $ printSResponseBody r

  let r' = getResponseBody r
  case (A.decode r' :: Maybe SubmissionResult) of
    Nothing -> do
        liftIO $ expectationFailure $ "Unable to parse the response from check_msisdn_active_subscription \n" <> show r
        return "ERROR"
    Just s -> do
      let hsid = submissionId s
      let msid = decrypt' $ Char8.pack $ unpack hsid
      liftIO $
        putStrLn $ "submissionId: " <> show msid <> ", " <> unpack hsid
      return hsid


addMSISDNSubmissionTest :: Text -> Text -> Text -> Int -> Text -> WaiSession Text
addMSISDNSubmissionTest domain country handle offer msisdn =
  addSubmissionTest ("/submit_msisdn/" <> domain <> "/" <> country <> "/" <> handle <> "/" <> pack (show offer) <> "/?msisdn=" <> msisdn)

addPINSubmissionTest :: Text -> Text -> WaiSession Text
addPINSubmissionTest sid pin =
  addSubmissionTest ("/submit_pin/?sid=" <> sid <> "&pin=" <> pin)

checkMSISDNTest :: Text -> Text -> WaiSession ()
checkMSISDNTest country msisdn = do
  r <- getResponseBody <$> testRequest200 ("/check_msisdn_active_subscription/" <> country <> "/?msisdn=" <> msisdn)
  maybe
    (liftIO $ expectationFailure $ "Unable to parse the response from check_msisdn_active_subscription \n" <> show r)
    (const $ return ())
    (A.decode r :: Maybe JM.FinalResult)

addLodgementRequestTest :: WaiSession ()
addLodgementRequestTest = do
  r <- getResponseBody <$> testPost200 "/tola/lodgement_request/" "{\"accountname\":\"Joe Blogs\",\"amount\":1000,\"amounttype\":\"unit\",\"channel\":\"KENYA.SAFARICOM\",\"currency\":\"KES\",\"customerreference\":\"123456789\",\"date\":\"2015-06-23T09:45:31Z\",\"mac\":\"a9993e364706816aba3e25717850c26c9cd0d89d\",\"msisdn\":\"25412345678\",\"operatorreference\":\"ABCDEF123456\",\"reference\":\"1.123.1435455096.1\",\"sourcereference\":\"113121316\",\"target\":\"800123\",\"type\":\"lodgement\"}"
  maybe
    (liftIO $ expectationFailure $ "Unable to parse the response from check_msisdn_active_subscription \n" <> show r)
    (const $ return ())
    (A.decode r :: Maybe Tola.SuccessResponse)


addChargeRequestTest :: WaiSession ()
addChargeRequestTest = do
  r <- getResponseBody <$> testRequest200
    "/api/charge/300000001/25.6"
  return ()

--

testMigrations =
  describe "Testing Migrations" $
    withAppT myApp $
      it "must Migrate Database Schema" $
        get "/do_migrations" `shouldRespondWith` 200

testAddMSISDNSubmission sync msisdn =
  describe "Testing Adding A MSISDN Submission" $
    withAppT myApp $
      it "must add a new MSISDN Submission" $ do
        sid <- addMSISDNSubmissionTest "m.mobiworld.biz" "gr" "antivirus-kspr" 1 msisdn
        liftIO $ print sid
        liftIO $ putMVar sync sid

testAddPINSubmission sync pin =
  describe "Testing Adding A PIN Submission" $
    withAppT myApp $
      it "must add a new PIN Submission" $ do
        sid <- liftIO $ readMVar sync
        sid' <- addPINSubmissionTest sid pin
        liftIO $ print sid'

testCheckMSISDN country msisdn =
  describe "Testing Check MSISDN"
    $ withAppT myApp
    $ it "must return a valid FinalResult JSON object"
    $ checkMSISDNTest country msisdn

testAddLodgementRequest =
  describe "Testing Add Lodgement Request"
    $ withAppT myApp
    $ it "must return '{ success: true }' JSON"
    $ addLodgementRequestTest

testAddChargeRequest =
  describe "Testing Add Charge Request"
    $ withAppT myApp
    $ it "must return '{ success: true }' JSON"
    $ addChargeRequestTest

testAddChargeNotification sourceref =
  describe "Testing Add Charge Notification"
    $ withAppT myApp
    $ it "must return hello_world"
    $ do
      r <- getResponseBody <$> testRequest200
        ("/tola/charge_notification/" <> Tola.unSourceReference sourceref)
      return ()



--
mockedTolaInterface :: TolaInterface.TolaInterface
mockedTolaInterface = TolaInterface.TolaInterface
  { TolaInterface.makeChargeRequest = \req -> do
    forkIO $ do
      threadDelay 100
      putStrLn "forkIO testChargeNotification"
      hspec $ testAddChargeNotification (TChargeRequest.sourcereference req)
    return $ TChargeResponse.mkSuccessChargeResponse "some.reference"
  }


--

main :: IO ()
main = do
  sync <- newEmptyMVar
  hspec $ do
    testMigrations
    testAddChargeRequest
  threadDelay 1000
  -- forkIO $ (hspec testAddLodgementRequest) >> putMVar sync ()
  -- takeMVar sync

    -- testCheckMSISDN "GR" "6972865341"
    -- testAddMSISDNSubmission sync "306972865341"
    -- testAddPINSubmission sync "1234"
