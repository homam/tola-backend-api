{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web (main)
where

import           Control.Concurrent.MVar
import qualified Data.ByteString.Char8   as Char8
import qualified Data.List               as List
import           Data.Monoid             ((<>))
import           Data.Text               (Text, pack)
import qualified Data.Text.Encoding      as E
import qualified Database.Redis          as R
import qualified Network.Wai.Test        as WT
import           Test.Hspec
import           Test.Hspec.Wai
import qualified Web.Scotty.Trans        as Trans
import           Web.Visit
import qualified Web.WebM                as W

-- test utilities
printSResponseHeaders (WT.SResponse s h b) = print h

printSResponseBody (WT.SResponse s h b) = print b

getHeader name (WT.SResponse _ h _) = snd <$> List.find ((== name) . fst) h

getHeaderM name r = case getHeader name r of
  Just v  -> return v
  Nothing -> error "Header not found"


-- WebMApp

myApp :: W.WebMApp ()
myApp = doMigrationsWeb >> msisdnSubmissionWeb >> pinSubmissionWeb

withAppT app = with $ Trans.scottyAppT (W.runWebM R.defaultConnectInfo "host=localhost dbname=test") app

addSubmissionTest :: Text -> WaiSession Int
addSubmissionTest url = do
  r <- get $ E.encodeUtf8 url
  shouldRespondWith (return r) 200
  liftIO $ printSResponseBody r
  read . Char8.unpack <$> getHeaderM "X-Scotch-SubmissionId" r

addMSISDNSubmissionTest :: Text -> Text -> Text -> Int -> Text -> WaiSession Int
addMSISDNSubmissionTest domain country handle offer msisdn =
  addSubmissionTest ("/submit_msisdn/" <> domain <> "/" <> country <> "/" <> handle <> "/" <> pack (show offer) <> "/?msisdn=" <> msisdn)

addPINSubmissionTest :: Int -> Text -> WaiSession Int
addPINSubmissionTest sid pin =
  addSubmissionTest ("/submit_pin/?sid=" <> pack (show sid) <> "&pin=" <> pin)

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

main :: IO ()
main = do
  sync <- newEmptyMVar
  hspec $ do
    testMigrations
    testAddMSISDNSubmission sync "6972865344"
    testAddPINSubmission sync "1234"
