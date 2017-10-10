{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent.MVar
import qualified Data.ByteString.Char8   as Char8
import qualified Data.List               as List
import qualified Network.Wai.Test        as WT
import           Test.Hspec
import           Test.Hspec.Wai
import qualified Test.Hspec.Wai.Matcher  as Matcher
import           Web.Model               hiding (addVisit)
import qualified Web.Scotty.Trans        as Trans
import           Web.Visit
import qualified Web.WebM                as W

-- test utilities
printSResponse (WT.SResponse s h b) = print h

getHeader name (WT.SResponse _ h _) = snd <$> List.find ((== name) . fst) h

getHeaderM name r = case getHeader name r of
  Just v  -> return v
  Nothing -> error "Header not found"


-- WebMApp

myApp :: W.WebMApp ()
myApp = visitWeb

withAppT app = with $ Trans.scottyAppT W.runWebM app

addVisit :: WaiSession Int
addVisit = do
  r <-  get "/visit/1/PayguruTurkey/SomeLP/?click_id=128373"
  shouldRespondWith (return r) 200
  read . Char8.unpack <$> getHeaderM "X-Scotch-VisitId" r

--

testAddPerson sync =
  describe "Testing Adding Person" $
    withAppT myApp $
      it "must add Visit" $ do
        vid <- addVisit
        liftIO $ print vid
        liftIO $ putMVar sync vid

main :: IO ()
main = do
  sync <- newEmptyMVar
  hspec $ -- do
    testAddPerson sync
