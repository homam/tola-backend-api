{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Test (
  module Test
) where

import           Control.Applicative     (Applicative)
import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.Reader    (MonadIO, MonadReader, ReaderT, asks,
                                          lift, liftIO, runReaderT)
import           Data.ByteString         (ByteString)
import qualified Data.CaseInsensitive    as CI
import           Data.Monoid             ((<>))
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Text.Lazy          (Text, toStrict, unpack)
import           Network.Wai             (Application, Middleware, Request,
                                          requestHeaders)
import           Network.Wai.Test        (SResponse (..))
import           Test.Hspec              (Spec, SpecWith, describe, hspec, it)
import           Test.Hspec.Wai          (WaiSession, shouldRespondWith, with)
import qualified Test.Hspec.Wai          as WaiTest
import           Test.QuickCheck
import           Web.Scotty.Trans        (ScottyT, get, middleware, param,
                                          scottyAppT, text)



data AppState = AppState {
  api :: Api
}
newtype WebM a = WebM { unWebM :: ReaderT AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

type WebMApp a = ScottyT Text WebM ()

runWeb :: Api -> WebM a -> IO a
runWeb a m = runReaderT (unWebM m) AppState {
    api = a
  }


-- Routes
clientRequestWeb :: WebMApp ()
clientRequestWeb = get "/client/:str" $ do
  i <- lift $ asks api
  str <- param "str"
  res <- liftIO $ someApiCall i str
  text res

apiCallbackWeb :: WebMApp ()
apiCallbackWeb = get "/callback/:notification" $
  text =<< ("Notified with " <>) <$> param "notification"

-- Wai Utility

myMiddleware :: Middleware
myMiddleware app req = app (addHeader ("RequestId", "1") req)

addHeader :: (ByteString, ByteString) -> Request -> Request
addHeader (k, v) req =
  req { requestHeaders = (CI.mk k, v) : requestHeaders req }


-- API

-- | Represents the external API
data Api = Api {
  someApiCall :: Text -> IO Text
}

-- | Real external API, a mocked instance is used in
realApi :: Api
realApi = Api {
  someApiCall = error "Not implemented"
}

-- Test

myApp :: WebMApp ()
myApp = clientRequestWeb >> apiCallbackWeb

withAppT :: Api -> ScottyT e WebM () -> SpecWith Application -> Spec
withAppT mockApi =
  with . (\s -> scottyAppT (runWeb mockApi) (middleware myMiddleware >> s))



testClientRequest :: (SpecWith Application -> SpecWith a) -> SpecWith a
testClientRequest appSpec =
  let str = "hello" in
  describe "Testing Client Request"
    $ appSpec
    $ it (unpack $ "must return: Received " <> str)
    $ testGet200 ("/client/" <> str) >> return ()

testCallback :: (SpecWith Application -> SpecWith a) -> Text -> SpecWith a
testCallback appSpec str =
  describe "Testing Callback"
    $ appSpec
    $ it (unpack $ "must return: Notified with " <> str)
    $ testGet200 ("/callback/" <> str) >> return ()

-- Test Helpers
testGet200 :: Text -> WaiSession SResponse
testGet200 url = test200 url WaiTest.get

test200 url f = do
  r <- f (encodeUtf8 $ toStrict url)
  shouldRespondWith (return r) 200
  liftIO $ printSResponseBody r
  return r

printSResponseBody (SResponse _ _ b) = print b

main = do
  sync <- newEmptyMVar
  let

    appSpec :: SpecWith Application -> Spec
    appSpec = withAppT mockApi myApp

    -- | Mocked external API
    mockApi :: Api
    mockApi = Api
      { someApiCall = \str -> do
        _ <- forkIO $ do
          threadDelay 300
          hspec $ testCallback appSpec str
          putMVar sync ()
        return $ "Received " <> str
      }
  hspec (testClientRequest appSpec)

  takeMVar sync

  main'

main' =
  hspec $ describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)

