{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tola.RealTolaApi where

import           Control.Monad.Trans.Class
import           Tola.MonadTolaApi
--
import qualified Data.Aeson                as A
import qualified Data.ByteString.Char8     as Char8
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import qualified Data.Maybe                as Y
import qualified Network.HTTP.Conduit      as C
--
import           Control.Monad.Reader
import           Web.Logging.MonadLogger
--
import           Tola.Types.ChargeRequest
import           Tola.Types.ChargeResponse


data TolaApiConfig = TolaApiConfig {
  tolaApiBasicAuth :: (Char8.ByteString, Char8.ByteString)
, tolaApiUrl       :: String
} deriving Show

class HasTolaApiConfig t where
  tolaApiConfig :: t -> TolaApiConfig

newtype RealTolaApiT r m a = RealTolaApiT {
  unRealTolaApiT :: ReaderT r m a
} deriving (Functor, Applicative, Monad, MonadTrans, MonadReader r, MonadIO, MonadLogger)

runRealTolaApiT :: forall r (m :: * -> *) a . RealTolaApiT r m a -> r -> m a
runRealTolaApiT = runReaderT . unRealTolaApiT

instance (Monad m, MonadLogger m, HasTolaApiConfig r, MonadReader r m, MonadIO m) => MonadTolaApi (RealTolaApiT r m) where
  makeChargeRequest = makeChargeRequest'

makeChargeRequest' :: forall (m :: * -> *) t.
  (MonadIO m,  HasTolaApiConfig t,
  MonadReader t m, MonadLogger m) =>
  ChargeRequest -> m ChargeResponse
makeChargeRequest' req = do
  config <- asks tolaApiConfig
  makeChargeRequest'' config req


makeChargeRequest'' :: (MonadIO m, MonadLogger m) => TolaApiConfig -> ChargeRequest -> m ChargeResponse
makeChargeRequest'' config req = do
  writeLog "makeChargeRequest"
  Y.fromJust
    .   A.decode
    .   fromStrict
    <$> post (tolaApiBasicAuth config) (tolaApiUrl config) req



---

post :: (MonadIO m, MonadLogger m, A.ToJSON t) => (Char8.ByteString, Char8.ByteString) -> String -> t -> m Char8.ByteString
post (user, pass) url obj = do
  manager <- liftIO $ C.newManager C.tlsManagerSettings
  r <- liftIO $ C.parseUrlThrow url
  let json = A.encode obj
  let request = C.applyBasicAuth user pass $ r {
      C.secure = True
    , C.method = "POST"
    , C.requestBody = C.RequestBodyBS (toStrict json)
    , C.requestHeaders = C.requestHeaders r ++ [("Content-Type",  "application/json")]
    }
  writeLog $ Char8.pack $ show request
  writeLog (toStrict json)
  response <- C.httpLbs request manager
  let strictBody = toStrict $ C.responseBody response
  writeLog strictBody
  return strictBody
