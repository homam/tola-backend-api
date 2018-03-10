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
import           Tola.Imports
import           Tola.Types.Common
--
import           Control.Monad.Reader
import           Web.Types.Logger


data TolaApiConfig = TolaApiConfig {
  tolaApiBasicAuth :: (Char8.ByteString, Char8.ByteString)
, tolaApiUrl       :: String
}

class HasTolaApiConfig t where
  tolaApiConfig :: t -> TolaApiConfig

newtype RealTolaApiT r m a = RealTolaApiT {
  unRealTolaApiT :: ReaderT r m a
} deriving (Functor, Applicative, Monad, MonadTrans, MonadReader r, MonadIO)

runRealTolaApiT :: forall r (m :: * -> *) a . RealTolaApiT r m a -> r -> m a
runRealTolaApiT = runReaderT . unRealTolaApiT

instance (Monad m, HasLogger r, HasTolaApiConfig r, MonadReader r m, MonadIO m) => MonadTolaApi (RealTolaApiT r m) where
  makeChargeRequest req = do
    writeLog'  <- asks writeLog
    config     <- asks tolaApiConfig
    liftIO $ writeLog' "makeChargeRequest"
    liftIO $ (Y.fromJust . A.decode . fromStrict) <$> post writeLog' (tolaApiBasicAuth config) (tolaApiUrl config) req


---

post :: A.ToJSON t => Logger -> (Char8.ByteString, Char8.ByteString) -> String -> t -> IO Char8.ByteString
post writeLog' (user, pass) url obj = do
  manager <- C.newManager C.tlsManagerSettings
  r <- C.parseUrlThrow url
  let json = A.encode obj
  let request = C.applyBasicAuth user pass $ r {
      C.secure = True
    , C.method = "POST"
    , C.requestBody = C.RequestBodyBS (toStrict json)
    , C.requestHeaders = (C.requestHeaders r) ++ [("Content-Type",  "application/json")]
    }
  writeLog' $ Char8.pack $ show request
  writeLog' (toStrict json)
  response <- C.httpLbs request manager
  let strictBody = toStrict $ C.responseBody response
  writeLog' strictBody
  return strictBody

