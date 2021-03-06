{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Tola.RealTolaApi where

import qualified Control.Exception         as X
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.Aeson                as A
import qualified Data.ByteString.Char8     as Char8
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import qualified Network.HTTP.Conduit      as C
import           Tola.Types.ChargeRequest
import           Tola.Types.ChargeResponse
import           Tola.Types.Common
import           Web.Logging.MonadLogger




data TolaApiConfig = TolaApiConfig {
  tolaApiBasicAuth :: (Char8.ByteString, Char8.ByteString)
, tolaApiUrl       :: String
, _tolaSecret      :: Secret
} deriving Show

class HasTolaApiConfig t where
  tolaApiConfig :: t -> TolaApiConfig

instance HasTolaSecret TolaApiConfig where
  tolaSecret = _tolaSecret

mkTolaApiConfig :: (Char8.ByteString, Char8.ByteString) -> String -> Secret -> TolaApiConfig
mkTolaApiConfig = TolaApiConfig

{-
newtype RealTolaApiT r m a = RealTolaApiT {
  unRealTolaApiT :: ReaderT r m a
} deriving (Functor, Applicative, Monad, MonadTrans, MonadReader r, MonadIO, MonadLogger)

runRealTolaApiT :: forall r (m :: * -> *) a . RealTolaApiT r m a -> r -> m a
runRealTolaApiT = runReaderT . unRealTolaApiT

instance (Monad m, MonadLogger m, HasTolaApiConfig r, MonadReader r m, MonadIO m) => MonadTolaApi (RealTolaApiT r m) where
  makeChargeRequest = makeChargeRequest'
-}

makeChargeRequest' :: forall (m :: * -> *) t.
  (MonadIO m,  HasTolaApiConfig t, HasTolaSecret t,
  MonadReader t m, MonadLogger m, MonadCatch m) =>
  ChargeRequest -> m (Either String ChargeResponse)
makeChargeRequest' req = do
  config <- asks tolaApiConfig
  req' <- (`MACed` req) <$> asks tolaSecret
  makeChargeRequest'' config req'


makeChargeRequest'' :: (MonadIO m, MonadLogger m, MonadCatch m) => TolaApiConfig -> (MACed ChargeRequest) -> m (Either String ChargeResponse)
makeChargeRequest'' config req = do
  writeLog "makeChargeRequest"
  A.eitherDecode
     .   fromStrict
    <$> post (tolaApiBasicAuth config) (tolaApiUrl config) req



---

data MyException = MyException String deriving Show
instance X.Exception MyException

post :: (MonadIO m, MonadLogger m, A.ToJSON t, MonadCatch m) => (Char8.ByteString, Char8.ByteString) -> String -> t -> m Char8.ByteString
-- post (user, pass) url obj = throwM $ ( MyException "MyException was thrown")
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
  response <- liftIO $ C.httpLbs request manager
  let strictBody = toStrict $ C.responseBody response
  writeLog strictBody
  return strictBody
