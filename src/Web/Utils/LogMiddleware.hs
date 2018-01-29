{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Utils.LogMiddleware (
  logAllMiddleware
  ) where

import           Control.Monad                (forM_)
import qualified Data.ByteString.Char8        as B8
import qualified Data.CaseInsensitive         as CI
import qualified Data.List                    as L
import           Data.Monoid                  (mconcat, (<>))
import qualified Data.Time                    as Time
import qualified Network.Wai                  as W
--
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (toLazyByteString)
import qualified Data.ByteString.Char8        as C8
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Builder as B
import           Data.IORef                   (modifyIORef', newIORef,
                                               readIORef)
-- import           Network.Wai.Header           (contentLength)
import           System.Log.FastLogger        (LogStr, ToLogStr, toLogStr)
--
import           Control.Exception            (SomeException, catch)
import qualified Network.HTTP.Types.Status    as Status



addHeader :: (BS.ByteString, BS.ByteString) -> W.Request -> W.Request
addHeader (k, v) req = req {
    W.requestHeaders = (CI.mk k, v) : W.requestHeaders req
  }

logAllMiddleware :: IO Integer -> (LogStr -> IO ()) -> W.Middleware
logAllMiddleware uniqueIdGenerator logger app req' sendResponse = do
  requestId <- uniqueIdGenerator
  let req = addHeader ("X-RequestId", C8.pack $ show requestId) req'
  let
      bs = B.byteString "\nrequestId<\n"
      append = (bs <>) . B.byteString
      headers' = foldl1 (<>) $ L.map ("\n  " <>) $ map (\(k, v) -> CI.foldedCase k <> ": " <> v) (W.requestHeaders req)
      path = W.rawPathInfo req
      queryString = W.rawQueryString req
  body <- W.requestBody req
  logger $ toLogStr $ BSL.toStrict $ B.toLazyByteString $ append $ mconcat
    $ L.intersperse "\n" [path <> queryString, headers', body]
  -- app req respond
  catch (app req $ \res -> do
    logger =<< ("requestId>\n" <>) . toLogStr . BSL.toStrict <$> responseBody res
    sendResponse res) (\ (ex :: SomeException) -> sendResponse (W.responseLBS Status.status500 [] "Some Error"))



logResponseMiddleware :: (BS.ByteString -> IO ()) -> W.Middleware
logResponseMiddleware logger app req sendResponse = app req $ \res -> do
  -- let msize = contentLength (W.responseHeaders res)
  -- print (W.responseStatus res)
  -- print msize
  logger =<< BSL.toStrict <$> responseBody res
  sendResponse res

responseBody :: W.Response -> IO BSL.ByteString
responseBody res =
  let (status, headers, body) = W.responseToStream res
  in  body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<>chunk)) (return ())
        toLazyByteString <$> readIORef content
