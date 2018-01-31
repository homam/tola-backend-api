{-# LANGUAGE OverloadedStrings #-}

module Tola.TolaInterface (
    TolaApi (..)
  , realTolaApi
  -- , TolaApiS (..), TolaApiSMaker (..), makeTolaApiS
) where

import           Control.Monad.Reader
import qualified Tola.ChargeRequest    as TChargeRequest
import qualified Tola.ChargeResponse   as TChargeResponse
import           Tola.Common
---
import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy  (fromStrict, toStrict)
import qualified Data.Maybe            as Y
import qualified Network.HTTP.Conduit  as C

newtype TolaApiM m a = TolaApiM { unTolaApiM :: ReaderT Secret m a }

data TolaApi = TolaApi {
  makeChargeRequest :: TChargeRequest.ChargeRequest -> IO TChargeResponse.ChargeResponse
}

realTolaApi :: TolaApi
realTolaApi = TolaApi {
  -- https://api.ea.oxygen8.com/sammedia
  -- makeChargeRequest = const $ return $ TChargeResponse.FailureChargeResponse 37366 "Real Tola API not implemented"
  makeChargeRequest = \ req -> do
      res <- post "https://requestb.in/u47ujvu4" req
      return $ Y.fromJust (A.decode $ fromStrict res) -- TODO: change fromJust

}
---
-- "https://requestb.in/sz1pc4sz"

post :: A.ToJSON t => String -> t -> IO B.ByteString
post url obj = do
  manager <- C.newManager C.tlsManagerSettings
  r <- C.parseUrl url
  let json = A.encode obj
  let request = C.applyBasicAuth "sammedia" "ZDMzNjY3" $ r {
      C.secure = True
    , C.method = "POST"
    , C.requestBody = C.RequestBodyBS (toStrict json)
    , C.requestHeaders = (C.requestHeaders r) ++ [("Content-Type",  "application/json")]
    }
  print request
  B.putStrLn (toStrict json)
  response <- C.httpLbs request manager
  print $ C.responseBody response
  return $ toStrict $ C.responseBody response

