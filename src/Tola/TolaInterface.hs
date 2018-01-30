{-# LANGUAGE OverloadedStrings #-}

module Tola.TolaInterface (
    TolaApi (..)
  , realTolaApi
  -- , TolaApiS (..), TolaApiSMaker (..), makeTolaApiS
) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Identity
import qualified Tola.ChargeRequest           as TChargeRequest
import qualified Tola.ChargeResponse          as TChargeResponse
import           Tola.Common
import           Tola.Imports
---
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Char8        as B
import           Data.ByteString.Lazy         (fromStrict, pack, toStrict)
import qualified Data.Maybe                   as Y
import           Data.Word                    (Word8)
import           Network.HTTP.Conduit

newtype TolaApiM m a = TolaApiM { unTolaApiM :: ReaderT Secret m a }

data TolaApi = TolaApi {
  makeChargeRequest :: TChargeRequest.ChargeRequest -> IO TChargeResponse.ChargeResponse
}

realTolaApi :: TolaApi
realTolaApi = TolaApi {
  -- https://api.ea.oxygen8.com/sammedia
  -- makeChargeRequest = const $ return $ TChargeResponse.FailureChargeResponse 37366 "Real Tola API not implemented"
  makeChargeRequest = \ req -> do
      res <- post "https://api.ea.oxygen8.com/sammedia" req
      return $ Y.fromJust (A.decode $ fromStrict res) -- TODO: change fromJust

}
---
-- "https://requestb.in/sz1pc4sz"

post :: A.ToJSON t => String -> t -> IO B.ByteString
post url obj = do
  manager <- newManager tlsManagerSettings
  r <- parseUrl url
  let json = A.encode obj
  let request = applyBasicAuth "sammedia" "ZDMzNjY3" $ r {
      secure = True
    , method = "POST"
    , requestBody = RequestBodyBS (toStrict json)
    , requestHeaders = (requestHeaders r) ++ [("Content-Type",  "application/json")]
    }
  print request
  print (toStrict json)
  response <- httpLbs request manager
  print $ responseBody response
  return $ toStrict $ responseBody response

