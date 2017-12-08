{-# LANGUAGE OverloadedStrings #-}

module Web.Utils.LogMiddleware (
  logAllMiddleware
  ) where

import           Control.Monad         (forM_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive  as CI
import qualified Data.List             as L
import           Data.Monoid           ((<>))
import qualified Data.Time             as Time
import qualified Network.Wai           as W


logAllMiddleware :: W.Middleware
logAllMiddleware app req respond = do
  B8.putStrLn "----"
  now <- Time.getCurrentTime
  let headers' = foldl1 (<>) $ L.intersperse "\n  " $ map (\(k, v) -> CI.foldedCase k <> ": " <> v) (W.requestHeaders req)
  let path = W.rawPathInfo req
  let queryString = W.rawQueryString req
  body <- W.requestBody req
  forM_ (L.intersperse (B8.putStrLn "  ---") $ map (B8.putStrLn . ("  " <>)) [B8.pack (show now), path <> queryString, headers', body]) id
  app req respond
