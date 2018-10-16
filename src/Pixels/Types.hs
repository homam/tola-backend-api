{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Pixels.Types
  (
    module Pixels.Types
  )
where

import qualified Data.Aeson           as A
import qualified Data.ByteString      as BS
import qualified Data.CaseInsensitive as CI
import           Data.Data
import qualified Data.HashMap.Strict  as HMap
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import qualified Data.Text.Lazy       as TL
import           Data.Text.Template
import           Network.HTTP.Types   (parseQueryText)
import qualified Network.Wai          as W
import           Tola.Imports
import           Tola.Types.Common

type QueryString = M.Map (CI.CI String) String

data PixelInput = PixelInput {
  msisdn      :: Maybe Msisdn
, queryString :: QueryString
} deriving (Show, Read, Eq, Ord, Data, Typeable, Generic) -- , A.ToJSON, A.FromJSON)

queryStringFromJson :: A.Value -> QueryString
queryStringFromJson (A.Object o) =
  M.fromList
  $ map (\(k, v) -> (CI.mk $ unpack k, unpack $ toText v))
  $ HMap.toList o
  where
    toText (A.String v) = v
    toText _            = ""
queryStringFromJson _ = M.empty

queryStringParams :: W.Request -> QueryString
queryStringParams = M.fromList . parseEncodedParams . W.rawQueryString
  where
    parseEncodedParams :: BS.ByteString -> [(CI.CI String, String)]
    parseEncodedParams bs = [ (CI.mk $ unpack k, unpack $ fromMaybe "" v) | (k, v) <- parseQueryText bs ]

-- | Format a pixel url
-- Test it by:
--
-- @
--     let pi = PixelInput (Just $ mkMsisdn "9747123431") (M.fromList [("sub_id", "s192845"), ("rockman_id", "some_rockman_id")])
--     in formatPixelUrl pi (mkUrl "http://httpbin.org/get/?msisdn=$msisdn$&sub_id=$q.sub_id$")
-- @
-- formatPixelUrl :: PixelInput -> Url -> Url
-- formatPixelUrl qs = mkUrl . render . (setAttribute "msisdn" ( maybe "" unMsisdn $ msisdn qs) . setAttribute "q" (queryString qs)) . newSTMP . unpack . unUrl
formatPixelUrl :: PixelInput -> Url -> Url
formatPixelUrl pxin templ = mkUrl $ TL.toStrict $ substitute (unUrl templ) context where
  context = pack . context' . unpack
  context' :: String -> String
  context' ('q':'_':k) = fromMaybe ("QUERY_STRING_NOT_FOUND (" ++ k ++ ")") $
    M.lookup
      (CI.mk k)
      (queryString pxin)
  context' "msisdn" = maybe "NO_MSISDN" (unpack . unMsisdn) (msisdn pxin)
  context' k = "KEY_NOT_DEFINED (" ++ k ++ ")"

{--
test =
  let pi = PixelInput (Just $ mkMsisdn "9747123431") (M.fromList [("sub_id", "s192845"), ("rockman_id", "some_rockman_id")])
  in formatPixelUrl pi (mkUrl "http://httpbin.org/get/?msisdn=$msisdn&sub_id=${q_sub_id}")
--}
