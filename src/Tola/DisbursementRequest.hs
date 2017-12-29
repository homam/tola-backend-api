{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tola.DisbursementRequest (
    DisbursementRequest (..)
  , mkDisbursementRequest
) where

import qualified Data.Aeson   as A
import           Tola.Common
import           Tola.Imports


data DisbursementRequest = DisbursementRequest {
    amount          :: Amount
  , amounttype      :: Text
  , channel         :: Text
  , currency        :: Text
  , sourcereference :: Text
  , msisdn          :: Msisdn
  , requestType     :: Text
  , target          :: Text
  , mac             :: Text
  , date            :: UTCTime
} deriving (Show, Generic)

instance A.ToJSON DisbursementRequest where toEncoding = toTolaEncoding
instance A.FromJSON DisbursementRequest where parseJSON = parseTolaJSON

-- | Create a new Tola Disbursement Request.
-- Test it by:
-- (A.encode  <$> mkDisbursementRequest (mkSecret "secret") (Amount 24) "reference" (Msisdn "30387162221") "800123" <$> getCurrentTime)
mkDisbursementRequest
  ::    Secret
     -> Amount
     -> Text
     -> Msisdn
     -> Text
     -> UTCTime
     -> DisbursementRequest
mkDisbursementRequest s a r m t d = DisbursementRequest {
      amount = a
    , amounttype = "unit"
    , channel = "KENYA.SAFARICOM"
    , currency = "KES"
    , sourcereference = r
    , msisdn = m
    , requestType = "disbursement"
    , target = t
    , mac = toMAC s m d
    , date = d
  }
