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
  , sourcereference :: SourceReference
  , msisdn          :: Msisdn
  , requestType     :: Text
  , target          :: Target
  , mac             :: Mac
  , date            :: UTCTime
} deriving (Show, Generic)

instance A.ToJSON DisbursementRequest where toEncoding = toTolaEncoding
instance A.FromJSON DisbursementRequest where parseJSON = parseTolaJSON

-- | Create a new Tola Disbursement Request.
mkDisbursementRequest
  ::    Secret
     -> Amount
     -> SourceReference
     -> Msisdn
     -> Target
     -> UTCTime
     -> DisbursementRequest
mkDisbursementRequest s a sr m t d = DisbursementRequest {
      amount = a
    , amounttype = "unit"
    , channel = "KENYA.SAFARICOM"
    , currency = "KES"
    , sourcereference = sr
    , msisdn = m
    , requestType = "disbursement"
    , target = t
    , mac = toMAC s m d
    , date = d
  }
