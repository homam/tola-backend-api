{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Tola.ChargeRequest (
    ChargeRequest (..)
  , mkChargeRequest
  , ChargeRequestState (..)
) where

import qualified Data.Aeson          as A
import           Database.Persist.TH
import           Tola.Common
import           Tola.Imports


data ChargeRequest = ChargeRequest {
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

instance A.ToJSON ChargeRequest where toEncoding = toTolaEncoding
instance A.FromJSON ChargeRequest where parseJSON = parseTolaJSON

-- | Create a new Tola Charge Request.
-- Test it by:
-- @
-- (A.encode  <$> mkChargeRequest (mkSecret "secret") (Amount 24) "reference" (Msisdn "30387162221") "800123" <$> getCurrentTime)
-- @
mkChargeRequest
  ::    Secret
     -> Target
     -> Amount
     -> Msisdn
     -> UTCTime
     -> SourceReference
     -> ChargeRequest
mkChargeRequest s t a m d r = ChargeRequest {
      amount = a
    , amounttype = "unit"
    , channel = "KENYA.SAFARICOM"
    , currency = "KES"
    , sourcereference = r
    , msisdn = m
    , requestType = "charge"
    , target = t
    , mac = toMAC s m d
    , date = d
  }

data ChargeRequestState =
    ChargeRequestCreated
  | SuccessChargeResponseReceived
  | FailChargeResponseReceived
  | SuccessChargeNotificationReceived
  | FailChargeNotificationReceived
  deriving (Show, Read, Eq, Enum, Generic)

derivePersistField "ChargeRequestState"

instance A.ToJSON ChargeRequestState
instance A.FromJSON ChargeRequestState




