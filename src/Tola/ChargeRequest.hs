{-# LANGUAGE DeriveAnyClass    #-}
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
  , amounttype      :: Text -- ^ Whether the amount is in units or centile units (e.g. £ or pence)
  , channel         :: Text
  , currency        :: Text -- ^ The ISO 4217 currency code for this Transaction
  , sourcereference :: SourceReference
  , msisdn          :: Msisdn -- ^ The MSISDN for the Transaction, in full international format
  , requestType     :: Text
  , target          :: Target -- ^ The transfer target of the Transaction (e.g. Paybill, etc.)
  , mac             :: Mac
  , date            :: UTCTime
} deriving (Show, Generic)

instance A.ToJSON ChargeRequest where toEncoding = toTolaEncoding
instance A.FromJSON ChargeRequest where parseJSON = parseTolaJSON

-- | Create a new Tola Charge Request.
-- Test it by:
--
-- >>> (A.encode  <$> mkChargeRequest (mkSecret "secret") (Amount 24) "reference" (Msisdn "30387162221") "800123" <$> getCurrentTime)
--
mkChargeRequest
  ::    Secret
     -> Target
     -> Amount
     -> Msisdn
     -> UTCTime
     -> SourceReference
     -> ChargeRequest
mkChargeRequest s t a m d sref = ChargeRequest {
      amount = a
    , amounttype = "unit"
    , channel = "KENYA.SAFARICOM"
    , currency = "KES"
    , sourcereference = sref
    , msisdn = m
    , requestType = "charge"
    , target = t
    , mac = toMAC s m d
    , date = d
  }

-- | 'ChargeRequestState' is equivalent to the similar Enum type in 'tola' PotgreSQL database.
data ChargeRequestState =
    ChargeRequestCreated
  | SuccessChargeResponseReceived
  | FailChargeResponseReceived
  | SuccessChargeNotificationReceived
  | FailChargeNotificationReceived
  deriving (Show, Read, Eq, Enum, Generic, A.ToJSON, A.FromJSON)

derivePersistField "ChargeRequestState"
