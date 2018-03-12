{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tola.Types.ChargeRequest (
    ChargeRequest (..)
  , mkChargeRequest, mkChargeRequest'
  , ChargeRequestState (..)
  , mkChargeRequestStatus, ChargeRequestStatus
) where

import           Control.Arrow         ((+++))
import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as Char8
import           Data.Time.Clock       (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql
import           Text.Read             (readEither)
import           Tola.Imports
import           Tola.Types.Common

data ChargeRequest = ChargeRequest {
    amount          :: Amount
  , amounttype      :: Text -- ^ Whether the amount is in units or centile units (e.g. £ or pence)
  , channel         :: Text
  , currency        :: Text -- ^ The ISO 4217 currency code for this Transaction
  , sourcereference :: ArbitraryReference
  , msisdn          :: Msisdn -- ^ The MSISDN for the Transaction, in full international format
  , requestType     :: Text
  , target          :: Target -- ^ The transfer target of the Transaction (e.g. Paybill, etc.)
  -- , mac             :: Mac
  , date            :: UTCTime
} deriving (Show, Generic)

instance HasTolaMsisdn ChargeRequest where
  tolaMsisdn = msisdn

instance HasTolaTime ChargeRequest where
  tolaTime = date

instance A.ToJSON ChargeRequest where toJSON = toTolaJSON
instance A.FromJSON ChargeRequest where parseJSON = parseTolaJSON

-- | Create a new Tola Charge Request.
-- Test it by:
--
-- >>> (A.encode  <$> mkChargeRequest (mkSecret "secret") (Amount 24) "reference" (Msisdn "30387162221") "800123" <$> getCurrentTime)
--
mkChargeRequest ::
    -- Secret
     Target
  -> Amount
  -> Msisdn
  -> UTCTime
  -> ArbitraryReference
  -> ChargeRequest
mkChargeRequest t a m d sref = ChargeRequest
  { amount          = a
  , amounttype      = "unit"
  , channel         = "KENYA.SAFARICOM"
  , currency        = "KES"
  , sourcereference = sref
  , msisdn          = m
  , requestType     = "charge"
  , target          = t
  -- , mac             = toMAC s m d
  , date            = d
  }

mkChargeRequest' ::
    -- Secret
     Target
  -> Amount
  -> Msisdn
  -> ArbitraryReference
  -> IO ChargeRequest
mkChargeRequest' t a m sref = do
  d <- getCurrentTime
  return $ mkChargeRequest t a m d sref

-- | 'ChargeRequestState' is equivalent to the similar Enum type in 'tola' PotgreSQL database.
data ChargeRequestState =
    ChargeRequestCreated
  | SuccessChargeResponseReceived
  | FailChargeResponseReceived
  | SuccessLodgementNotificationReceived
  | FailLodgementNotificationReceived
  | SuccessDisbursementNotificationReceived
  | FailDisbursementNotificationReceived
  deriving (Show, Read, Eq, Enum, Generic, A.ToJSON, A.FromJSON)

instance PersistField ChargeRequestState where
   toPersistValue = PersistDbSpecific . Char8.pack . show
   fromPersistValue (PersistDbSpecific c) =  (pack +++ id) . readEither . Char8.unpack $ c
   fromPersistValue x = Left $ pack $ "Expected chargerequeststate, got: " ++ show x

instance PersistFieldSql ChargeRequestState where
  sqlType _ = SqlOther "chargerequeststate"


data ChargeRequestStatus = ChargeRequestStatus {
    _state        :: ChargeRequestState
  , _reference    :: Maybe SourceReference
  , _errorMessage :: Maybe Text
} deriving (Show, Read, Generic)

instance  A.ToJSON ChargeRequestStatus where
  toJSON = toTolaJSON

instance A.FromJSON ChargeRequestStatus where
  parseJSON = parseTolaJSON


mkChargeRequestStatus
  :: ChargeRequestState
  -> Maybe SourceReference
  -> Maybe Text
  -> ChargeRequestStatus
mkChargeRequestStatus = ChargeRequestStatus

