{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Tola.ChargeNotification (
    ChargeNotification (..)
  , ChargeNotificationDetails (..)
  , mkChargeNotificationDetails
  , mkSuccessChargeNotification
  , mkFailureChargeNotification
  , fromChargeRequest
) where

import           Data.Aeson         ((.:), (.=))
import qualified Data.Aeson         as A
import qualified Data.Aeson.Types   as AT
import qualified Data.HashMap.Lazy  as HML
import           Data.Time          (UTCTime)
import qualified Tola.ChargeRequest as CR
import           Tola.Common

mergeAeson :: [A.Value] -> A.Value
mergeAeson = A.Object . HML.unions . map (\(A.Object x) -> x)

data ChargeNotificationDetails = ChargeNotificationDetails {
    amount            :: Amount
  , amounttype        :: Text
  , channel           :: Text
  , currency          :: Text
  , customerreference :: CustomerReference
  , date              :: UTCTime
  , mac               :: Mac
  , msisdn            :: Msisdn
  , operatorreference :: OperatorReference
  , sourcereference   :: SourceReference
  , target            :: Target
  } deriving (Show, Generic)

instance A.ToJSON ChargeNotificationDetails
instance A.FromJSON ChargeNotificationDetails

data ChargeNotification =
    SuccessChargeNotification { details :: ChargeNotificationDetails }
  | FailureChargeNotification { errorMessage :: Text, details :: ChargeNotificationDetails } deriving (Show, Generic)

instance A.ToJSON ChargeNotification where
  toJSON (SuccessChargeNotification det) =
    mergeAeson [A.object ["success" .= True], A.toJSON det]
  toJSON (FailureChargeNotification m det) =
    mergeAeson [A.object ["success" .= False, "errormessage" .= m], A.toJSON det]

instance A.FromJSON ChargeNotification where
  parseJSON j@(A.Object o) = do
    (tag :: Bool) <- o .: "success"
    if tag
      then SuccessChargeNotification <$> A.parseJSON j
      else FailureChargeNotification <$> (o .: "errormessage") <*> A.parseJSON j
  parseJSON o = AT.typeMismatch "{ success :: Boolean}" o

mkChargeNotificationDetails
  :: Secret
  -> Amount
  -> Msisdn
  -> CustomerReference
  -> OperatorReference
  -> SourceReference
  -> Target
  -> UTCTime
  -> ChargeNotificationDetails
mkChargeNotificationDetails s a m cr opr sr t d = ChargeNotificationDetails
  { amount          = a
  , amounttype      = "unit"
  , channel         = "KENYA.SAFARICOM"
  , currency        = "KES"
  , customerreference = cr
  , date            = d
  , mac             = toMAC s m d
  , msisdn          = m
  , operatorreference = opr
  , sourcereference = sr
  , target          = t
  }

mkSuccessChargeNotification :: ChargeNotificationDetails -> ChargeNotification
mkSuccessChargeNotification = SuccessChargeNotification

mkFailureChargeNotification :: Text -> ChargeNotificationDetails -> ChargeNotification
mkFailureChargeNotification = FailureChargeNotification

fromChargeRequest :: Secret -> SourceReference -> OperatorReference -> CustomerReference -> UTCTime -> CR.ChargeRequest -> ChargeNotificationDetails
fromChargeRequest s sref oref cref d cr = mkChargeNotificationDetails s (CR.amount cr) (CR.msisdn cr) cref oref sref (CR.target cr) d


