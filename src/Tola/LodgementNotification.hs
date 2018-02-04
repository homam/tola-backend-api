{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tola.LodgementNotification (
    LodgementNotification, PLodgementNotification (..)
  , mkLodgementNotification, fromChargeRequest
) where

import qualified Data.Aeson         as A
import           Data.Time          (UTCTime)
import qualified Tola.ChargeRequest as CR
import           Tola.Common


data PLodgementNotification a t = LodgementNotification {
    accountname       :: Maybe Text
  , amount            :: a
  , amounttype        :: Text
  , channel           :: Text
  , currency          :: Text
  , customerreference :: CustomerReference
  , date              :: t
  , mac               :: Mac
  , msisdn            :: Msisdn
  , operatorreference :: OperatorReference
  , reference         :: Reference
  , sourcereference   :: SourceReference
  , target            :: Target
  , requestType       :: Text
} deriving (Show, Read, Generic)


type LodgementNotification = PLodgementNotification Amount UTCTime

instance (A.ToJSON a, A.ToJSON t) => A.ToJSON (PLodgementNotification a t) where
  toEncoding = toTolaEncoding

instance (A.FromJSON a, A.FromJSON t) => A.FromJSON (PLodgementNotification a t) where
  parseJSON = parseTolaJSON


mkLodgementNotification
  :: Secret
  -> Amount
  -> Msisdn
  -> CustomerReference
  -> OperatorReference
  -> SourceReference
  -> Target
  -> Reference
  -> Text
  -> Maybe Text
  -> UTCTime
  -> LodgementNotification
mkLodgementNotification s a m cr opr sr t ref reqType accName d = LodgementNotification
  { amount            = a
  , amounttype        = "unit"
  , channel           = "KENYA.SAFARICOM"
  , currency          = "KES"
  , customerreference = cr
  , date              = d
  , mac               = toMAC s m d
  , msisdn            = m
  , operatorreference = opr
  , sourcereference   = sr
  , reference         = ref
  , requestType       = reqType
  , accountname       = accName
  , target            = t
  }

fromChargeRequest
  :: Secret
  -> Reference
  -> OperatorReference
  -> CustomerReference
  -> Maybe Text
  -> UTCTime
  -> CR.ChargeRequest
  -> LodgementNotification
fromChargeRequest s ref oref cref accName d cr =
  mkLodgementNotification
  s
  (CR.amount cr)
  (CR.msisdn cr)
  cref
  oref
  (CR.sourcereference cr)
  (CR.target cr)
  ref
  "lodgement"
  accName
  d
