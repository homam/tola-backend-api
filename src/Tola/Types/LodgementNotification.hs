{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tola.Types.LodgementNotification (
    LodgementNotification, PLodgementNotification (..)
  , mkLodgementNotification, fromChargeRequest
) where

import qualified Data.Aeson               as A
import           Data.Time                (UTCTime)
import           Tola.Imports
import qualified Tola.Types.ChargeRequest as CR
import           Tola.Types.Common


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
  , reference         :: SourceReference
  , sourcereference   :: ArbitraryReference
  , target            :: Target
  , requestType       :: Text
} deriving (Show, Read, Generic)


type LodgementNotification = PLodgementNotification Amount UTCTime

instance (A.ToJSON a, A.ToJSON t) => A.ToJSON (PLodgementNotification a t) where
  toEncoding = toTolaEncoding

instance (A.FromJSON a, A.FromJSON t) => A.FromJSON (PLodgementNotification a t) where
  parseJSON = parseTolaJSON


mkLodgementNotification ::
     Secret
  -> Amount
  -> Msisdn
  -> CustomerReference
  -> OperatorReference
  -> ArbitraryReference
  -> Target
  -> SourceReference
  -> Text
  -> Maybe Text
  -> UTCTime
  -> LodgementNotification
mkLodgementNotification s a m cr opr sref t ref reqType accName d = LodgementNotification
  { amount            = a
  , amounttype        = "unit"
  , channel           = "KENYA.SAFARICOM"
  , currency          = "KES"
  , customerreference = cr
  , date              = d
  , mac               = toMAC s m d
  , msisdn            = m
  , operatorreference = opr
  , sourcereference   = sref
  , reference         = ref
  , requestType       = reqType
  , accountname       = accName
  , target            = t
  }

fromChargeRequest
  :: Secret
  -> SourceReference
  -> OperatorReference
  -> CustomerReference
  -> Maybe Text
  -> UTCTime
  -> CR.ChargeRequest
  -> LodgementNotification
fromChargeRequest s sref oref cref accName d cr =
  mkLodgementNotification
  s
  (CR.amount cr)
  (CR.msisdn cr)
  cref
  oref
  (CR.sourcereference cr)
  (CR.target cr)
  sref
  "lodgement"
  accName
  d
