{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tola.DisbursementNotification (
    DisbursementNotification (..)
  , DisbursementNotificationDetails (..)
  , mkDisbursementNotificationDetails
  , mkSuccessDisbursementNotification
  , mkFailureDisbursementNotification
  , fromChargeRequest
  , successAndError
) where

import           Control.Arrow      ((|||))
import           Data.Aeson         ((.:), (.:?), (.=))
import qualified Data.Aeson         as A
import qualified Data.Aeson.Types   as AT
import qualified Data.HashMap.Lazy  as HML
import           Data.Time          (UTCTime)
import           Text.Read          (readEither)
import qualified Tola.ChargeRequest as CR
import           Tola.Common

mergeAeson :: [A.Value] -> A.Value
mergeAeson = A.Object . HML.unions . map (\(A.Object x) -> x)

-- | A Disbursement Notification occurs when an asynchronous Disbursement Request is received and queued by Tola,
-- and subsequently the EMoney supplier confirms that the transaction has completed.
data DisbursementNotificationDetails = DisbursementNotificationDetails {
    amount            :: Amount
  , amounttype        :: Text
  , channel           :: Text
  , currency          :: Text
  , customerreference :: CustomerReference
  , date              :: UTCTime
  , mac               :: Mac
  , msisdn            :: Msisdn
  , operatorreference :: OperatorReference -- ^ A mobile network provided reference
  , sourcereference   :: SourceReference -- ^ Apparently matches 'Tola.ChargeResponse.reference' of 'Tola.ChargeResponse.SuccessChargeResponse'
  , target            :: Target
  , requestType       :: Text
  } deriving (Show, Generic)


-- 2018-02-05T11:49:40+0000:
-- ----Start>
-- RequestId: 151783138058487
-- POST /tola/disbursement_notification
--   Request Body: {
--    "accountname" : "LEE MBUGUA KAIRIANJA",
--    "amount" : "10.0",
--    "amounttype" : "unit",
--    "channel" : "KENYA.SAFARICOM",
--    "currency" : "KES",
--    "customerreference" : "dcdf9f73531.a",
--    "date" : "2018-02-05T11:49:39Z",
--    "errormessage" : "rejected",
--    "mac" : "5149CB3E373D5564AFA57A14FEEBCD92",
--    "msisdn" : "254797561830",
--    "operatorreference" : "09f757d20a78be8a53ecde41effa8152",
--    "sourcereference" : "1.100.1517831319.1",
--    "success" : "false",
--    "target" : "850702",
--    "type" : "notification"
-- }

--   Request Headers:
--     x-requestid: 151783138058487
--     connection: upgrade
--     x-forwarded-for: 197.248.3.186
--     host: tola-api.sam-media.com
--     content-length: 520
--     user-agent: Oxygen8 HTTP Client
--     accept: */*
--     content-type: application/json
--   Raw Query String:
--   Status: 500 Error in $.success: expected Bool, encountered String 0.000098s
--   Response Body: {"success":false}
-- ----End>

instance A.ToJSON DisbursementNotificationDetails
instance A.FromJSON DisbursementNotificationDetails

-- | The data in Charge Notification callback from Tola
-- Example of a notification in JSON:
-- @
-- {
--   "amount" : "100.0",
--   "amounttype" : "unit",
--   "channel" : "KENYA.SAFARICOM",
--   "currency" : "KES",
--   "customerreference" : "JKLMNOPQ",
--   "date" : "2016-12-14T03:40:04Z",
--   "mac" : "a9993e364706816aba3e25717850c26c9cd0d89d",
--   "msisdn" : "25412345678",
--   "operatorreference" : "ABCDEFGHI",
--   "sourcereference" : "1.123.1435455096.1",
--   "success" : "true",
--   "target" : "800000",
--   "type" : "notification"
-- }
-- @
data DisbursementNotification =
    SuccessDisbursementNotification { details :: DisbursementNotificationDetails }
  | FailureDisbursementNotification { errorMessage :: Text, details :: DisbursementNotificationDetails } deriving (Show, Generic)

successAndError :: DisbursementNotification -> (Bool, Maybe Text)
successAndError (FailureDisbursementNotification e _) = (False, Just e)
successAndError _                                     = (True, Nothing)

instance A.ToJSON DisbursementNotification where
  toJSON (SuccessDisbursementNotification det) =
    mergeAeson [A.object ["success" .= True], toTolaJSON det]
  toJSON (FailureDisbursementNotification m det) =
    mergeAeson [A.object ["success" .= False, "errormessage" .= m], toTolaJSON det]

instance A.FromJSON DisbursementNotification where
  parseJSON j@(A.Object o) = do
    (tag :: Bool) <- isSuccess o
    if tag
      then SuccessDisbursementNotification <$> parseTolaJSON j
      else FailureDisbursementNotification <$> (o .: "errormessage") <*> parseTolaJSON j
      where
          isSuccess :: AT.Object -> AT.Parser Bool
          isSuccess o = do
            -- (s :: Either String Bool)  <- maybeToRight "" <$> (o .:? "success")
            (b :: Either String Bool)  <- toBool <$> (o .: "success")
            -- return $ (const (const False ||| id $  s) ||| id) b
            return (const False ||| id $ b)

          toBool "true"  = Right True
          toBool "false" = Right False
          toBool x       = Left $ "Cannot parse to bool " ++ x

  parseJSON o = AT.typeMismatch "{ success :: Boolean}" o

mkDisbursementNotification :: Either Text () -> DisbursementNotificationDetails -> DisbursementNotification
mkDisbursementNotification (Left e) = mkFailureDisbursementNotification e
mkDisbursementNotification _        = mkSuccessDisbursementNotification


mkDisbursementNotificationDetails
  :: Secret
  -> Amount
  -> Msisdn
  -> CustomerReference
  -> OperatorReference
  -> SourceReference
  -> Target
  -> Text
  -> UTCTime
  -> DisbursementNotificationDetails
mkDisbursementNotificationDetails s a m cref oref sref t rtype d = DisbursementNotificationDetails
  { amount          = a
  , amounttype      = "unit"
  , channel         = "KENYA.SAFARICOM"
  , currency        = "KES"
  , customerreference = cref
  , date            = d
  , mac             = toMAC s m d
  , msisdn          = m
  , operatorreference = oref
  , sourcereference = sref
  , target          = t
  , requestType = rtype
  }

mkSuccessDisbursementNotification :: DisbursementNotificationDetails -> DisbursementNotification
mkSuccessDisbursementNotification = SuccessDisbursementNotification

mkFailureDisbursementNotification :: Text -> DisbursementNotificationDetails -> DisbursementNotification
mkFailureDisbursementNotification = FailureDisbursementNotification

fromChargeRequest
  :: Secret
  -> OperatorReference
  -> CustomerReference
  -> UTCTime
  -> CR.ChargeRequest
  -> DisbursementNotification
fromChargeRequest s oref cref d cr =
  mkDisbursementNotification (Right ()) $ mkDisbursementNotificationDetails
    s
    (CR.amount cr)
    (CR.msisdn cr)
    cref
    oref
    (CR.sourcereference cr)
    (CR.target cr)
    "notification"
    d


maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y
