{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tola.Types.Common (
    module Tola.Types.Common
) where

import qualified Crypto.Hash.MD5        as MD5
import qualified Data.Aeson             as A
import qualified Data.Aeson.Types       as AT
import qualified Data.ByteString.Base16 as B16
import           Data.Data              (Data)
import qualified Data.HashMap.Lazy      as HashMap
import qualified Data.Text              as T
import           Data.Text.Encoding     as E
import           Data.Time              (UTCTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Data.Typeable          (Typeable)
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics
import           Numeric                (readFloat, showFFloat)
-- import           Tola.Imports

-- | MD5 hash
hashText :: T.Text -> T.Text
hashText = E.decodeUtf8 . B16.encode . MD5.hash . E.encodeUtf8

class HasTolaTime t where
  tolaTime :: t -> UTCTime

-- | Creates a MAC digest code using MD5 hash function according to Tola docs v1.9
toMAC :: Secret -> Msisdn -> UTCTime -> Mac
toMAC s m d = mkMac $ hashText mac'
 where
  mac' = T.intercalate ":" [unMsisdn m, T.pack (encodeTime d), unSecret s]
  encodeTime = formatTime defaultTimeLocale "%FT%T%QZ"

newtype Secret = Secret { unSecret :: T.Text } deriving (Show, Generic)
mkSecret :: T.Text -> Secret
mkSecret = Secret

mkSecret' :: String -> Secret
mkSecret' = mkSecret . T.pack

class HasTolaSecret t where
  tolaSecret :: t -> Secret

mergeAeson :: [A.Value] -> A.Value
mergeAeson = A.Object . HashMap.unions . map (\(A.Object x) -> x)

data MACed t = MACed Secret t deriving (Generic)

instance (Generic r, A.ToJSON r, HasTolaMsisdn r, HasTolaTime r) => A.ToJSON (MACed r) where
  toJSON (MACed s r) = mergeAeson
    [ A.object ["mac" A..= toMAC s (tolaMsisdn r) (tolaTime r)]
    , A.toJSON r
    ]

class ToMACed m where
  toMACed :: r -> m (MACed r)

addMAC ::
     (AT.ToJSON a, HasTolaTime t, HasTolaMsisdn t, HasTolaSecret t)
  => a
  -> t
  -> AT.Value
addMAC a r = mergeAeson [A.object ["mac" A..= toMAC (tolaSecret r) (tolaMsisdn r) (tolaTime r)], A.toJSON a]

-- JSON utilities
-- | Default Tola JSON options
tolaJSONOptions :: AT.Options
tolaJSONOptions = A.defaultOptions
  { AT.fieldLabelModifier    = fixType . sanitize
  , AT.allNullaryToStringTag = True
  }
  where
    sanitize ('_':xs) = xs -- drop preceding underscore for lenses
    sanitize y        = y
    fixType = \case
      "requestType" -> "type"
      x             -> x

toTolaJSON :: (Generic a, AT.GToJSON AT.Zero (Rep a)) => a -> AT.Value
toTolaJSON = A.genericToJSON tolaJSONOptions

-- | Use to create 'FromJSON' instances
-- @
-- parseJSON = parseTolaJSON
-- @
parseTolaJSON
  :: (AT.GFromJSON AT.Zero (Rep a), Generic a) => AT.Value -> AT.Parser a
parseTolaJSON = A.genericParseJSON tolaJSONOptions

-- | Represents the 'amount' field in LodgementReuest object
-- The 'amount' field is passed as a String, here we assume it is of Decimal type
newtype Amount = Amount { unAmount :: Rational } deriving (Show, Generic)

mkAmount :: Rational -> Amount
mkAmount = Amount

newtype OuiSysCampaignId = OuiSysCampaignId { unOuiSysCampaignId :: Int } 
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkOuiSysCampaignId :: Int -> OuiSysCampaignId
mkOuiSysCampaignId = OuiSysCampaignId

-- | Tola passes Amount as String in JSON
instance PersistField Amount where
   toPersistValue = PersistDouble . fromRational . unAmount
   fromPersistValue (PersistDouble c) = Right . Amount . toRational $ c
   fromPersistValue (PersistRational c) = Right . Amount $ c
   fromPersistValue x = Left $ T.pack $ "Expected Double, got: " ++ show x

instance PersistFieldSql Amount where
  sqlType _ = SqlReal

instance A.ToJSON Amount where
  toJSON =
    AT.String
      . T.pack
      . (\f -> showFFloat (Just 0) f "") -- Tola amount does not support "10.0"
      . (fromRational :: Rational -> Double)
      . unAmount


instance A.FromJSON Amount where
  parseJSON = fmap Amount . parseAmount where
    parseAmount (AT.Number o) = return $ toRational o
    parseAmount (AT.String o) = return (fst $ head $ readFloat (T.unpack o))
    parseAmount o             = AT.typeMismatch "Number or String" o


-- | Wraps Msisdn in a newtype
newtype Msisdn = Msisdn { unMsisdn :: T.Text }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkMsisdn :: T.Text -> Msisdn
mkMsisdn = Msisdn

class HasTolaMsisdn t where
  tolaMsisdn :: t -> Msisdn

-- | Wraps AffiliateId in a newtype
newtype AffiliateId = AffiliateId { unAffiliateId :: T.Text }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkAffiliateId :: T.Text -> AffiliateId
mkAffiliateId = AffiliateId

class HasTolaAffiliateId t where
  tolaAffiliateId :: t -> AffiliateId

-- | Wraps Url in a newtype
newtype Url = Url { unUrl :: T.Text }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkUrl :: T.Text -> Url
mkUrl = Url

-- | Wraps CampaignId in a newtype
newtype CampaignId = CampaignId { unCampaignId :: Int }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkCampaignId :: Int -> CampaignId
mkCampaignId = CampaignId


-- | Wraps Pixel Template Id in a newtype
newtype PixelTemplateId = PixelTemplateId { unPixelTemplateId :: Int }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkPixelTemplateId :: Int -> PixelTemplateId
mkPixelTemplateId = PixelTemplateId


-- | Wraps Source Reference in a newtype
newtype SourceReference = SourceReference { unSourceReference :: T.Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkSourceReference :: T.Text -> SourceReference
mkSourceReference = SourceReference

mkSourceReferenceFromInt :: (Integral i, Show i) => i -> SourceReference
mkSourceReferenceFromInt = SourceReference . T.pack . show

mkSourceReferenceFromString :: String -> SourceReference
mkSourceReferenceFromString = SourceReference . T.pack

-- | Wraps Arbitrary Reference in a newtype
newtype ArbitraryReference = ArbitraryReference { unArbitraryReference :: T.Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkArbitraryReference :: T.Text -> ArbitraryReference
mkArbitraryReference = ArbitraryReference

mkArbitraryReferenceFromString :: String -> ArbitraryReference
mkArbitraryReferenceFromString = ArbitraryReference . T.pack


-- | Wraps Customer Reference in a newtype
newtype CustomerReference = CustomerReference { unCustomerReference :: T.Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkCustomerReference :: T.Text -> CustomerReference
mkCustomerReference = CustomerReference

-- | Wraps Operator Reference in a newtype
newtype OperatorReference = OperatorReference { unOperatorReference :: T.Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkOperatorReference :: T.Text -> OperatorReference
mkOperatorReference = OperatorReference

-- | Wraps Target in a newtype
newtype Target = Target { unTarget :: T.Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkTarget :: T.Text -> Target
mkTarget = Target

-- | Wraps Mac in a newtype
newtype Mac = Mac { unMac :: T.Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkMac :: T.Text -> Mac
mkMac = Mac

-- | The standard reponse from our server to Tola requests
data SuccessResponse = SuccessResponse { success :: Bool, _successResponse :: () } deriving (Show, Read, Generic)
instance A.ToJSON SuccessResponse
instance A.FromJSON SuccessResponse

mkSuccessResponse :: Bool -> SuccessResponse
mkSuccessResponse = (`SuccessResponse` ())

---

data ApiError d = ApiError { errorMessage :: String, details :: d } deriving (Show, Read, Generic)

instance A.ToJSON d => A.ToJSON (ApiError d)
instance A.FromJSON d => A.FromJSON (ApiError d)

mkApiError :: String -> ApiError ()
mkApiError = (`ApiError` ())

mkApiErrorWithDetails :: String -> d -> ApiError d
mkApiErrorWithDetails = ApiError
