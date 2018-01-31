{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Tola.Common (
    module Tola.Common
  , Text, pack, unpack, Generic
) where

import qualified Crypto.Hash.MD5        as MD5
import qualified Data.Aeson             as A
import qualified Data.Aeson.Types       as AT
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text              as T
import           Data.Text.Encoding     as E
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Database.Persist
import           Database.Persist.Sql
import           Debug.Trace            (trace)
import           GHC.Generics
import           Numeric                (readFloat, showFloat)
import           Tola.Imports

-- | MD5 hash
hashText :: Text -> Text
hashText = E.decodeUtf8 . B16.encode . MD5.hash . E.encodeUtf8

-- | Creates a MAC digest code using MD5 hash function according to Tola docs v1.9
toMAC :: Secret -> Msisdn -> UTCTime -> Mac
toMAC s m d = mkMac $ trace (T.unpack mac') (hashText mac')
 where
  mac' = T.intercalate ":" [(unMsisdn m), pack (encodeTime d), (unSecret s)]
  encodeTime = formatTime defaultTimeLocale "%FT%T%QZ"

newtype Secret = Secret { unSecret :: Text } deriving (Show, Generic)
mkSecret :: Text -> Secret
mkSecret = Secret

mkSecret' :: String -> Secret
mkSecret' = mkSecret . T.pack

-- JSON utilities
-- | Default Tola JSON options
tolaJSONOptions :: AT.Options
tolaJSONOptions = A.defaultOptions
  { AT.fieldLabelModifier    = \case
    "requestType" -> "type"
    x             -> x
  , AT.allNullaryToStringTag = True
  }

-- | Use to create 'ToJSON' instances
-- @
-- toEncoding = toTolaEncoding
-- @
toTolaEncoding
  :: (AT.GToEncoding AT.Zero (Rep a), Generic a) => a -> AT.Encoding
toTolaEncoding = A.genericToEncoding tolaJSONOptions

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

-- | Tola passes Amount as String in JSON
instance PersistField Amount where
   toPersistValue = PersistDouble . fromRational . unAmount
   fromPersistValue (PersistDouble c) = Right . Amount . toRational $ c
   fromPersistValue x = Left $ pack $ "Expected Double, got: " ++ show x

instance PersistFieldSql Amount where
  sqlType _ = SqlReal

instance A.ToJSON Amount where
  toJSON = AT.String . pack . (`showFloat` "") . (fromRational :: Rational -> Double) . unAmount

instance A.FromJSON Amount where
  parseJSON = fmap Amount . parseAmount where
    parseAmount (AT.Number o) = return $ toRational o
    parseAmount (AT.String o) = return (fst $ head $ readFloat (unpack o))
    parseAmount o             = AT.typeMismatch "Number or String" o


-- | Wraps Msisdn in a newtype
newtype Msisdn = Msisdn { unMsisdn :: Text }
    deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkMsisdn :: Text -> Msisdn
mkMsisdn = Msisdn

-- | Wraps Source Reference in a newtype
newtype SourceReference = SourceReference { unSourceReference :: Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkSourceReference :: Text -> SourceReference
mkSourceReference = SourceReference

mkSourceReferenceFromInt :: (Integral i, Show i) => i -> SourceReference
mkSourceReferenceFromInt = SourceReference . pack . show

mkSourceReferenceFromString :: String -> SourceReference
mkSourceReferenceFromString = SourceReference . pack


-- | Wraps  Reference in a newtype
newtype Reference = Reference { unReference :: Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkReference :: Text -> Reference
mkReference = Reference

-- | Wraps Customer Reference in a newtype
newtype CustomerReference = CustomerReference { unCustomerReference :: Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkCustomerReference :: Text -> CustomerReference
mkCustomerReference = CustomerReference

-- | Wraps Operator Reference in a newtype
newtype OperatorReference = OperatorReference { unOperatorReference :: Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkOperatorReference :: Text -> OperatorReference
mkOperatorReference = OperatorReference

-- | Wraps Target in a newtype
newtype Target = Target { unTarget :: Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkTarget :: Text -> Target
mkTarget = Target

-- | Wraps Mac in a newtype
newtype Mac = Mac { unMac :: Text }
  deriving (Show, Read, Eq, Ord, Generic, A.ToJSON, A.FromJSON, PersistField, PersistFieldSql)

mkMac :: Text -> Mac
mkMac = Mac

-- | The standard reponse from our server to Tola requests
data SuccessResponse = SuccessResponse { success :: Bool } deriving (Show, Read, Generic)
instance A.ToJSON SuccessResponse
instance A.FromJSON SuccessResponse

mkSuccessResponse :: Bool -> SuccessResponse
mkSuccessResponse = SuccessResponse
