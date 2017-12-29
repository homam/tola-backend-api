{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}


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
import           GHC.Generics
import           Numeric                (readFloat, showFloat)
import           Tola.Imports

-- | MD5 hash
hashText :: Text -> Text
hashText = E.decodeUtf8 . B16.encode . MD5.hash . E.encodeUtf8

-- | To Tola MAC
toMAC :: Secret -> Msisdn -> UTCTime -> Text
toMAC s m d = hashText $ T.intercalate ":" [(unMsisdn m), pack (encodeTime d), (unSecret s)] where
  encodeTime = formatTime defaultTimeLocale "%FT%T%QZ"

newtype Secret = Secret { unSecret :: Text } deriving (Show, Generic)
mkSecret :: Text -> Secret
mkSecret = Secret

-- JSON utilities
-- | Default Tola JSON options
tolaJSONOptions :: AT.Options
tolaJSONOptions = A.defaultOptions
  { AT.fieldLabelModifier = \case
    "requestType" -> "type"
    x             -> x
  }

-- | Use to create 'ToJSON' instances
-- @
-- toEncoding = toTolaEncoding
-- @
toTolaEncoding :: (AT.GToEncoding AT.Zero (Rep a), Generic a) => a -> AT.Encoding
toTolaEncoding = A.genericToEncoding tolaJSONOptions

-- | Use to create 'FromJSON' instances
-- @
-- parseJSON = parseTolaJSON
-- @
parseTolaJSON :: (AT.GFromJSON AT.Zero (Rep a), Generic a) => AT.Value -> AT.Parser a
parseTolaJSON = A.genericParseJSON tolaJSONOptions

-- | Represents the 'amount' field in LodgementReuest object
-- The 'amount' field is passed as a String, here we assume it is of Decimal type
newtype Amount = Amount { unAmount :: Rational } deriving (Show, Generic)

mkAmount :: Rational -> Amount
mkAmount = Amount

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


-- | Wraps MSISDN in a newtype
newtype Msisdn = Msisdn { unMsisdn :: Text } deriving (Show, Read, Generic)

mkMsisdn :: Text -> Msisdn
mkMsisdn = Msisdn

instance PersistField Msisdn where
   toPersistValue = PersistText . unMsisdn
   fromPersistValue (PersistText c) = Right . Msisdn $ c
   fromPersistValue x = Left $ pack $ "Expected Text, got: " ++ show x

instance PersistFieldSql Msisdn where
  sqlType _ = SqlString

instance A.ToJSON Msisdn where
  toJSON = A.toJSON . unMsisdn

instance A.FromJSON Msisdn where
  parseJSON = fmap Msisdn . AT.parseJSON

-- | Wraps Source Reference in a newtype
newtype SourceReference = SourceReference { unSourceReference :: Text } deriving (Show, Read, Generic)

mkSourceReference :: Text -> SourceReference
mkSourceReference = SourceReference

instance PersistField SourceReference where
   toPersistValue = PersistText . unSourceReference
   fromPersistValue (PersistText c) = Right . SourceReference $ c
   fromPersistValue x = Left $ pack $ "Expected Text, got: " ++ show x

instance PersistFieldSql SourceReference where
  sqlType _ = SqlString

instance A.ToJSON SourceReference where
  toJSON = A.toJSON . unSourceReference

instance A.FromJSON SourceReference where
  parseJSON = fmap SourceReference . AT.parseJSON


-- | The standard reponse from our server to Tola requests
data SuccessResponse = SuccessResponse { success :: Bool } deriving (Show, Read, Generic)
instance A.ToJSON SuccessResponse
instance A.FromJSON SuccessResponse

mkSuccessResponse :: Bool -> SuccessResponse
mkSuccessResponse = SuccessResponse
