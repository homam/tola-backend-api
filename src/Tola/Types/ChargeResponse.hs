{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Tola.Types.ChargeResponse (
    ChargeResponse (..)
  , mkSuccessChargeResponse
  , mkFailureChargeResponse
  , toApiError
) where

import           Data.Aeson        ((.:), (.=))
import qualified Data.Aeson        as A
import qualified Data.Aeson.Types  as AT
import           Tola.Imports
import           Tola.Types.Common


data ChargeResponse =
    SuccessChargeResponse { reference :: SourceReference }
  | FailureChargeResponse { code :: Int, message :: Text } deriving (Show, Generic)

mkSuccessChargeResponse :: SourceReference -> ChargeResponse
mkSuccessChargeResponse = SuccessChargeResponse

mkFailureChargeResponse :: Int -> Text -> ChargeResponse
mkFailureChargeResponse = FailureChargeResponse

instance A.ToJSON ChargeResponse where
  toJSON (SuccessChargeResponse ref) = A.object ["success" .= True, "reference" .= ref]
  toJSON (FailureChargeResponse c m) =
    A.object ["success" .= False, "error" .= A.object [ "code" .= c, "message" .= m ]]


instance A.FromJSON ChargeResponse where
  parseJSON (A.Object o) = do
    (tag :: Bool) <- o .: "success"
    if tag
      then SuccessChargeResponse <$> (o .: "reference")
      else do
        e <- o .: "error"
        FailureChargeResponse <$> (e .: "code") <*> (e .: "message")
  parseJSON o = AT.typeMismatch "{ success :: Boolean}" o


toApiError :: ChargeResponse -> Either (ApiError ChargeResponse) ChargeResponse
toApiError r@(FailureChargeResponse _ m) = Left $ mkApiErrorWithDetails (unpack m) r
toApiError r                             = Right r
