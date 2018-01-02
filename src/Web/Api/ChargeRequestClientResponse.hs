{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Web.Api.ChargeRequestClientResponse (
  module Web.Api.ChargeRequestClientResponse
) where

import qualified Data.Aeson          as A
import           Tola.ChargeResponse (ChargeResponse)
import           Tola.Common

data ChargeRequestClientResponse = ChargeRequestClientResponse {
    tolaResponse    :: ChargeResponse
  , chargeRequestId :: SourceReference
} deriving (Show, Generic, A.FromJSON, A.ToJSON)

mkChargeRequestClientResponse
  :: SourceReference -> ChargeResponse -> ChargeRequestClientResponse

mkChargeRequestClientResponse crid cresp = ChargeRequestClientResponse {
    tolaResponse = cresp
  , chargeRequestId = crid
  }
