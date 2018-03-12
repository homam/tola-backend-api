{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Web.Types.ChargeRequestClientResponse (
  module Web.Types.ChargeRequestClientResponse
) where

import qualified Data.Aeson                as A
import           Tola.Imports
import           Tola.Types.ChargeResponse (ChargeResponse)
import           Tola.Types.Common

data ChargeRequestClientResponse = ChargeRequestClientResponse {
    tolaResponse    :: ChargeResponse
  , chargeRequestId :: SourceReference
} deriving (Show, Generic, A.FromJSON, A.ToJSON)

mkChargeRequestClientResponse
  :: SourceReference -> ChargeResponse -> ChargeRequestClientResponse

mkChargeRequestClientResponse crid cresp =
  ChargeRequestClientResponse {tolaResponse = cresp, chargeRequestId = crid}
