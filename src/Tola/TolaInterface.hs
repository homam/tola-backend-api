{-# LANGUAGE OverloadedStrings #-}

module Tola.TolaInterface (
    TolaApi (..)
  , realTolaApi
) where

import qualified Tola.ChargeRequest  as TChargeRequest
import qualified Tola.ChargeResponse as TChargeResponse
import           Tola.Common
import           Tola.Imports



data TolaApi = TolaApi {
  makeChargeRequest :: TChargeRequest.ChargeRequest -> IO TChargeResponse.ChargeResponse
}

realTolaApi :: TolaApi
realTolaApi = TolaApi {
  makeChargeRequest = error "Not implemented"
}
