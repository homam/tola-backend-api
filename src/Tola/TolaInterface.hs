{-# LANGUAGE OverloadedStrings #-}

module Tola.TolaInterface (
    TolaInterface (..)
  , realTolaInterface
) where

import qualified Tola.ChargeRequest  as TChargeRequest
import qualified Tola.ChargeResponse as TChargeResponse
import           Tola.Common
import           Tola.Imports



data TolaInterface = TolaInterface {
  makeChargeRequest :: TChargeRequest.ChargeRequest -> IO TChargeResponse.ChargeResponse
}

realTolaInterface :: TolaInterface
realTolaInterface = TolaInterface {
  makeChargeRequest = error "Not implemented"
}
