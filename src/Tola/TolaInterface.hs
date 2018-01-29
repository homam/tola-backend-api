{-# LANGUAGE OverloadedStrings #-}

module Tola.TolaInterface (
    TolaApi (..)
  , realTolaApi
  -- , TolaApiS (..), TolaApiSMaker (..), makeTolaApiS
) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Identity
import qualified Tola.ChargeRequest           as TChargeRequest
import qualified Tola.ChargeResponse          as TChargeResponse
import           Tola.Common
import           Tola.Imports

newtype TolaApiM m a = TolaApiM { unTolaApiM :: ReaderT Secret m a }

data TolaApi = TolaApi {
  makeChargeRequest :: TChargeRequest.ChargeRequest -> IO TChargeResponse.ChargeResponse
}

realTolaApi :: TolaApi
realTolaApi = TolaApi {
  makeChargeRequest = error "NA" -- const $ return $ TChargeResponse.FailureChargeResponse 37366 "Real Tola API not implemented" -- error "Not implemented"
}

-- data TolaApiS = TolaApiS {
--   makeChargeRequestS :: (Secret -> TChargeRequest.ChargeRequest) -> IO TChargeResponse.ChargeResponse
-- }

-- newtype TolaApiSMaker = TolaApiSMaker {
--   runTolaApiSMaker :: Secret  -> TolaApi -> TolaApiS
-- }

-- makeTolaApiS :: TolaApiSMaker
-- makeTolaApiS = TolaApiSMaker {
--   runTolaApiSMaker = \ secret api -> TolaApiS {
--     makeChargeRequestS = \f -> (makeChargeRequest api) (f secret)
--   }
-- }

-- runTolaApiS :: TolaApiS -> (TolaApiS -> b) -> b
-- runTolaApiS apiS f = f apiS
