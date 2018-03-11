{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Visit where

import           Network.HTTP.Types.Status
import           Tola.MonadTolaApi
import           Web.Logging.MonadLogger
import           Web.Scotty.Trans
import           Web.ScottyHelpers
import           Web.Types.WebM
--
import           Control.Monad.Trans       (MonadIO, lift, liftIO)
import qualified Data.Text.Lazy            as TL
import           Tola.Types.ChargeRequest
import           Tola.Types.Common

homeWeb :: WebAction
homeWeb = getAndPostAndHead "/" $ do
  writeLog "request to /"
  req <- liftIO $ mkChargeRequest' (mkSecret "secret") (mkTarget "0000") (mkAmount 23) (mkMsisdn "0292883") (mkArbitraryReference "someref")
  makeChargeRequest req
  status status404 >> text "homeWeb"
