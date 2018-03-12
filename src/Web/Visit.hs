{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Visit where

import           Control.Monad.Trans      (liftIO)
import           Tola.Database.Model
import           Tola.MonadTolaApi
import           Tola.Types.ChargeRequest
import           Tola.Types.Common
import           Web.Logging.MonadLogger
import           Web.Scotty.Trans
import           Web.ScottyHelpers
import           Web.Types.WebApp

homeWeb :: WebApp
homeWeb = getAndPostAndHead "/" $ do
  writeLog "request to /"
  req <- liftIO $ mkChargeRequest' (mkSecret "secret") (mkTarget "0000") (mkAmount 23) (mkMsisdn "0292883") (mkArbitraryReference "someref")
  insertChargeRequest req
  res <- makeChargeRequest req
  json res
