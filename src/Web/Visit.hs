{-# LANGUAGE OverloadedStrings #-}

module Web.Visit where

import           Control.Monad.Trans.Class (lift)
import           Network.HTTP.Types.Status
import           Web.Logging.MonadLogger
import           Web.Scotty.Trans
import           Web.ScottyHelpers
import           Web.Types.WebM

homeWeb ::  WebMApp ()
homeWeb = getAndPostAndHead "/" $ do
  writeLog "request to /"
  status status404 >> text "homeWeb"
