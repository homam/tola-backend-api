{-# LANGUAGE OverloadedStrings #-}

module Web.Visit where

import           Network.HTTP.Types.Status
import           Web.Scotty.Trans
import           Web.ScottyHelpers
import           Web.Types.WebM

homeWeb ::  WebMApp ()
homeWeb = getAndPostAndHead "/" $ do
  logWeb "request to /"
  status status404 >> text "homeWeb"
