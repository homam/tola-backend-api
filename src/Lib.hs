{-# LANGUAGE OverloadedStrings #-}

module Lib (
   main
  ) where

import Web.Visit
import qualified Web.WebM as W
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Database.Redis as R


myApp :: W.WebMApp ()
myApp = doMigrationsWeb >> msisdnSubmissionWeb >> pinSubmissionWeb

main :: Int -> String -> IO ()
main port db = W.runWebServer port R.defaultConnectInfo (E.encodeUtf8 $ T.pack db) myApp
