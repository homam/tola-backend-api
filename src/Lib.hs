{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances               #-}

module Lib (
   main
  ) where

import Web.Visit
import qualified Web.WebM as W
import qualified Data.Text.Encoding as E
import qualified Data.Text as T

myApp :: W.WebMApp ()
myApp = doMigrationsWeb >> msisdnSubmissionWeb >> pinSubmissionWeb

main :: Int -> String -> IO ()
main port db = W.runWebServer port (E.encodeUtf8 $ T.pack db) myApp
