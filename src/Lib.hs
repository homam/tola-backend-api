module Lib
    ( someFunc
    ) where

import           Web.Types.WebM
import           Web.Visit

app = homeWeb

someFunc :: IO ()
someFunc = runWebServer 8080 app
