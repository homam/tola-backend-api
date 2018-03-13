module Lib
    ( someFunc
    ) where

import           Web.RealWebApp
import           Web.Visit

app :: RealWebApp IO ()
app =  homeWeb
    >> chargeRequestWeb

someFunc :: IO ()
someFunc = runWebServer 8080 app
