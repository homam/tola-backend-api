module Lib (
   main, mkSecret'
  ) where

import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import qualified Database.Redis     as R
import           Tola.Common
import qualified Tola.TolaInterface as Tola
import           Web.Visit
import qualified Web.WebM           as W


myApp :: W.WebMApp ()
myApp =
  doMigrationsWeb

    -- Tola API
    >> echoWeb
    >> lodgementNotificationWeb
    >> lodgementNotificationWeb

    -- Client API
    >> chargeRequestWeb


main :: Int -> String -> String -> Secret -> IO ()
main port jewlDb db secret =
  W.runWebServer port
                 W.simpleStdoutLogType
                 R.defaultConnectInfo
                 (E.encodeUtf8 $ T.pack jewlDb)
                 (E.encodeUtf8 $ T.pack db)
                 secret
                 Tola.realTolaApi
                 myApp

