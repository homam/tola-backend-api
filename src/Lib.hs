module Lib
    ( main
    ) where

import qualified Data.ByteString.Char8 as Char8
import qualified System.Environment    as Env
import           Tola.Types.Common
import           Web.RealWebApp
import           Web.Visit

app :: RealWebApp IO ()
app   =  homeWeb
      >> lodgementNotificationWeb
      >> disbursementNotificationWeb
      >> chargeRequestWeb
      >> checkChargeRequestWeb
      >> doMigrationsWeb

main :: IO ()
main = do
    db             <- Env.getEnv "db"
    secret         <- fmap mkSecret' (Env.getEnv "tola_secret")
    port <- fmap read (Env.getEnv "port")
    url <- Env.getEnv "tola_url"
    auth <- (,) <$> (Char8.pack <$> Env.getEnv "tola_username") <*> (Char8.pack <$> Env.getEnv "tola_password")
    runWebServer
              (Char8.pack db)
              secret
              url
              auth
              port
              app
