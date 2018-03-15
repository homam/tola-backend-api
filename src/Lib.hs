module Lib
    ( main
    ) where

import qualified Data.ByteString.Char8 as Char8
import qualified System.Environment    as Env
import           Tola.Types.Common
import qualified Web.Apps.MockWebApp   as MockWebApp
import qualified Web.Apps.RealWebApp   as RealWebApp
import qualified Web.Visit             as Visit

main :: IO ()
main = do
    db             <- Env.getEnv "db"
    secret         <- fmap mkSecret' (Env.getEnv "tola_secret")
    port           <- fmap read (Env.getEnv "port")
    url            <- Env.getEnv "tola_url"
    auth           <- (,) <$> (Char8.pack <$> Env.getEnv "tola_username") <*> (Char8.pack <$> Env.getEnv "tola_password")
    isMock         <- fmap (== Just "true") (Env.lookupEnv "mocked")
    if isMock
        then MockWebApp.runWebServer
            (Char8.pack db)
            secret
            url
            auth
            port
            Visit.app

        else RealWebApp.runWebServer
            (Char8.pack db)
            secret
            url
            auth
            port
            Visit.app
