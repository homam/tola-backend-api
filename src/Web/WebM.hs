{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Web.WebM (
    module Web.WebM
  , DB.fromSqlKey
  , ActionT, ScottyT, ScottyError, param, text, json, params, body, get, post, options, redirect, request, status, header, headers, addHeader, addroute
) where

import           Control.Monad               (void)
import           Control.Monad.Reader        (MonadIO, MonadReader)
import           Control.Monad.Trans.Reader  (ReaderT (..), runReaderT)
import           Data.Monoid                 ((<>))
import           Data.Pool                   ()

import qualified Data.Pool                   as P

import           Data.Text
import qualified Data.Text.Encoding          as E
import qualified Data.Text.Lazy              as TL
import qualified Database.Persist.Postgresql as DB
import qualified Database.PostgreSQL.Simple  as PS
import qualified Database.Redis              as R
import           Network.HTTP.Types          (StdMethod (..))
import qualified Network.Wai                 as W
import           System.Log.FastLogger       as FL
import           Tola.TolaInterface          (TolaApi)
import           Web.AppState
import           Web.Model
import           Web.Scotty                  (RoutePattern)
import           Web.Scotty.Trans            (ActionT, ScottyError, ScottyT,
                                              addHeader, addroute, body, get,
                                              header, headers, json, middleware,
                                              options, param, params, post,
                                              redirect, request, scottyT,
                                              status, text)
import           Web.Utils.LogMiddleware     (logAllMiddleware)

newtype WebM a = WebM { unWebM :: ReaderT AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

type WebMAction a = ActionT TL.Text WebM a
type WebMApp a = ScottyT TL.Text WebM ()

shead :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
shead = addroute HEAD

getAndHead, postAndHead, getAndPostAndHead :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
getAndHead a b = get a b >> shead a b
postAndHead a b = post a b >> shead a b
getAndPostAndHead a b = get a b >> post a b >> shead a b

addScotchHeader :: Monad m => TL.Text -> TL.Text -> ActionT e m ()
addScotchHeader name = addHeader ("X-Scotch-" <> name)

runWeb :: IO FL.LogType -> R.Connection -> P.Pool PS.Connection -> DB.ConnectionPool -> TolaApi -> forall a. WebM a -> IO a
runWeb logType redisConn jewlPool pool ti = runActionToIO where
  appState myLogger = AppState {
        echo = putStrLn . (unpack :: Text -> String)
      , runRedis = R.runRedis redisConn
      , runSql = (`DB.runSqlPool` pool)
      , runJewl = P.withResource jewlPool
      , tolaApi = ti
      , logIO = myLogger
    }
  runActionToIO m = withLogger logType (runReaderT (unWebM m) . appState)

withLogger :: forall msg b. ToLogStr msg => IO FL.LogType -> ((msg -> IO ()) -> IO b) -> IO b
withLogger logType f = do
  lt <- logType
  timeCache <- FL.newTimeCache myTimeFormat
  (logger, cleanUp) <- FL.newTimedFastLogger timeCache lt
  let myLogger = void . logT logger
  a <- f myLogger
  cleanUp
  return a
  where
    myTimeFormat :: TimeFormat
    myTimeFormat = "%Y-%m-%dT%H:%M:%S%z"

    logT :: FL.ToLogStr msg => FL.TimedFastLogger -> msg -> IO ()
    logT logger msg = logger
      $ \ft -> toLogStr ft <> toLogStr (": " :: String) <> toLogStr msg <> "\n"

simpleStdoutLogType :: IO FL.LogType
simpleStdoutLogType = return $ FL.LogStdout FL.defaultBufSize

noneLogType :: IO FL.LogType
noneLogType = return FL.LogNone

fileLogType :: FilePath -> IO FL.LogType
fileLogType path =
  let spec = FL.FileLogSpec path 10 10
  in return $ FL.LogFile spec FL.defaultBufSize


runWebM :: IO FL.LogType -> R.ConnectInfo -> DB.ConnectionString -> DB.ConnectionString -> TolaApi -> WebM b -> IO b
runWebM logType redisConnInfo jewlConnStr connStr ti a = do
  redisConn <- R.checkedConnect redisConnInfo
  jewlPool <- P.createPool (PS.connectPostgreSQL jewlConnStr) PS.close 1 10 10

  runApp connStr (\pool -> runWeb logType redisConn jewlPool pool ti a)


addServerHeader :: W.Middleware
addServerHeader =
  W.modifyResponse (W.mapResponseHeaders (("Server", "Scotch") :))

runWebServer :: Int -> IO FL.LogType -> R.ConnectInfo -> DB.ConnectionString -> DB.ConnectionString -> TolaApi -> WebMApp b -> IO ()
runWebServer port logType redisConnInfo jewlConnStr connStr ti a = do
  redisConn <- R.checkedConnect redisConnInfo
  jewlPool <- P.createPool (PS.connectPostgreSQL jewlConnStr) PS.close 1 10 10
  runApp connStr (\pool -> scottyT port (runWeb logType redisConn jewlPool pool ti) (middleware logAllMiddleware >> middleware addServerHeader >> a))

