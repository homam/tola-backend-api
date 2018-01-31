{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Web.WebM (
    getAndHead , postAndHead , getAndPostAndHead
  , addScotchHeader, addServerHeader
  , runWebM, runWebServer
  , simpleStdoutLogType, noneLogType , fileLogType
  , DB.fromSqlKey
  , WebMApp, WebMAction, WebM, addMiddlewares
  , ActionT, ScottyT, ScottyError, param, text, json, params, body, get, post, options, redirect, request, status, header, headers, addHeader, addroute
) where

import           Control.Monad                      (void)
import           Control.Monad.Catch                (onException)
import           Control.Monad.Reader               (MonadIO, MonadReader,
                                                     liftIO)
import           Control.Monad.Trans.Reader         (ReaderT (..), runReaderT)
import           Data.ByteString                    (ByteString)
import           Data.Monoid                        ((<>))
import qualified Data.Pool                          as P
import           Data.Text                          (Text, unpack)
import qualified Data.Text.Lazy                     as TL
import qualified Database.Persist.Postgresql        as DB
import qualified Database.PostgreSQL.Simple         as PS
import qualified Database.Redis                     as R
import           Network.HTTP.Types                 (StdMethod (..))
import qualified Network.Wai                        as W
import           System.Log.FastLogger              as FL
import           System.Log.FastLogger              (ToLogStr)
import           Tola.Common                        (Secret)
import           Tola.TolaInterface                 (TolaApi)
import           Web.AppState
import qualified Web.Localization                   as WL
import           Web.Model
import           Web.Scotty                         (RoutePattern)
import           Web.Scotty.Trans                   (ActionT, ScottyError,
                                                     ScottyT, addHeader,
                                                     addroute, body, get,
                                                     header, headers, json,
                                                     middleware, options, param,
                                                     params, post, redirect,
                                                     request, scottyAppT,
                                                     scottyT, status, text)
import           Web.Utils.DetailedLoggerMiddleware (detailedMiddleware)


newtype WebM a = WebM { unWebM :: ReaderT AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

type WebMAction a = ActionT TL.Text WebM a
type WebMApp a = ScottyT TL.Text WebM ()

fromWebM :: AppState -> WebM a -> WebMAction a
fromWebM s m = liftIO $ runReaderT (unWebM m) s

shead :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
shead = addroute HEAD

getAndHead, postAndHead, getAndPostAndHead :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
getAndHead a b = get a b >> shead a b
postAndHead a b = post a b >> shead a b
getAndPostAndHead a b = get a b >> post a b >> shead a b

addScotchHeader :: Monad m => TL.Text -> TL.Text -> ActionT e m ()
addScotchHeader name = addHeader ("X-Scotch-" <> name)

runWeb :: (ByteString -> IO ()) -> R.Connection -> P.Pool PS.Connection -> DB.ConnectionPool -> Secret -> TolaApi -> forall a. WebM a -> IO a
runWeb myLogger redisConn jewlPool pool sec ti = runActionToIO where
  appState = AppState {
        echo = putStrLn . (unpack :: Text -> String)
      , runRedis = R.runRedis redisConn
      , runSql = (`DB.runSqlPool` pool)
      , runJewl = P.withResource jewlPool
      , tolaApi = ti
      , logIO = myLogger
      , secret = sec
    }
  runActionToIO :: WebM a -> IO a
  runActionToIO m = runReaderT (unWebM m) appState

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

runWebM :: IO LogType -> R.ConnectInfo -> ByteString -> DB.ConnectionString -> Secret -> TolaApi -> ScottyT e WebM () -> IO W.Application
runWebM logType redisConnInfo jewlConnStr connStr sec ti =
  addMiddlewares myApp' logType
  where
    myApp' :: (ByteString -> IO ()) -> ScottyT e WebM () -> IO W.Application
    myApp' logger = scottyAppT (\ app -> do
      redisConn <- onException (R.checkedConnect redisConnInfo) (putStrLn "Error: Cannot connect to redis")
      jewlPool <- P.createPool (PS.connectPostgreSQL jewlConnStr) PS.close 1 10 10
      runApp connStr (\pool ->
            runWeb logger redisConn jewlPool pool sec ti app
          )
      )

runWebServer :: Int -> IO FL.LogType -> R.ConnectInfo -> DB.ConnectionString -> DB.ConnectionString -> Secret -> TolaApi -> WebMApp b -> IO ()
runWebServer port logType redisConnInfo jewlConnStr connStr sec ti a = do
  redisConn <- onException (R.checkedConnect redisConnInfo) (putStrLn "Error: Cannot connect to redis")
  jewlPool <- P.createPool (PS.connectPostgreSQL jewlConnStr) PS.close 1 10 10
  addMiddlewares
    (\ logger app ->
      runApp
        connStr
        (\ pool ->
            scottyT port (runWeb logger redisConn jewlPool pool sec ti) app
        )
    ) logType a


addMiddlewares :: ToLogStr s => (
  (s -> IO ()) -> ScottyT e WebM () -> IO b) -- ^ A callback function receiving a logger and a Scotty app. Use either 'runWebServer' or 'runWebM' for the callback.
  -> IO FL.LogType -> ScottyT e WebM () -> IO b
addMiddlewares cb logType a = do
  let uniqueIdGenerator = WL.uniqueTimestamp 100000 =<< WL.newMVar 0
  withLogger logType (\logger -> do
      logger'' <- detailedMiddleware logger uniqueIdGenerator True
      cb (logger . toLogStr) (middleware logger'' >> middleware addServerHeader >> a)
    )

addServerHeader :: W.Middleware
addServerHeader =
  W.modifyResponse (W.mapResponseHeaders (("Server", "Scotch") :))
