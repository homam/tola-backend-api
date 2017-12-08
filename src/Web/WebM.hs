{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Web.WebM (
    module Web.WebM
  , DB.fromSqlKey
  , ActionT, ScottyT, ScottyError, param, text, json, params, get, post, options, redirect, request, status, header, headers, addHeader, addroute
) where

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
import           Web.AppState
import           Web.Model
import           Web.Scotty                  (RoutePattern)
import           Web.Scotty.Trans            (ActionT, ScottyError, ScottyT,
                                              addHeader, addroute, get, header,
                                              headers, json, middleware,
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

runWeb :: R.Connection -> P.Pool PS.Connection -> DB.ConnectionPool -> forall a. WebM a -> IO a
runWeb redisConn jewlPool pool = runActionToIO where
  appState = AppState {
        echo = putStrLn . (unpack :: Text -> String)
      , runRedis = R.runRedis redisConn
      , runSql = (`DB.runSqlPool` pool)
      , runJewl = P.withResource jewlPool
    }
  runActionToIO m = runReaderT (unWebM m) appState

runWebM :: R.ConnectInfo -> DB.ConnectionString -> DB.ConnectionString -> WebM b -> IO b
runWebM redisConnInfo jewlConnStr connStr a = do
  redisConn <- R.checkedConnect redisConnInfo
  jewlPool <- P.createPool (PS.connectPostgreSQL jewlConnStr) PS.close 1 10 10

  runApp connStr (\pool -> runWeb redisConn jewlPool pool a)


addServerHeader :: W.Middleware
addServerHeader =
  W.modifyResponse (W.mapResponseHeaders (("Server", "Scotch") :))

runWebServer :: Int -> R.ConnectInfo -> DB.ConnectionString -> DB.ConnectionString -> WebMApp b -> IO ()
runWebServer port redisConnInfo jewlConnStr connStr a = do
  redisConn <- R.checkedConnect redisConnInfo
  jewlPool <- P.createPool (PS.connectPostgreSQL jewlConnStr) PS.close 1 10 10

  runApp connStr (\pool -> scottyT port (runWeb redisConn jewlPool pool) (middleware logAllMiddleware >> middleware addServerHeader >> a))

