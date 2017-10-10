{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Web.WebM (
    module Web.WebM
  , DB.fromSqlKey
  , ActionT, ScottyT, ScottyError, param, text, params, get, post, options, redirect, request, status, header, headers, addHeader, addroute
) where

import           Control.Monad.Reader                 (MonadReader, MonadIO)
import           Control.Monad.Trans.Reader           (ReaderT (..), runReaderT)
import           Data.Text
import qualified Data.Text.Lazy                       as TL
import           Network.HTTP.Types                   (StdMethod (..))
import           Web.AppState
import           Web.Scotty                           (ActionM, RoutePattern,
                                                       ScottyM)
import           Web.Scotty.Trans                     (ActionT, ScottyError,
                                                       ScottyT, addHeader,
                                                       addroute, get, header,
                                                       headers, options, param,
                                                       params, post, redirect,
                                                       request, status, text)

import           Data.Monoid                          ((<>))

import           Data.Pool                            ()
import qualified Database.Persist.Postgresql          as DB
import           Web.Model



newtype WebM a = WebM { unWebM :: ReaderT AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

type WebMAction a = ActionT TL.Text WebM a
type WebMApp a = ScottyT TL.Text WebM ()

shead :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
shead = addroute HEAD

getAndHead a b = get a b >> shead a b
postAndHead a b = post a b >> shead a b
getAndPostAndHead a b = get a b >> post a b >> shead a b

addScotchHeader :: Monad m => TL.Text -> TL.Text -> ActionT e m ()
addScotchHeader name = addHeader ("X-Scotch-" <> name)

connStr = "host=localhost dbname=test"

runWebM :: WebM b -> IO b
runWebM a = runApp connStr (runWeb' a)

runWeb' :: WebM a -> DB.ConnectionPool -> IO a
runWeb' a pool = do
  let appState = AppState {
        echo = putStrLn . (unpack :: Text -> String)
      , getPool = pool
    }

  let runActionToIO m = runReaderT (unWebM m) appState
  runActionToIO a
