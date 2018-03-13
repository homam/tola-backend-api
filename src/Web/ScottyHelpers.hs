{-# LANGUAGE OverloadedStrings #-}

module Web.ScottyHelpers where

import           Control.Monad.Trans
import           Data.Monoid         ((<>))
import qualified Data.Text.Lazy      as TL
import           Network.HTTP.Types  (StdMethod (..))
import qualified Network.Wai         as W
import           Web.Scotty.Trans

shead
  :: (ScottyError e, MonadIO m)
  => RoutePattern
  -> ActionT e m ()
  -> ScottyT e m ()
shead = addroute HEAD

getAndHead, postAndHead, getAndPostAndHead, getAndHeadAccessOrigin
  :: (ScottyError e, MonadIO m)
  => RoutePattern
  -> ActionT e m ()
  -> ScottyT e m ()
getAndHead a b = get a b >> shead a b
postAndHead a b = post a b >> shead a b
getAndPostAndHead a b = get a b >> post a b >> shead a b

getAndHeadAccessOrigin a b = do
  shead a add
  get a (add >> b)
  where add = addHeader "Access-Control-Allow-Origin" "*"

addScotchHeader :: Monad m => TL.Text -> TL.Text -> ActionT e m ()
addScotchHeader name = addHeader ("X-Scotch-" <> name)

addServerHeader :: W.Middleware
addServerHeader =
  W.modifyResponse (W.mapResponseHeaders (("Server", "Scotch") :))
