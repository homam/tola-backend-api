{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Web.Types.WebM where

import qualified Data.Text.Lazy                       as TL
import           Tola.RealTolaApi                     (TolaApiConfig (..))
import           Web.Scotty.Trans
import           Web.Types.State
--
import           Control.Monad.Reader
import qualified Data.Vault.Lazy                      as V
import qualified Network.Wai                          as W
import           Tola.MonadTolaApi
import           Web.Logging.Logger
import           Web.Logging.MonadLogger
--
import           Data.Monoid                          ((<>))
import           System.Log.FastLogger                as FL
import qualified Web.Crypto                           as WC
import           Web.Logging.DetailedLoggerMiddleware (detailedMiddleware)



newtype WebM m a = WebM { unWebM :: ReaderT AppState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState, MonadTrans, MonadTolaApi, MonadLogger)

type WebMAction a = ActionT TL.Text (WebM IO) a
type WebMApp a = ScottyT TL.Text (WebM IO) ()

instance MonadLogger (ActionT TL.Text (WebM IO)) where
  writeLog str = do
    req <- request
    key <- lift $ asks appVaultLoggerKey
    let logger = V.lookup key (W.vault req)
    liftIO $ case logger of
      Nothing      -> return ()
      Just logger' -> logger' str


runWeb ::
     VaultLoggerKey

  -> TolaApiConfig
  -> forall a. (WebM IO) a -> IO a
runWeb loggerVaultKey tolaApiConfig app = do
  let appState = AppState {
      appVaultLoggerKey = loggerVaultKey
    , appTolaApiConfig = tolaApiConfig
    }
  runReaderT (unWebM app) appState

runWebServer :: Int -> WebMApp b -> IO ()
runWebServer port app = do
  loggerVaultKey <- V.newKey
  addMiddlewares
    loggerVaultKey
    (
      scottyT
        port
        (runWeb loggerVaultKey
                TolaApiConfig {
                  tolaApiBasicAuth = ("", "")
                , tolaApiUrl = ""
                }
        )
    )
    simpleStdoutLogType
    app


simpleStdoutLogType :: FL.LogType
simpleStdoutLogType = FL.LogStdout FL.defaultBufSize

noneLogType :: FL.LogType
noneLogType = FL.LogNone

fileLogType :: FilePath -> FL.LogType
fileLogType path =
  let spec = FL.FileLogSpec path 10 10
  in  FL.LogFile spec FL.defaultBufSize

withLogger :: forall msg b.
     ToLogStr msg
  => FL.LogType
  -> ((msg -> IO ()) -> IO b)
  -> IO b
withLogger logType f = do
  timeCache         <- FL.newTimeCache myTimeFormat
  (logger, cleanUp) <- FL.newTimedFastLogger timeCache logType
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


addMiddlewares :: (Monad m) =>
     VaultLoggerKey
  -> (ScottyT e (WebM m) () -> IO b) -- ^ A callback function receiving a logger and a Scotty app. Use either 'runWebServer' or 'runWebM' for the callback.
  -> FL.LogType -> ScottyT e (WebM m) () -> IO b
addMiddlewares loggerVaultKey cb logType a = do
  let uniqueIdGenerator = WC.uniqueTimestamp (100000 :: Double) =<< WC.newMVar 0
  withLogger logType (\logger -> do
      logger'' <- detailedMiddleware loggerVaultKey logger uniqueIdGenerator True
      cb (middleware logger'' >> a) --  middleware addServerHeader >>
    )
