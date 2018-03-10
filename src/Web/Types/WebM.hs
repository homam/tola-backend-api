{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Web.Types.WebM where

import qualified Data.Text.Lazy       as TL
import           Tola.RealTolaApi     (TolaApiConfig (..))
import           Web.Scotty.Trans
import           Web.Types.State
--
import           Control.Monad.Reader
import qualified Data.ByteString      as BS
import qualified Data.Vault.Lazy      as V
import qualified Network.Wai          as W
import           Web.Types.Logger


newtype WebM m a = WebM { unWebM :: ReaderT AppState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState, MonadTrans)

type WebMAction a = ActionT TL.Text (WebM IO) a
type WebMApp a = ScottyT TL.Text (WebM IO) ()

-- instance HasLogger (WebMAction ()) where
--   writeLog = logWeb

runWeb ::
    TolaApiConfig
  -> forall a. (WebM IO) a -> IO a
runWeb tolaApiConfig m = do
  loggerVaultKey <- liftIO V.newKey
  let appState = AppState {
      appVaultLoggerKey = loggerVaultKey
    , appTolaApiConfig = tolaApiConfig
    }
  runReaderT (unWebM m) appState


logWeb :: BS.ByteString -> WebMAction ()
logWeb str = do
  req <- request
  key <- lift $ asks appVaultLoggerKey
  let logger = V.lookup key (W.vault req)
  liftIO $ case logger of
    Nothing      -> return ()
    Just logger' -> logger' str

runWebServer :: Int -> WebMApp b -> IO ()
runWebServer port app = scottyT
  port
  (runWeb
          TolaApiConfig {
            tolaApiBasicAuth = ("", "")
          , tolaApiUrl = ""
          }
  )
  app

