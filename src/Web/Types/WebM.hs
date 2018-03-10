{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

module Web.Types.WebM where

import qualified Data.Text.Lazy            as TL
import           Tola.RealTolaApi          (TolaApiConfig (..))
import           Web.Scotty.Trans
import           Web.Types.State
--
import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import qualified Data.ByteString           as BS

newtype WebM m a = WebM { unWebM :: ReaderT AppState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState, MonadTrans)

type WebMAction a = ActionT TL.Text (WebM IO) a
type WebMApp a = ScottyT TL.Text (WebM IO) ()

runWeb ::
     (BS.ByteString -> IO ())
  -> TolaApiConfig
  -> forall a. (WebM IO) a -> IO a
runWeb logger tolaApiConfig = runActionToIO
 where
  appState = AppState {
      appWriteLog = logger
    , appTolaApiConfig = tolaApiConfig
    }
  runActionToIO :: (WebM IO) a -> IO a
  runActionToIO m = runReaderT (unWebM m) appState


logWeb :: BS.ByteString -> WebMAction ()
logWeb x = do
  logger <- lift $ asks appWriteLog
  liftIO $ logger x
  return ()

runWebServer :: Int -> WebMApp b -> IO ()
runWebServer port app = scottyT
  port
  (runWeb BS.putStrLn
          TolaApiConfig {
            tolaApiBasicAuth = ("", "")
          , tolaApiUrl = ""
          }
  )
  app

