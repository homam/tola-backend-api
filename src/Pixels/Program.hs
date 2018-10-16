{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Pixels.Program
where

import qualified Control.Concurrent              as Concurrent
import qualified Control.Concurrent.MVar         as MVar
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Base64.URL      as Base64
import qualified Data.ByteString.Char8           as Char8
import           Data.ByteString.Lazy            (toStrict)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as Encoding
import qualified Network.HTTP.Conduit            as C
import qualified Network.HTTP.Types.Status       as Status
import qualified Pixels.Types                    as PT
import qualified System.Environment              as Env
import           Tola.Database.Helpers
import           Tola.Database.MonadTolaDatabase
import           Tola.Types.Common
import           Web.Logging.MonadLogger



data AppState = AppState {
  appDbPool :: TolaPool
, appLogger :: MVar.MVar ()
}

instance HasDbPool AppState where
  dbPool = appDbPool

newtype RealWebAppT m a = RealWebAppT { unRealWebAppT ::  ReaderT AppState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState, MonadTrans, MonadThrow, MonadCatch)

instance MonadBase IO (RealWebAppT IO) where
  liftBase = liftBaseDefault

instance MonadBaseControl IO (RealWebAppT IO) where
  type StM (RealWebAppT IO) a = a
  liftBaseWith f = RealWebAppT $ liftBaseWith $ \q -> f (q . unRealWebAppT)
  restoreM =  RealWebAppT . restoreM


instance MonadIO m => MonadLogger (RealWebAppT m) where
  writeLog str = do
    AppState _ appLogger' <- ask
    liftIO $ do
      MVar.takeMVar appLogger'
      Char8.putStrLn str
      MVar.putMVar appLogger' ()



instance MonadPixelsDatabase (RealWebAppT IO) where
  getAllPixelsToBeFired = runDb' getAllPixelsToBeFired'
  insertAPixel = runDb' . insertAPixel'




-- main :: RealWebAppT IO [(Text, Text)]
-- main' app connStr = runNoLoggingT $ withPostgresqlPool connStr 10 $ liftSqlPersistMPool app
-- main = Char8.pack <$> Env.getEnv "db" >>= main' app
-- main' app connStr = withDbPool' connStr app

-- startApp dkb app = withDbPool db (\pool -> runReaderT (unRealWebAppT app) (AppState pool))

-- {-
main :: IO ()
main = do
  db <- Char8.pack <$> Env.getEnv "db"
  appLogger' <- MVar.newMVar ()
  withDbPool db $ \ pool -> runReaderT (unRealWebAppT app) (AppState pool appLogger')



app :: RealWebAppT IO ()
app = do
  pixels <- getAllPixelsToBeFired
  pixelResults <- mapM mapPixels pixels
  -- pixelResults <- firePixels pixelUrls
  liftIO $ print pixelResults

  liftIO $ Concurrent.threadDelay (10*1000*1000)
  app

  where
  mapPixels (_campaignName, chargeRequestKey, urlTemplate, msisdn, qs) = do
    let url = PT.formatPixelUrl (PT.PixelInput (Just msisdn) qs) urlTemplate
    (status, body) <-
      catchAll
        (getUrl url)
        (\e -> do
          let errString = Encoding.encodeUtf8 $ T.pack $ show e
          return (Status.Status 0 errString, errString)
        )
    let statusCode = Status.statusCode status
    let body' = case (Encoding.decodeUtf8' body) of
                      Left _  -> Encoding.decodeUtf8 $ Base64.encode body
                      Right t -> t
    insertAPixel (chargeRequestKey, url, statusCode == 200, statusCode, body')


getUrl ::
     (MonadIO m, MonadLogger m, MonadCatch m)
  => Url
  -> m (Status.Status, Char8.ByteString)
getUrl url = do
  manager <- liftIO $ C.newManager C.tlsManagerSettings
  r       <- liftIO $ C.parseUrlThrow (T.unpack $ unUrl url)
  let request = r
        { C.secure         = True
        , C.method         = "GET"
        }
  writeLog $ Char8.pack $ show request
  response <- liftIO $ C.httpLbs request manager
  let strictBody = toStrict $ C.responseBody response
  let status = C.responseStatus response
  let headers = C.responseHeaders response
  writeLog strictBody
  writeLog (Char8.pack $ show status)
  writeLog (Char8.pack $ show headers)
  return (status, strictBody)
--}
