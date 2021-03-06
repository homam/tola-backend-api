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
import           Database.Persist
import           Database.Persist.Postgresql
import qualified Data.Aeson as A
import           Data.Aeson                     ( (.=) )
import qualified Data.Map as Map
import qualified Data.CaseInsensitive          as CI
import           Data.Time.Clock.POSIX          as POSIX
import qualified Network.URI.Encode            as URIEncoding
import           Data.Maybe                     ( fromMaybe )
import Text.Read (readMaybe)
import Data.Monoid ((<>))


data AppState = AppState {
  appDbPool :: TolaPool
, appLogger :: MVar.MVar ()
}

instance HasDbPool AppState where
  dbPool = appDbPool

newtype RealRockmanPixelsApp m a = RealRockmanPixelsApp { unRealRockmanPixelsApp ::  ReaderT AppState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState, MonadTrans, MonadThrow, MonadCatch)

instance MonadBase IO (RealRockmanPixelsApp IO) where
  liftBase = liftBaseDefault

instance MonadBaseControl IO (RealRockmanPixelsApp IO) where
  type StM (RealRockmanPixelsApp IO) a = a
  liftBaseWith f = RealRockmanPixelsApp $ liftBaseWith $ \q -> f (q . unRealRockmanPixelsApp)
  restoreM =  RealRockmanPixelsApp . restoreM


instance MonadIO m => MonadLogger (RealRockmanPixelsApp m) where
  writeLog str = do
    AppState _ appLogger' <- ask
    liftIO $ do
      MVar.takeMVar appLogger'
      Char8.putStrLn str
      MVar.putMVar appLogger' ()

instance MonadRockmanPixelDatabase (RealRockmanPixelsApp IO) where
  getAllRockmanPixelsToBeFired = runDb' getAllRockmanPixelsToBeFired'
  insertRockmanPixel = runDb' . insertRockmanPixel'


main :: IO ()
main = do
  db <- Char8.pack <$> Env.getEnv "db"
  Char8.putStrLn $ "connected to " <> db
  appLogger' <- MVar.newMVar ()
  withDbPool db $ \ pool -> runReaderT (unRealRockmanPixelsApp app) (AppState pool appLogger')


app :: RealRockmanPixelsApp IO ()
app = do
  pixels <- take 10 <$> getAllRockmanPixelsToBeFired
  let totalPixels = length pixels
  writeLog $ Char8.pack $ "Total Pixels to be Fired: " ++ show totalPixels
  mapM_ (\(p, i) -> do 
      writeLog $ Char8.pack $ "Firing Pixel " ++ show i ++ " / " ++ show totalPixels
      fireAndRecordRockmanPixel p
      writeLog $ Char8.pack $ "Pixel Fired: " ++ show i ++ " / " ++ show totalPixels
      liftIO $ Concurrent.threadDelay (400*1000)
    ) (pixels `zip` [(1::Int) ..])
  writeLog "All Pixels Fired!"
  liftIO $ Concurrent.threadDelay (30*1000*1000)
  app
  where

  fireAndRecordRockmanPixel args@(_campaignName, chargeRequestKey, _affiliateId, _saleTime, _msisdn, _qs) = do
    let postData = makeRockmanJSON args
    (status, responseBody) <- fireSaleToRockmnan postData
    key <- recordRockmanPixel chargeRequestKey postData status
    return (key, responseBody)

  recordRockmanPixel chargeRequestKey postData status@(Status.Status statusCode _) =
    insertRockmanPixel (chargeRequestKey, postData , Status.statusIsSuccessful status, statusCode)

  fireSaleToRockmnan postData = do
    let url = mkUrl "http://rockman-api.sam-media.com:8002/api/v1/event/sale" 
    (status, body) <-
      catchAll
        (postUrl url postData)
        (\e -> do
          let errString = Encoding.encodeUtf8 $ T.pack $ show e
          return (Status.Status 0 errString, errString)
        )
    let body' = case (Encoding.decodeUtf8' body) of
                      Left _  -> Encoding.decodeUtf8 $ Base64.encode body
                      Right t -> t
    liftIO $ Concurrent.threadDelay (200*1000)
    --TODO: add to DB
    return (status, body')

  makeRockmanJSON (_campaignName, chargeRequestKey, affiliateId, saleTime, msisdn, qs) = 
    let page = fromMaybe "iphone-xs" (Map.lookup "page" qs)
        offer = fromMaybe (1 :: Int) (readMaybe =<< Map.lookup "offer_id" qs)
    in A.object  [
        "rockman_id" .= lookup' "rockman_id" qs
      , "timestamp" .=  ((fromIntegral .  round $ POSIX.utcTimeToPOSIXSeconds saleTime) :: Int)
      , "affiliate_id" .= unAffiliateId affiliateId
      , "platform" .= ("os" :: T.Text)
      , "product_type" .= ("MCB" :: T.Text)
      , "country_code" .= ("KE" :: T.Text)
      , "gateway" .= ("KE_TOLA" :: T.Text)
      , "operator_code" .= ("KE_SAFARICOM" :: T.Text)
      , "traffic_type" .= ("REG" :: T.Text)
      , "query_string" .= ((T.pack $ toQs $  map (\(k, v) -> CI.original k ++ "=" ++ URIEncoding.encode v) $  Map.toList qs) :: T.Text)
      , "msisdn" .= unMsisdn msisdn

      , "handle_name" .= page 
      , "handle_id" .= page
      , "ad_name" .= page
      , "ad_id" .= page
      , "offer_id" .= offer

      , "service_identifier1" .= ("bidiotv" :: T.Text)
      , "scenario_id" .= ("tola" :: T.Text)
      , "scenario_name" .= ("tola" :: T.Text)
      , "device_class" .= ("smart" :: T.Text)
    ]
  
  lookup' :: (CI.FoldCase s, Ord s) => s -> Map.Map (CI.CI s) a -> Maybe a
  lookup' k = Map.lookup (CI.mk k)

  toQs [] = ""
  toQs (x:xs) = x ++ toQs' xs
  toQs' [] = ""
  toQs' (x:xs) = "&" ++ x ++ toQs' xs


pairsToJSON :: A.ToJSON v => [(T.Text, v)] -> A.Value
pairsToJSON =  A.object . map (\(k, v) -> k .= v)

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

postUrl
  :: (MonadIO m, MonadLogger m, MonadCatch m, A.ToJSON a)
  => Url
  -> a
  -> m (Status.Status, Char8.ByteString)
postUrl url d = do
  let body = A.encode d
  writeLog (toStrict body)
  manager <- liftIO $ C.newManager C.tlsManagerSettings
  r       <- liftIO $ C.parseUrlThrow (T.unpack $ unUrl url)
  let request = r { C.secure = False, C.method = "POST", C.requestHeaders = [(CI.mk "Content-Type", "application/json")], C.requestBody = C.RequestBodyLBS body }
  writeLog $ Char8.pack $ show request
  response <- liftIO $ C.httpLbs request manager
  let strictBody = toStrict $ C.responseBody response
  let status     = C.responseStatus response
  let headers    = C.responseHeaders response
  writeLog strictBody
  writeLog (Char8.pack $ show status)
  writeLog (Char8.pack $ show headers)
  return (status, strictBody)
