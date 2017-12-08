{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Web.JewlModel (
  msisdnStatus, isMSISDNActive, runJewlDb
) where

import           Control.Monad.IO.Class               (MonadIO (..), liftIO)
import           Control.Monad.Reader                 (asks)
import           Control.Monad.Reader.Class           (MonadReader)
import           Control.Monad.Trans.Class            (MonadTrans, lift)
import           Data.Aeson                           (ToJSON)
import qualified Data.List                            as L
import qualified Database.PostgreSQL.Simple           as PS
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.SqlQQ     (sql)
import           GHC.Generics                         (Generic)
import           Web.AppState


data MSISDNExists a = MSISDNExists { msisdn :: String, optout :: a } deriving (Show, Generic)
instance FromField a => PS.FromRow (MSISDNExists a)
instance ToJSON a => ToJSON (MSISDNExists a)

isMSISDNActive :: MSISDNExists Bool -> Bool
isMSISDNActive = not . optout

msisdnStatus :: String -> String -> PS.Connection -> IO (Maybe (MSISDNExists Bool))
msisdnStatus country msisdn' conn = safeHead <$> map normalize <$> PS.query
  conn
   [sql|
    SELECT TOP 1 msisdn, optout from user_subscriptions WHERE timestamp > ? and country_code = ? and msisdn = ?
    |]
  ("2017-11-01" :: String, country, fixMSISDN msisdn')
  where
    normalize :: MSISDNExists Int -> MSISDNExists Bool
    normalize m = m { optout = optout m > 0 }

    fixMSISDN :: String -> String
    fixMSISDN m = case country of
      "GR" -> if "30" `L.isPrefixOf` m then drop 2 m else m
      _    -> m

    safeHead :: [a] -> Maybe a
    safeHead []    = Nothing
    safeHead (x:_) = Just x

runJewlDb :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => (PS.Connection -> IO b) -> t m b
runJewlDb query = do
  run <- lift $ asks runJewl
  liftIO (run query)
