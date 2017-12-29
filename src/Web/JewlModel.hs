{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Web.JewlModel (
  msisdnStatus, runJewlDb, FinalResult (..)
) where

import           Control.Monad.IO.Class               (MonadIO (..), liftIO)
import           Control.Monad.Reader                 (asks)
import           Control.Monad.Reader.Class           (MonadReader)
import           Control.Monad.Trans.Class            (MonadTrans, lift)
import qualified Data.Aeson                           as A
import qualified Data.Aeson.Types                     as AT
import           Data.Char                            (toUpper)
import qualified Data.HashMap.Strict                  as M
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple           as PS
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.SqlQQ     (sql)
import           GHC.Generics                         (Generic)
import           Web.AppState
import           Web.Localization                     (toLocalMSISDN)


data MSISDNExists a = MSISDNExists { msisdn :: String, optout :: a } deriving (Show, Generic)
instance FromField a => PS.FromRow (MSISDNExists a)
instance A.ToJSON a => A.ToJSON (MSISDNExists a)

data FinalResult = FinalResult { finalUrl :: Maybe T.Text, isActive :: Bool } deriving (Show, Generic)
instance A.ToJSON FinalResult where
  toJSON (FinalResult u s) = if s
    then AT.Object (M.insert "finalUrl" (A.toJSON u) isActiveJSON)
    else AT.Object isActiveJSON
    where
      isActiveJSON = M.insert "isActive" (A.toJSON s) M.empty

instance A.FromJSON FinalResult


msisdnStatus :: String -> String -> PS.Connection -> IO FinalResult
msisdnStatus country msisdn' conn = toFinalResult . map normalize <$> PS.query
  conn
   [sql|
    SELECT TOP 1 msisdn, optout from user_subscriptions WHERE timestamp > ? and country_code = ? and msisdn = ?
    |]
  ("2017-11-01" :: String, normalizeCountry country, toLocalMSISDN country msisdn')
  where
    normalize :: MSISDNExists Int -> MSISDNExists Bool
    normalize m = m { optout = optout m > 0 }

    normalizeCountry = map toUpper

    toFinalResult :: [MSISDNExists Bool] -> FinalResult
    toFinalResult [] = FinalResult Nothing False
    toFinalResult (MSISDNExists _ True:_)  = FinalResult Nothing False
    toFinalResult (MSISDNExists _ False:_) = FinalResult (Just "http://gr.mobiworldbiz.com/?uid=fdf098fcc6&uip=2.84.0.0") True


runJewlDb :: (MonadIO (t m), MonadReader AppState m, MonadTrans t) => (PS.Connection -> IO b) -> t m b
runJewlDb query = do
  run <- lift $ asks runJewl
  liftIO (run query)
