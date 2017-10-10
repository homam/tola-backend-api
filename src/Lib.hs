{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances               #-}

module Lib
    ( main
    ) where

import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import qualified Data.Aeson as A
import Data.Aeson (( .= ))
import Data.Aeson.Encode.Pretty (encodePretty)
import           Database.Persist.Postgresql.Json
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as Char8
import SeverityLevel
import PersonUserName
import Dictionary
import Control.Monad.IO.Class (liftIO, MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Map as M
import qualified Data.Time as Time
import Helpers ()

import           Control.Monad.Reader       (asks)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Reader.Class (MonadReader)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json sql=People
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  username PersonUserName
  UniqueUserName username
  severity SeverityLevel default="'Low'::severityLevel"
  name String
  age Int Maybe
  info Dictionary
  extra Json
  deriving Show

BlogPost json
  Id
  title String
  authorId PersonId
  deriving Show

Tweet
  tweetId Int
  statusText Text sqltype=varchar(170)
  Primary tweetId
  UniqueTweetId tweetId
  deriving Show
TweetUrl
  tweetId TweetId
  tweetUrl Text sqltype=varchar(255)
  finalUrl Text Maybe sqltype=varchar(255)
  UniqueTweetIdTweetUrl tweetId tweetUrl
  deriving Show
|]

connStr = "host=localhost dbname=test"

defaultUTCTime :: Time.UTCTime
defaultUTCTime = Time.UTCTime (Time.fromGregorian 2017 1 1) 0

addPerson :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistStoreWrite backend) => PersonUserName -> SeverityLevel -> String -> Maybe Int -> Dictionary -> Json -> ReaderT backend m (Key Person)
addPerson userName slevel name age info extra = insert $ Person userName slevel name age info extra

getPerson :: (PersistStoreRead backend, MonadIO m, PersistRecordBackend record backend) => Key record -> ReaderT backend m (Maybe record)
getPerson = get

data Config = Config { getPool :: ConnectionPool }

newtype ConfigM a = ConfigM
    { runConfigM :: ReaderT Config IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadTrans t, MonadIO (t ConfigM)) => SqlPersistT IO a -> t ConfigM a
runDb query = do
    pool <- lift $ asks getPool
    liftIO (runSqlPool query pool)


doDbStuff :: ReaderT SqlBackend IO ()
doDbStuff = do
  let pjson  = Json  $ A.object [
                        "key" .= ("value":: Text)
                      , "key2" .= A.object [ "nested" .= (28844 :: Int) ]
                      ]

  addPerson (PersonUserName "john") Low "John Doe" (Just 35) (Dictionary $ M.fromList [("a", "b")] ) pjson

  return ()

main :: IO ()
main = do
  -- pool <- runStderrLoggingT $ createPostgresqlPool connStr 10
  -- liftIO (runDb doDbStuff)
  -- print "done"

  runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        let pjson  = Json  $ A.object [
                              "key" .= ("value":: Text)
                            , "key2" .= A.object [ "nested" .= (28844 :: Int) ]
                            ]

        johnId <- addPerson (PersonUserName "john") Low "John Doe" (Just 35) (Dictionary $ M.fromList [("a", "b")] ) pjson
        janeId <- addPerson (PersonUserName "jane") Medium "Jane Doe" Nothing (Dictionary $ M.fromList [("a", "hello")] ) pjson

        insert $ BlogPost "My fr1st p0st" johnId
        insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])
        liftIO $ print $ map entityVal (oneJohnPost :: [Entity BlogPost])

        liftIO $ print $ fromSqlKey johnId

        john <- getPerson johnId
        -- liftIO $ print (john :: Maybe Person)
        liftIO $ Char8.putStrLn $ encodePretty $ A.toJSON john
        delete janeId
        -- delete janeId
        -- deleteWhere [BlogPostAuthorId ==. johnId]

        let tweet = Tweet {tweetTweetId = 1, tweetStatusText = "Hello!"}
        tweetId <- insert tweet
        let url = TweetUrl {tweetUrlTweetId = tweetId, tweetUrlTweetUrl = "http://google.com", tweetUrlFinalUrl = Just "http://example.com"}
        insert_ url
        -- tweetId' <- insert tweet
        liftIO $ print tweetId
    --}
