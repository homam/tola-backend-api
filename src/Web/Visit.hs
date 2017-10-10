{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Visit(
  visitWeb
)
where

import           Control.Monad       (join)
import           Data.Text           (Text)
import qualified Data.Text.Lazy      as TL
import           Web.Model
import           Web.WebM

visitWeb :: WebMApp ()
visitWeb =
  getAndHead "/visit/:campaignId/:gateway/:landingpage" $
    join $ visitAction <$> param "campaignId" <*> param "gateway" <*> param "landingpage"

visitAction :: Text -> Text -> Text -> WebMAction ()
visitAction campaignId gateway landingPage = do
  --TODO: db connection
  vid <- fromIntegral . fromSqlKey <$> addVisit campaignId landingPage
  addScotchHeader "VisitId" (TL.pack $ show vid)
  text "hello there!"
