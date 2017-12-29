{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Tola.LodgementRequest (
    LodgementRequest, PLodgementRequest (..)
) where

import qualified Data.Aeson   as A
import           Tola.Common
import           Tola.Imports


data PLodgementRequest a t = LodgementRequest {
    accountname       :: Text
  , amount            :: a
  , amounttype        :: Text
  , channel           :: Text
  , currency          :: Text
  , customerreference :: Text
  , date              :: t
  , mac               :: Text
  , msisdn            :: Msisdn
  , operatorreference :: Text
  , reference         :: Text
  , sourcereference   :: Text
  , target            :: Text
  , requestType       :: Text
} deriving (Show, Read, Generic)

type LodgementRequest = PLodgementRequest Amount UTCTime

instance (A.ToJSON a, A.ToJSON t) => A.ToJSON (PLodgementRequest a t) where
  toEncoding = toTolaEncoding

instance (A.FromJSON a, A.FromJSON t) => A.FromJSON (PLodgementRequest a t) where
  parseJSON = parseTolaJSON

