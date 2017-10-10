{-# LANGUAGE DeriveGeneric     #-}

module Dictionary (
  Dictionary (..)
) where

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as Encoding
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics
import           Helpers                    ()

newtype Dictionary = Dictionary (M.Map T.Text T.Text)
  deriving (Eq, Ord, Show, Read, Generic)

instance A.ToJSON Dictionary
instance A.FromJSON Dictionary
