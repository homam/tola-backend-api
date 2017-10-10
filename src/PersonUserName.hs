{-# LANGUAGE DeriveGeneric #-}

module PersonUserName (
  PersonUserName (..)
) where

import           Database.Persist
import Database.Persist.Sql
import           Database.Persist.TH
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics

newtype PersonUserName = PersonUserName Text
  deriving (Eq, Ord, Show, Read, Generic)

instance A.ToJSON PersonUserName
instance A.FromJSON PersonUserName

instance PersistField PersonUserName where
  toPersistValue (PersonUserName t) = toPersistValue t
  fromPersistValue t = PersonUserName <$> fromPersistValue t

instance PersistFieldSql PersonUserName where
  sqlType _ = SqlString
