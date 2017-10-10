{-# LANGUAGE TemplateHaskell, DeriveGeneric , DeriveAnyClass           #-}

module SeverityLevel (
  SeverityLevel (..)
) where

import           Database.Persist.TH
import qualified Data.Aeson as A
import GHC.Generics

data SeverityLevel = Low | Medium | Critical | High
  deriving (Show, Read, Enum, Bounded, Generic, A.ToJSON, A.FromJSON)
derivePersistFieldSqlTypeOther "SeverityLevel" "severityLevel"
