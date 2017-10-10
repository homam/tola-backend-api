{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Helpers ()
where

import qualified Data.Aeson                       as A
import           Data.Aeson.Types
import qualified Data.ByteString.Char8            as Char8
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Lazy.Char8       as Char8L
import qualified Data.Map                         as M
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (unpack)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as Encoding
import           Data.Typeable.Internal           (Typeable)
import           Database.Persist
import           Database.Persist.Postgresql.Json
import           Database.Persist.Sql


instance {-# OVERLAPPABLE #-} (A.FromJSON a, A.ToJSON a) => PersistField a where
  toPersistValue t = toPersistValue $ Encoding.decodeUtf8 $ Char8L.toStrict $ A.encode $ A.toJSON t
  fromPersistValue (PersistByteString t) = fromMaybe (Left "Error") (Right <$> A.decode (Char8L.fromStrict t))
  fromPersistValue wrongValue = Left $ T.pack $ "Received " ++ show wrongValue ++ " when a value of type PersistText was expected."

instance {-# OVERLAPPABLE #-} (A.ToJSON a, PersistField a) => PersistFieldSql a where
  sqlType _ = SqlOther "JSON"
