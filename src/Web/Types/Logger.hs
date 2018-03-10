module Web.Types.Logger (Logger, HasLogger (..)) where

import qualified Data.ByteString.Char8 as Char8

type Logger = Char8.ByteString -> IO ()

class HasLogger t where
  writeLog :: t -> Logger
