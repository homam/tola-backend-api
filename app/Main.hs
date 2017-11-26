module Main where

import qualified Lib
import qualified System.Environment as Env
import Control.Monad (join)

main :: IO ()
main = join $ Lib.main <$> fmap read (Env.getEnv "port") <*> Env.getEnv "db"
