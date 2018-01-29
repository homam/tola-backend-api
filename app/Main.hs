module Main where

import           Control.Monad      (join)
import qualified Lib
import qualified System.Environment as Env

main :: IO ()
main = join $
  Lib.main <$>
    fmap read (Env.getEnv "port") <*>
    Env.getEnv "jewel_connection_string" <*>
    Env.getEnv "db" <*>
    fmap Lib.mkSecret' (Env.getEnv "tola_secret")
