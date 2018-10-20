module Main where

import System.Environment (getArgs)
import qualified Lib
import qualified Pixels.Program

isPixels = any (=="--pixels")

main :: IO ()
main = do 
  args <- getArgs
  if isPixels args
    then Pixels.Program.main -- rockman pixels
    else Lib.main -- main web app
