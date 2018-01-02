module Web.Security.Base62 (
  encode, decode
) where

import           Data.List
import           Data.Maybe

characters :: [Char]
characters = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

base :: Int
base = length characters

encode :: Int -> [Char]
encode n = reverse
  [ characters !! (x `mod` base)
  | x <- takeWhile (>0) (iterate (\x -> x `div` base) n)
  ]

decode :: [Char] -> Int
decode code =
  foldl (\r c -> (base * r) + fromJust (elemIndex c characters)) 0 code
