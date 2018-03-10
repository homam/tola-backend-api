{-# LANGUAGE OverloadedStrings #-}

module Web.Crypto (
    encrypt, encrypt'
  , decrypt, decrypt', decrypt''
  , encryptId, decryptId
  , getTime
  , toHex, fromHex
  , split
  , toNoEqB64, fromNoEqB64
  , encryptToNoEqB64, encryptToNoEqB64'
  , decryptFromNoEqB64, decryptFromNoEqB64''
  , uniqueTimestamp, M.newMVar
  , uniqueTimestampEncrypted, decryptUniqueTimestamp
  , idToHex, fromHexId
) where

import qualified Codec.Crypto.AES           as AES
import           Control.Arrow              ((***))
import qualified Control.Concurrent.MVar    as M
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.List                  as L
import           Data.Monoid                ((<>))
import           Data.Time                  (UTCTime)
import qualified Data.Time.Clock.POSIX      as POSIX
import           Numeric                    (readHex, showHex)
import           Text.Read                  (readEither)


-- | Creates a unique timestamp generator, useful for generating unique integral Ids.
-- > ut <- uniqueTimestamp 100000 =<< M.newMVar 0
-- > ut >>= print
--
uniqueTimestamp :: (RealFrac p, Integral a) => p -> M.MVar a -> IO a
uniqueTimestamp precision mv = do
  t <- getTime precision
  v <- M.takeMVar mv
  let t' = if t <= v then v + 1 else t
  M.putMVar mv t'
  return t'

algorithm :: AES.Direction -> C8.ByteString -> C8.ByteString
algorithm = AES.crypt' AES.CFB "abcdefghijl1ertg" "abcdefghijl1ertg"

{-
main = do
  encrypted <- B64.encode . algorithm AES.Encrypt . C8.pack . toHex <$> getTime 1000000
  let decrypted = readHex . C8.unpack . algorithm AES.Decrypt  <$> B64.decode encrypted
  print encrypted
  print decrypted
-}

getTime :: (Integral b, RealFrac a) => a -> IO b
getTime precision =
  round . (* precision) . fromRational . toRational <$> POSIX.getPOSIXTime

toHex :: (Show a, Integral a) => a -> String
toHex n = showHex n ""

fromHex :: String -> Either String Integer
fromHex = go . readHex
 where
  go []    = Left "Cannot parse hex"
  go (x:_) = Right $ fst x



encrypt' :: String -> String
encrypt' = C8.unpack . encrypt . C8.pack

encrypt :: C8.ByteString -> C8.ByteString
encrypt = B64.encode . algorithm AES.Encrypt

encryptToNoEqB64 :: Char -> C8.ByteString -> C8.ByteString
encryptToNoEqB64 c s =
  let pad = padStringForNoEqB64 C8.length C8.replicate c s in encrypt pad

-- | Encrypts a String in Base64 format but ensures the encoding has no equal sign (=) suffix.
-- Example:
-- >  decryptFromNoEqB64'' ' ' $ encryptToNoEqB64' ' ' "some text"
encryptToNoEqB64' :: Char -> String -> String
encryptToNoEqB64' c = C8.unpack . encryptToNoEqB64 c . C8.pack

decryptFromNoEqB64 :: Char -> C8.ByteString -> Either String C8.ByteString
decryptFromNoEqB64 c = fmap (C8.dropWhile (== c)) . decrypt

decryptFromNoEqB64'' :: Char -> String -> Either String String
decryptFromNoEqB64'' c = fmap C8.unpack . decryptFromNoEqB64 c . C8.pack

decrypt :: C8.ByteString -> Either String C8.ByteString
decrypt = fmap (algorithm AES.Decrypt) . B64.decode

decrypt' :: C8.ByteString -> Either String String
decrypt' = fmap C8.unpack . decrypt

decrypt'' :: String -> Either String String
decrypt'' = fmap C8.unpack . decrypt . C8.pack

split :: Eq t => t -> [t] -> ([t], [t])
split a = go ([], [])
 where
  go t      []     = t
  go (l, r) (x:xs) = if a == x then (reverse l, xs) else go (x : l, r) xs


-- | Encodes a String in Base64 format but ensures the encoding has no equal sign (=) suffix.
-- Use 'toNoEqualB64' and 'fromNoEqB64' for short strings.
--
-- > test = do
-- >   t <- getTime 1000
-- >   let b64 = toNoEqualB64 ' ' $ show t ++ "."  ++ show 62
-- >   print b64
-- >   t' = fromNoEqB64 ' ' b64
-- >   print t'
-- >   print $ t == t'
--
--
toNoEqB64 :: Char -> String -> String
toNoEqB64 c =
  C8.unpack . B64.encode . C8.pack . padStringForNoEqB64 length replicate c

-- | The reverse of 'toNoEqualB64'.
fromNoEqB64 :: Char -> String -> Either String String
fromNoEqB64 c t =
  L.dropWhile (== c) . C8.unpack <$> (B64.decode . C8.pack $ t)
---

-- | Internal utility
padStringForNoEqB64
  :: (Monoid m, Integral a) => (m -> a) -> (a -> t -> m) -> t -> m -> m
padStringForNoEqB64 len repl c t =
  let m   = len t `mod` 3
      pad = repl ((3 - m) `mod` 3) c
  in  pad <> t

---

-- | Encrypt a showable object by padding it with '.', timestamp and white space.
-- >>> (decryptId 1000 <$> encryptId 1000 287363) :: IO (Either String (UTCTime, Integer))
--
encryptId :: Show a => Rational -> a -> IO String
encryptId prec i =
  encryptToNoEqB64' ' ' . (<> "." <> show i) . (show :: Integer -> String) <$> getTime prec

-- | The reverse of 'encryptId'
decryptId :: Read a => Rational -> String -> Either String (UTCTime, a)
decryptId precision = fmap ((toTime *** read) . split '.')
  . decryptFromNoEqB64'' ' '
 where
  toTime =
    POSIX.posixSecondsToUTCTime
      . fromRational
      . (/ precision)
      . fromIntegral
      . (read :: String -> Integer)

---

idToHex :: (Integral a, Show a) => Rational -> a -> IO String
idToHex prec i = (<> "." <> toHex i) . (toHex :: Integer -> String) <$> getTime prec

fromHexId :: Rational -> String -> Either String (UTCTime, Integer)
fromHexId precision =
  uncurry (\a b -> (,) <$> a <*> b) . (toTime *** fromHex) . split '.'
 where
  toTime n =
    POSIX.posixSecondsToUTCTime
      .   fromRational
      .   (/ precision)
      .   fromIntegral
      <$> fromHex n



-- | Creates a new unique timestamp generator that outputs encrypted values.
-- Example of usage:
-- >>> print . fmap decryptUniqueTimestamp =<< uniqueTimestampEncrypted 100000 =<< M.newMVar 0
--
-- uniqueTimestampEncrypted :: (Integral a, RealFrac p, Show a) => p -> M.MVar a -> IO String
uniqueTimestampEncrypted
  :: (Integral a, RealFrac p, Show a) => p -> M.MVar a -> IO (a, String)
uniqueTimestampEncrypted precision =
  fmap (\s -> (s, encryptToNoEqB64' ' ' . show $ s)) . uniqueTimestamp precision

-- | The reverse of 'uniqueTimestampEncrypted'
decryptUniqueTimestamp :: String -> Either String Int
decryptUniqueTimestamp s = readEither =<< decryptFromNoEqB64'' ' ' s
