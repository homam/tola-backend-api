{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Localization (
    toLocalMSISDN
  , encrypt, encrypt'
  , decrypt, decrypt', decrypt''
  , getTime
  , toHex
  , toNoEqB64, fromNoEqB64
  , encryptToNoEqB64, encryptToNoEqB64'
  , decryptFromNoEqB64, decryptFromNoEqB64''
) where

import qualified Codec.Crypto.AES           as AES
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.CaseInsensitive       as CI
import qualified Data.List                  as L
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Time.Clock.POSIX      as POSIX
import           Numeric                    (readHex, showHex)

algorithm = AES.crypt' AES.CFB "abcdefghijl1ertg" "abcdefghijl1ertg"


main = do
  encrypted <- B64.encode . algorithm AES.Encrypt . C8.pack . toHex <$> getTime 1000000
  let decrypted = readHex . C8.unpack . algorithm AES.Decrypt  <$> B64.decode encrypted
  print encrypted
  print decrypted

getTime :: (Integral b, RealFrac a) => a -> IO b
getTime precision =
  round . (* precision) . fromRational . toRational <$> POSIX.getPOSIXTime

toHex :: (Show a, Integral a) => a -> String
toHex n = showHex n ""

encrypt' :: String -> String
encrypt' = C8.unpack . encrypt . C8.pack

encrypt :: C8.ByteString -> C8.ByteString
encrypt = B64.encode . algorithm AES.Encrypt

encryptToNoEqB64 :: Char -> C8.ByteString -> C8.ByteString
encryptToNoEqB64 c s =
  let pad = padStringForNoEqB64 C8.length C8.replicate c s
  in encrypt pad

-- | Encrypts a String in Base64 format but ensures the encoding has no equal sign (=) suffix.
-- Example:
-- >  decryptFromNoEqB64'' ' ' $ encryptToNoEqB64' ' ' "some text"
encryptToNoEqB64' :: Char -> String -> String
encryptToNoEqB64' c = C8.unpack . encryptToNoEqB64 c . C8.pack

decryptFromNoEqB64 :: Char -> C8.ByteString -> Either String C8.ByteString
decryptFromNoEqB64 c = fmap (C8.reverse . C8.dropWhile (== c) . C8.reverse) . decrypt

decryptFromNoEqB64'' :: Char -> String -> Either String String
decryptFromNoEqB64'' c = fmap C8.unpack . decryptFromNoEqB64 c . C8.pack

decrypt :: C8.ByteString -> Either String C8.ByteString
decrypt = fmap (algorithm AES.Decrypt) . B64.decode

decrypt' :: C8.ByteString -> Either String String
decrypt' = fmap C8.unpack . decrypt

decrypt'' :: String -> Either String String
decrypt'' = fmap C8.unpack . decrypt . C8.pack


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
toNoEqB64 c = C8.unpack . B64.encode . C8.pack . padStringForNoEqB64 length replicate c

-- | The reverse of 'toNoEqualB64'.
fromNoEqB64 :: Char -> String -> Either String String
fromNoEqB64 c t = (L.dropWhileEnd (== c) . C8.unpack) <$> (B64.decode . C8.pack $ t)

---

-- | Internal utility
padStringForNoEqB64 :: (Monoid m, Integral a)
  => (m -> a) -> (a -> t -> m) -> t -> m -> m
padStringForNoEqB64 len repl c t =
  let m   = len t `mod` 3
      pad = repl ((3 - m) `mod` 3) c
  in  t <> pad

---

toLocalMSISDN :: String -> String -> String
toLocalMSISDN country m = case CI.mk country of
  "gr" -> if "30" `L.isPrefixOf` m then drop 2 m else m
  _    -> m

