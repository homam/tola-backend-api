{-# LANGUAGE OverloadedStrings #-}

module Sam.Robot
(
    submitMSISDN, submitPIN, runSubmission, C.HttpException (..), SubmissionError (..)
  , main -- for demonstration purpose only
) where

import           Control.Exception          (try)
import           Control.Monad              (join)
import           Control.Monad.Except       (liftIO)
import qualified Control.Monad.Trans.Except as X
import qualified Data.ByteString            as BS
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import qualified Data.Text.IO               as T
import qualified Network.HTTP.Client        as C
import qualified Network.HTTP.Types.URI     as U
import qualified Network.URI                as U

type Submission e b = X.ExceptT (SubmissionError e b) IO
data SubmissionError e b = NetworkError e | ValidationError b

runSubmission :: Submission e b a -> IO (Either (SubmissionError e b) a)
runSubmission = X.runExceptT

callSAM :: String -> Submission C.HttpException b (U.URI, BS.ByteString)
callSAM url = do
  x <- liftIO $ try $ join $ C.withResponseHistory
      <$> C.parseRequest url
      <*> C.newManager C.defaultManagerSettings
      <*> return (\hr -> (BS.concat <$> C.brConsume (C.responseBody $ C.hrFinalResponse hr) ) >>= \b -> return (C.getUri $ C.hrFinalRequest hr, b))
  case x of
    Left e  -> X.throwE (NetworkError e)
    Right r -> return r

submitMSISDN' :: String -> String -> String -> Int -> String -> Submission C.HttpException b (U.URI, BS.ByteString)
submitMSISDN' domain handle country offer msisdn = callSAM $ "http://" <> domain <> "/" <> country <> "/" <> handle <> "?country=" <> country <> "&handle=" <> handle <> "&offer=" <> show offer <> "&device=smart&msisdnSubmitted=Y&incentivizedCheckbox=Y&legalCheckbox=N&legalCheckbox=Y&op_confirmCheckbox=N&msisdn%5B0%5D=" <> msisdn

validateSubmission :: Bool -> T.Text -> (b, BS.ByteString) -> Submission C.HttpException BS.ByteString b
validateSubmission includes search (url, bs) = if (`op` T.empty) $ snd $ T.breakOn search (E.decodeUtf8 bs)
    then X.throwE (ValidationError bs)
    else return url
    where
      op = if includes then (==) else (/=)

validateMSISDNSubmission :: (b, BS.ByteString) -> Submission C.HttpException BS.ByteString b
validateMSISDNSubmission = validateSubmission True "numeric-field pin pin-input"

validatePINSubmission :: (b, BS.ByteString) -> Submission C.HttpException BS.ByteString b
validatePINSubmission = validateSubmission False "numeric-field pin pin-input"

submitPIN' :: BS.ByteString -> U.URI -> Submission C.HttpException b (U.URI, BS.ByteString)
submitPIN' pin url = callSAM $ (U.uriToString id $ makePINUrl pin url) ""

makePINUrl :: BS.ByteString -> U.URI -> U.URI
makePINUrl pin url = U.URI (U.uriScheme url) (U.uriAuthority url) (U.uriPath url) (queryString pin url) ""
  where
    queryString pin url = T.unpack $ E.decodeUtf8 $ U.renderQuery True $ [("pinSubmitted", Just "Y"), ("pin", Just pin)] ++ keepRelevantParams url
    keepRelevantParams url = filter ((`elem` [
      "country",
      "handle",
      "offer",
      "device",
      "msisdn[0]",
      "incentivizedCheckbox",
      "legalCheckbox",
      "identified",
      "operator",
      "msisdnSubmitted",
      "rid",
      "_extracted"
      ]) . fst) $ U.parseQuery $ E.encodeUtf8 $ T.pack $ U.uriQuery url

submitMSISDN d h c o = (validateMSISDNSubmission =<<) . submitMSISDN' d h c o
submitPIN p = (validatePINSubmission =<<) . submitPIN' p

-- 6949041021
-- 6949713057
-- | for demonstration purpose only
main = do
  putStrLn "Hello!"
  finalResult <- X.runExceptT $ do
    msisdn <- liftIO $ do
      putStrLn "MSISDN?"
      readLn
    url <- validateMSISDNSubmission =<< submitMSISDN' "m.mobiworld.biz" "antivirus-kspr" "gr" 1 msisdn
    liftIO $ putStrLn "MSISDN Submission Successful"
    pin <- liftIO $ do
      putStrLn "PIN?"
      readLn
    finalUrl <- validatePINSubmission =<< submitPIN' pin url
    liftIO $ putStrLn "PIN Submission Successful"
    liftIO $ print finalUrl
    return finalUrl
  case finalResult of
    Left (NetworkError e) -> putStrLn "NetworkError" >> print e
    Left (ValidationError bs) -> putStrLn "Validation Error" >> T.writeFile "/Users/homam/temp/submissoinf.html" (E.decodeUtf8 bs)
    Right url -> putStrLn "Success" >> print url
