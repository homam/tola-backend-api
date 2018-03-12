module Web.Testing.Helpers where

import qualified Data.ByteString.Char8     as Char8
import qualified Data.ByteString.Lazy      as BL
import qualified Data.List                 as List
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as E
import           Network.HTTP.Types.Header (HeaderName)
import qualified Network.Wai.Test          as WT
import           Test.Hspec.Wai

test200
  :: Text
  -> (Char8.ByteString -> WaiSession WT.SResponse)
  -> WaiSession WT.SResponse
test200 url f = do
  r <- f (E.encodeUtf8 url)
  liftIO $ printSResponseBody r
  shouldRespondWith (return r) 200
  return r

testPost200 :: Text -> BL.ByteString -> WaiSession WT.SResponse
testPost200 url body = test200 url (`post` body)

testGet200 :: Text -> WaiSession WT.SResponse
testGet200 url = test200 url get

printSResponseHeaders, printSResponseBody :: WT.SResponse -> IO ()
printSResponseHeaders (WT.SResponse _ h _) = print h
printSResponseBody (WT.SResponse _ _ b) = print b

getHeader :: HeaderName -> WT.SResponse -> Maybe Char8.ByteString
getHeader name (WT.SResponse _ h _) = snd <$> List.find ((== name) . fst) h

getHeaderM :: Monad m => HeaderName -> WT.SResponse -> m Char8.ByteString
getHeaderM name r = case getHeader name r of
  Just v  -> return v
  Nothing -> error "Header not found"

getResponseBody :: WT.SResponse -> BL.ByteString
getResponseBody (WT.SResponse _ _ b) = b
