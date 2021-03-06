{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- NOTE: Due to https://github.com/yesodweb/wai/issues/192, this module should
-- not use CPP.
module Web.Logging.DetailedLoggerMiddleware
    ( -- * Basic stdout logging
      detailedMiddleware
    , RequestLoggerSettings
    , outputFormat
    , autoFlush
    , destination
    , OutputFormat (..)
    , OutputFormatter
    , OutputFormatterWithDetails
    , Destination (..)
    , Callback
    , IPAddrSource (..)
    , VaultLogger, VaultLoggerKey
    , withDetailedLoggerMiddleware
    , simpleStdoutLogType
    , noneLogType
    , fileLogType
    ) where

import qualified Blaze.ByteString.Builder  as B
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.ByteString           as BS
import           Data.ByteString.Char8     (pack)
import qualified Data.ByteString.Char8     as S8
import qualified Data.ByteString.Lazy      as LBS
import           Data.Default.Class        (Default (def))
import           Data.IORef.Lifted
import           Data.Monoid               (mconcat, (<>))
import qualified Data.Text as T
import           Data.Text.Encoding        (decodeUtf8')
import           Data.Time                 (NominalDiffTime, diffUTCTime,
                                            getCurrentTime)
import           Network.HTTP.Types        as H
import           Network.Wai               (Middleware, Request (..),
                                            RequestBodyLength (..), Response,
                                            requestBodyLength, responseLBS,
                                            responseStatus, responseToStream)
import           Network.Wai.Header        (contentLength)
import           Network.Wai.Internal      (Response (..))
import           Network.Wai.Logger
import           Network.Wai.Parse         (File, Param, fileName,
                                            getRequestBodyType, lbsBackEnd,
                                            sinkRequestBody)
import           System.Console.ANSI
import           System.IO                 (Handle, stdout)
import           System.Log.FastLogger
--
import qualified Control.Concurrent.MVar   as M
import           Control.Exception
import           Control.Monad             (void)
import qualified Data.CaseInsensitive      as CI
import qualified Data.IORef                as IORef
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import qualified Data.Vault.Lazy           as V
import qualified Network.HTTP.Types.Status as Status
import qualified Network.Wai               as W
import           Web.Logging.Logger

data OutputFormat = Apache IPAddrSource
                  | Detailed Bool -- ^ use colors?
                  | CustomOutputFormat OutputFormatter
                  | CustomOutputFormatWithDetails OutputFormatterWithDetails

type OutputFormatter = ZonedDate -> Request -> Status -> Maybe Integer -> LogStr
type OutputFormatterWithDetails =
  ZonedDate -> Request -> Status -> Maybe Integer -> NominalDiffTime -> [S8.ByteString] -> B.Builder -> LogStr

data Destination = Handle Handle
                 | Logger LoggerSet
                 | Callback Callback

type Callback = LogStr -> IO ()

-- | @RequestLoggerSettings@ is an instance of Default. See <https://hackage.haskell.org/package/data-default Data.Default> for more information.
--
-- @outputFormat@, @autoFlush@, and @destination@ are record fields
-- for the record type @RequestLoggerSettings@, so they can be used to
-- modify settings values using record syntax.
data RequestLoggerSettings = RequestLoggerSettings
    {
      -- | Default value: @Detailed@ @True@.
      outputFormat :: OutputFormat
      -- | Only applies when using the @Handle@ constructor for @destination@.
      --
      -- Default value: @True@.
    , autoFlush    :: Bool
      -- | Default: @Handle@ @stdout@.
    , destination  :: Destination
    }

instance Default RequestLoggerSettings where
    def = RequestLoggerSettings
        { outputFormat = Detailed True
        , autoFlush = True
        , destination = Handle stdout
        }

-- | Prints a message using the given callback function for each request.
-- This is not for serious production use- it is inefficient.
-- It immediately consumes a POST body and fills it back in and is otherwise inefficient
--
-- Note that it logs the request immediately when it is received.
-- This meanst that you can accurately see the interleaving of requests.
-- And if the app crashes you have still logged the request.
-- However, if you are simulating 10 simultaneous users you may find this confusing.
--
-- This is lower-level - use 'logStdoutDev' unless you need greater control.
--
-- Example ouput:
--
-- > GET search
-- >   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
-- >   Status: 200 OK 0.010555s
-- >
-- > GET static/css/normalize.css
-- >   Params: [("LXwioiBG","")]
-- >   Accept: text/css,*/*;q=0.1
-- >   Status: 304 Not Modified 0.010555s

detailedMiddleware :: V.Key VaultLogger -> Callback -> IO Integer -> Bool -> IO Middleware
detailedMiddleware loggerVaultKey cb uniqueIdGenerator useColors =
    let (ansiColor, ansiMethod, ansiStatusCode) =
          if useColors
            then (ansiColor', ansiMethod', ansiStatusCode')
            else (\_ t -> [t], (:[]), \_ t -> [t])

    in return $ detailedMiddleware' loggerVaultKey cb uniqueIdGenerator ansiColor ansiMethod ansiStatusCode

ansiColor' :: Color -> BS.ByteString -> [BS.ByteString]
ansiColor' color bs =
    [ pack $ setSGRCode [SetColor Foreground Dull color]
    , bs
    , pack $ setSGRCode [Reset]
    ]

-- | Tags http method with a unique color.
ansiMethod' :: BS.ByteString -> [BS.ByteString]
ansiMethod' m = case m of
    "GET"    -> ansiColor' Cyan m
    "HEAD"   -> ansiColor' Cyan m
    "PUT"    -> ansiColor' Green m
    "POST"   -> ansiColor' Yellow m
    "DELETE" -> ansiColor' Red m
    _        -> ansiColor' Magenta m

ansiStatusCode' :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
ansiStatusCode' c t = case S8.take 1 c of
    "2" -> ansiColor' Green t
    "3" -> ansiColor' Yellow t
    "4" -> ansiColor' Red t
    "5" -> ansiColor' Magenta t
    _   -> ansiColor' Blue t

getRequestBody :: Request -> IO (Request, [S8.ByteString])
getRequestBody req = do
  let loop front = do
         bs <- requestBody req
         if S8.null bs
             then return $ front []
             else loop $ front . (bs:)
  body <- loop id
  -- logging the body here consumes it, so fill it back up
  -- obviously not efficient, but this is the development logger
  --
  -- Note: previously, we simply used CL.sourceList. However,
  -- that meant that you could read the request body in twice.
  -- While that in itself is not a problem, the issue is that,
  -- in production, you wouldn't be able to do this, and
  -- therefore some bugs wouldn't show up during testing. This
  -- implementation ensures that each chunk is only returned
  -- once.
  ichunks <- newIORef body
  let rbody = atomicModifyIORef ichunks $ \case
             []  -> ([], S8.empty)
             x:y -> (y, x)
  let req' = req { requestBody = rbody }
  return (req', body)

tellLog :: IORef.IORef [BS.ByteString] -> VaultLogger
tellLog ref bs = IORef.modifyIORef ref (bs :)

isChargeRequestPath :: [T.Text] -> Bool
isChargeRequestPath ("api":"check_charge":_) = True
isChargeRequestPath _ = False

detailedMiddleware' :: V.Key VaultLogger -> Callback
                    -> IO Integer
                    -> (Color -> BS.ByteString -> [BS.ByteString])
                    -> (BS.ByteString -> [BS.ByteString])
                    -> (BS.ByteString -> BS.ByteString -> [BS.ByteString])
                    -> Middleware
detailedMiddleware' loggerVaultKey cb uniqueIdGenerator ansiColor ansiMethod ansiStatusCode app req1 sendResponse = do
    requestId <- fmap (S8.pack . show) uniqueIdGenerator
    let req = addHeader ("X-RequestId", requestId) req1
    if isChargeRequestPath (pathInfo req)
        then app req sendResponse
        else detailedMiddlewareNoFilter requestId loggerVaultKey cb ansiColor ansiMethod ansiStatusCode app req sendResponse

detailedMiddlewareNoFilter requestId loggerVaultKey cb ansiColor ansiMethod ansiStatusCode app req sendResponse = do

    (req', body) <-
        -- second tuple item should not be necessary, but a test runner might mess it up
        case (requestBodyLength req, contentLength (requestHeaders req)) of
            -- log the request body if it is small
            (KnownLength len, _) | len <= 2048 -> getRequestBody req
            (_, Just len)        | len <= 2048 -> getRequestBody req
            _                    -> return (req, [])

    let headers' = foldl1 (<>) $ map (("\n    " <>) . (\ (k, v) -> CI.foldedCase k <> ": " <> v)) (W.requestHeaders req)
        -- path = W.rawPathInfo req
        rawQueryString' = W.rawQueryString req
    let reqbodylog _ = if null body then [""] else ansiColor White "  Request Body: " <> body <> ["\n"]
        reqbody = concatMap (either (const [""]) reqbodylog . decodeUtf8') body
    postParams <- if requestMethod req `elem` ["GET", "HEAD"]
        then return []
        else do postParams <- liftIO $ allPostParams body
                return $ collectPostParams postParams

    let getParams = map emptyGetParam $ queryString req
        params = let par | not $ null postParams = [pack (show postParams)]
                         | not $ null getParams  = [pack (show getParams)]
                         | otherwise             = []
                 in if null par then [""] else ansiColor White "  Params: " <> par <> ["\n"]


    t0 <- getCurrentTime

    let cb' = cb . (\ l -> mconcat $ map toLogStr $ ["\n----Start>\n"] ++
            ansiColor Green "RequestId: " ++ [requestId, "\n"] ++
            ansiMethod (requestMethod req) ++ [" ", rawPathInfo req, "\n"] ++
            params ++
            reqbody ++
            -- ansiColor White "  Accept: " ++ [accept, "\n"] ++ l)
            ansiColor White "  Request Headers: " ++ [headers', "\n"] ++
            ansiColor White "  Raw Query String: " ++ [rawQueryString', "\n"]
            ++ l
            ++ ["\n----End>\n"]
            )
    logIORef <- IORef.newIORef []
    let vault' = V.insert loggerVaultKey (tellLog logIORef) (vault req')
        req'' = req' { vault = vault' }
    catch (app req'' $ \rsp -> do
        let isRaw =
                case rsp of
                    ResponseRaw{} -> True
                    _             -> False
            stCode = statusBS rsp
            stMsg = msgBS rsp

        t1 <- getCurrentTime
        respBody <- responseBody rsp
        vaultLogs <- map (S8.concat . map (\s -> "    " <> s <> "\n") . S8.split '\n') . reverse <$> IORef.readIORef logIORef

        -- log the status of the response
        cb' $
            if isRaw then [] else
                ansiColor White "  Status: " ++
                ansiStatusCode stCode (stCode <> " " <> stMsg) ++
                ansiColor White "\n  Vault Logs: " ++ map ("\n" <>) vaultLogs ++
                ["\n  ", pack $ show $ diffUTCTime t1 t0, "\n"] ++
                ansiColor White "  Response Body: " ++ [LBS.toStrict respBody]
        sendResponse rsp
        ) (\ (ex :: SomeException) -> do
            let stCode = pack . show . statusCode $ status500
                stMsg = statusMessage  status500
                respBody = S8.pack $ show ex
            t1 <- getCurrentTime
            vaultLogs <- map (S8.concat . map (\s -> "    " <> s <> "\n") . S8.split '\n') . reverse <$> IORef.readIORef logIORef
            cb' $
                ansiColor White "  Status: " ++
                ansiStatusCode stCode (stCode <> " " <> stMsg) ++
                ansiColor White "\n Vault Logs: " ++ map ("\n" <>) vaultLogs ++
                [" ", pack $ show $ diffUTCTime t1 t0, "\n"] ++
                ansiColor White "  Response Body: " ++ [respBody]
            sendResponse (responseLBS Status.status500 [] (LBS.fromStrict respBody)))

  where
    allPostParams body =
        case getRequestBodyType req of
            Nothing -> return ([], [])
            Just rbt -> do
                ichunks <- newIORef body
                let rbody = atomicModifyIORef ichunks $ \case
                            []  -> ([], S8.empty)
                            x:y -> (y, x)
                sinkRequestBody lbsBackEnd rbt rbody

    emptyGetParam :: (BS.ByteString, Maybe BS.ByteString) -> (BS.ByteString, BS.ByteString)
    emptyGetParam (k, Just v)  = (k,v)
    emptyGetParam (k, Nothing) = (k,"")

    collectPostParams :: ([Param], [File LBS.ByteString]) -> [Param]
    collectPostParams (postParams, files) = postParams ++
      map (\(k,v) -> (k, "FILE: " <> fileName v)) files



statusBS :: Response -> BS.ByteString
statusBS = pack . show . statusCode . responseStatus

msgBS :: Response -> BS.ByteString
msgBS = statusMessage . responseStatus



responseBody :: Response -> IO LBS.ByteString
responseBody res =
    let (_, _, body) = responseToStream res
    in  body $ \f -> do
            content <- newIORef mempty
            f (\chunk -> modifyIORef' content (<>chunk)) (return ())
            B.toLazyByteString <$> readIORef content

-- protect :: [H.ResponseHandler] -> Middleware
-- product handlers app req = catches (app req) (wrapHandlers handlers)
--         where wrapHandlers = fmap (\(ResponseHandler f) -> Handler (`f` req) )

addHeader :: (BS.ByteString, BS.ByteString) -> Request -> Request
addHeader (k, v) req = req {
    requestHeaders = (CI.mk k, v) : requestHeaders req
  }

---

simpleStdoutLogType :: LogType
simpleStdoutLogType = LogStdout defaultBufSize

noneLogType :: LogType
noneLogType = LogNone

fileLogType :: FilePath -> LogType
fileLogType path =
    let spec = FileLogSpec path 10 10 in LogFile spec defaultBufSize

withLogger :: forall msg b
     . ToLogStr msg
    => LogType
    -> ((msg -> IO ()) -> IO b)
    -> IO b
withLogger logType f = do
    timeCache         <- newTimeCache myTimeFormat
    withTimedFastLogger
        timeCache
        logType
        (\logger -> f $ void . logT logger)
  where
    myTimeFormat :: TimeFormat
    myTimeFormat = "%Y-%m-%dT%H:%M:%S%z"

    logT :: ToLogStr msg => TimedFastLogger -> msg -> IO ()
    logT logger msg = logger $ \ft ->
        toLogStr ft <> toLogStr (": " :: String) <> toLogStr msg <> "\n"


withDetailedLoggerMiddleware ::
     VaultLoggerKey
  -> (W.Middleware -> IO b) -- ^ A callback function receiving a logger and a Scotty app. Use either 'runWebServer' or 'runWebM' for the callback.
  -> LogType
  -> IO b
withDetailedLoggerMiddleware loggerVaultKey cb logType = do
  let uniqueIdGenerator = uniqueTimestamp (100000 :: Double) =<< M.newMVar 0
  withLogger
    logType
    ( \logger -> do
      logger'' <- detailedMiddleware loggerVaultKey
                                     logger
                                     uniqueIdGenerator
                                     True
      cb logger''
    )

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

getTime :: (Integral b, RealFrac a) => a -> IO b
getTime precision =
  round . (* precision) . fromRational . toRational <$> getPOSIXTime
