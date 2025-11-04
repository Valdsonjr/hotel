{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Log
  ( Config,
    OtlpAnyValue (anyValue),
    Record (..),
    OtlpSeverity (toTextAndNumber),
    (.=),
    withHotelLogger,
    mkHttpConfig,
  )
where

import Colog.Core
  ( LogAction (LogAction),
    Severity (..),
    cmapM,
  )
import Control.Concurrent
  ( MVar,
    forkIO,
    modifyMVar_,
    newMVar,
    swapMVar,
    threadDelay,
  )
import Control.Exception (bracket, try)
import Control.Monad (forever, unless, void)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage, encodeMessage)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import Data.Word (Word16, Word32, Word64, Word8)
import Debug.Trace (traceShowM)
import Lens.Family2 (set)
import Network.HTTP.Client
  ( HttpException,
    Manager,
    Request (method, requestBody, requestHeaders, responseTimeout),
    RequestBody (RequestBodyBS),
    httpNoBody,
    parseRequest_,
    responseTimeoutMicro,
  )
import Paths_hotel (version)
import qualified Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService as Logs
import qualified Proto.Opentelemetry.Proto.Common.V1.Common as Common
import qualified Proto.Opentelemetry.Proto.Common.V1.Common_Fields as Common
import qualified Proto.Opentelemetry.Proto.Logs.V1.Logs as Logs
import qualified Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields as Logs
import Proto.Opentelemetry.Proto.Resource.V1.Resource (Resource)

-- | Logger and OTLP config
data Config = Config
  { resourceAttrs :: [Common.KeyValue],
    otlpEndpoint :: String,
    flushIntervalSec :: Word,
    bufferLimit :: Word,
    manager :: Manager
  }

-- | "smart" constructor for a http-based logger config
mkHttpConfig ::
  -- | List of attributes of a Resource, this means that if you have multiple resources you must define multiple configurations and get multiple loggers
  [Common.KeyValue] ->
  -- | HTTP endpoint of an otlp-compatible service
  String ->
  -- | Flush interval in seconds
  Word ->
  -- | Max number of log entries before flush
  Word ->
  Manager ->
  Config
mkHttpConfig = Config

-- | Construct KeyValue's
(.=) :: (OtlpAnyValue a) => S.Text -> a -> Common.KeyValue
(.=) k v = set Common.value (anyValue v) $ set Common.key k defMessage

-- | HOTel simplified log record, it's not supposed to be used directly in application code instead, you should write a map between your domain log events and this one and use it like
-- let appLogger = contramap domainEventToRecord hotelLogger
data Record sev = Record
  { severity :: Maybe sev,
    time :: Maybe Word64,
    message :: S.Text,
    attributes :: [Common.KeyValue],
    traceId :: Maybe SB.ByteString,
    spanId :: Maybe SB.ByteString,
    eventName :: Maybe S.Text
  }

-- | Bracket-style logger that flushes all buffered events gracefuly on exit
withHotelLogger ::
  (OtlpSeverity sev) =>
  Config ->
  (LogAction IO (Record sev) -> IO a) ->
  IO a
withHotelLogger cfg action = do
  bracket
    (mkHotelLogger cfg)
    (\(_, mvar) -> flushAll mvar cfg)
    (\(logAction, _) -> action logAction)

--------------------------------------------------------------------------------
-- Typeclasses and instances
class OtlpSeverity a where
  toTextAndNumber :: a -> (S.Text, Logs.SeverityNumber)

instance OtlpSeverity () where
  toTextAndNumber () = ("unknown", Logs.SEVERITY_NUMBER_UNSPECIFIED)

instance OtlpSeverity Severity where
  toTextAndNumber Debug = ("debug", Logs.SEVERITY_NUMBER_DEBUG)
  toTextAndNumber Info = ("info", Logs.SEVERITY_NUMBER_INFO)
  toTextAndNumber Warning = ("warn", Logs.SEVERITY_NUMBER_WARN)
  toTextAndNumber Error = ("error", Logs.SEVERITY_NUMBER_ERROR)

class OtlpAnyValue a where
  anyValue :: a -> Common.AnyValue

instance OtlpAnyValue Int where
  anyValue x = set Common.intValue (fromIntegral x) defMessage

instance OtlpAnyValue Int16 where
  anyValue x = set Common.intValue (fromIntegral x) defMessage

instance OtlpAnyValue Int8 where
  anyValue x = set Common.intValue (fromIntegral x) defMessage

instance OtlpAnyValue Int32 where
  anyValue x = set Common.intValue (fromIntegral x) defMessage

instance OtlpAnyValue Int64 where
  anyValue x = set Common.intValue x defMessage

instance OtlpAnyValue Word where
  anyValue x = set Common.intValue (fromIntegral x) defMessage

instance OtlpAnyValue Word8 where
  anyValue x = set Common.intValue (fromIntegral x) defMessage

instance OtlpAnyValue Word16 where
  anyValue x = set Common.intValue (fromIntegral x) defMessage

instance OtlpAnyValue Word32 where
  anyValue x = set Common.intValue (fromIntegral x) defMessage

instance OtlpAnyValue Float where
  anyValue x = set Common.doubleValue (realToFrac x) defMessage

instance OtlpAnyValue Double where
  anyValue x = set Common.doubleValue x defMessage

instance OtlpAnyValue S.Text where
  anyValue x = set Common.stringValue x defMessage

instance OtlpAnyValue L.Text where
  anyValue x = set Common.stringValue (L.toStrict x) defMessage

instance OtlpAnyValue Bool where
  anyValue x = set Common.boolValue x defMessage

instance OtlpAnyValue LB.ByteString where
  anyValue x = set Common.bytesValue (LB.toStrict x) defMessage

instance OtlpAnyValue SB.ByteString where
  anyValue x = set Common.bytesValue x defMessage

instance (OtlpAnyValue a) => OtlpAnyValue [a] where
  anyValue x =
    set
      Common.arrayValue
      (set Common.values (map anyValue x) defMessage)
      defMessage

instance OtlpAnyValue [Char] where
  anyValue x = set Common.stringValue (S.pack x) defMessage

instance OtlpAnyValue [Common.KeyValue] where
  anyValue x = set Common.kvlistValue (set Common.values x defMessage) defMessage

--------------------------------------------------------------------------------
-- Auxiliary functions

-- | Constrói um Resource OTLP a partir da lista de atributos do Config
mkResource :: [Common.KeyValue] -> Resource
mkResource attrs = set Logs.attributes attrs defMessage

-- | Envia o payload OTLP via HTTP/Protobuf
sendOtlpLog :: String -> Logs.ExportLogsServiceRequest -> Manager -> IO ()
sendOtlpLog route payload man = do
  let req = parseRequest_ route
      request =
        req
          { method = "POST",
            requestBody = RequestBodyBS (encodeMessage payload),
            requestHeaders = ("Content-Type", "application/x-protobuf") : requestHeaders req,
            responseTimeout = responseTimeoutMicro 15000000
          }
  result <- try $ httpNoBody request man
  case result of
    Left e -> putStrLn $ "OTLP log send failed: " <> show (e :: HttpException)
    Right res -> traceShowM res

getUnixTimeInNanoseconds :: IO Word64
getUnixTimeInNanoseconds = do
  posixTime <- getPOSIXTime
  pure $ truncate (posixTime * 1000000000)

toOtlp :: (OtlpSeverity sev) => Word64 -> Record sev -> Logs.LogRecord
toOtlp t rec =
  set Logs.attributes (attributes rec) $
    maybe id (set Logs.severityNumber . snd . toTextAndNumber) (severity rec) $
      maybe id (set Logs.severityText . fst . toTextAndNumber) (severity rec) $
        maybe id (set Logs.eventName) (eventName rec) $
          maybe id (set Logs.traceId) (traceId rec) $
            maybe id (set Logs.spanId) (spanId rec) $
              set Logs.observedTimeUnixNano t $
                set Logs.body (anyValue (message rec)) $
                  set
                    Logs.timeUnixNano
                    (fromMaybe t $ time rec)
                    defMessage

mkResourceLogsBatch :: Resource -> [Logs.LogRecord] -> Logs.ResourceLogs
mkResourceLogsBatch res logRecs =
  set Logs.scopeLogs [scopeLog] $ set Logs.resource res defMessage
  where
    scopeLog :: Logs.ScopeLogs
    scopeLog = set Logs.scope scope $ set Logs.logRecords logRecs defMessage

    scope :: Common.InstrumentationScope
    scope =
      set Common.version (S.pack $ showVersion version) $
        set Common.name "haskell.hotel.logger" defMessage

mkHotelLogger ::
  (OtlpSeverity sev) =>
  Config ->
  IO (LogAction IO (Record sev), MVar [Logs.LogRecord])
mkHotelLogger cfg = do
  (otlpLogger, mvar) <- mkOtlpLoggerWithMVar cfg
  pure (cmapM (\x -> (`toOtlp` x) <$> getUnixTimeInNanoseconds) otlpLogger, mvar)

mkOtlpLoggerWithMVar :: Config -> IO (LogAction IO Logs.LogRecord, MVar [Logs.LogRecord])
mkOtlpLoggerWithMVar cfg = do
  mvar <- newMVar []

  void $ forkIO $ forever $ do
    threadDelay $ fromIntegral $ flushIntervalSec cfg * 1000000
    flushAll mvar cfg

  let action = LogAction $ \logRec ->
        modifyMVar_ mvar $ \logs ->
          let newLogs = logRec : logs
           in if length newLogs >= fromIntegral (bufferLimit cfg)
                then flushLogs cfg newLogs >> pure []
                else pure newLogs

  pure (action, mvar)

flushAll :: MVar [Logs.LogRecord] -> Config -> IO ()
flushAll mvar cfg = do
  logsToSend <- swapMVar mvar []
  unless (null logsToSend) $ flushLogs cfg logsToSend

flushLogs :: Config -> [Logs.LogRecord] -> IO ()
flushLogs cfg logsToSend = do
  let resourceLogs = mkResourceLogsBatch (mkResource $ resourceAttrs cfg) logsToSend
      request = set Logs.resourceLogs [resourceLogs] defMessage
  sendOtlpLog (otlpEndpoint cfg) request (manager cfg)
