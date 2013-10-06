-- | Logging in Dfterm3
--
-- We use the hslogging package and this module exposes the interface as it is
-- convenient in Dfterm3.
--

{-# LANGUAGE ViewPatterns #-}

module Dfterm3.Logging
    ( initializeLogging
    , logInfo
    , logNotice
    , logWarning
    , LoggingSystem(..) )
    where

import System.IO ( stdout )
import System.IO.Unsafe ( unsafePerformIO )
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import Data.IORef
import Data.Time

data LoggingSystem = Simple | Syslog

currentSystem :: IORef LoggingSystem
currentSystem = unsafePerformIO $ newIORef Simple
{-# NOINLINE currentSystem #-}

-- | Sets up whatever is necessary to start logging properly.
initializeLogging :: LoggingSystem -> IO ()
initializeLogging sys = do
    writeIORef currentSystem sys
    initializeLogging' sys
  where
    initializeLogging' Simple = do
        stdout_handler <- streamHandler stdout INFO
        updateGlobalLogger rootLoggerName $
            setLevel INFO . setHandlers [stdout_handler]
    initializeLogging' Syslog = do
        syslog_handler <- openlog "dfterm3" [PID] DAEMON INFO
        updateGlobalLogger rootLoggerName $
            setLevel INFO . setHandlers [syslog_handler]

isSimplistic :: LoggingSystem -> Bool
isSimplistic Simple = True
isSimplistic Syslog = False

stamp :: String -> IO String
stamp string = do
    sys <- readIORef currentSystem
    case sys of
        (isSimplistic -> True) -> do
            utc_time <- getCurrentTime
            tz <- getTimeZone utc_time
            let ZonedTime (LocalTime day (TimeOfDay hour minute sec)) _ =
                 utcToZonedTime tz utc_time
            return $ showGregorian day ++ "T" ++ pad 2 (show hour) ++
                           ":" ++ pad 2 (show minute) ++ ":" ++
                           pad 2 (show (floor sec :: Int)) ++
                           " : " ++ string
        _ -> return string
  where
    pad :: Int -> String -> String
    pad len str
        | length str >= len = str
        | otherwise = pad len ('0':str)

-- | Log normal operations. Things that are not unusual or indicate anything
-- could be wrong.
logInfo :: String -> IO ()
logInfo msg = do
    res <- stamp msg
    infoM rootLoggerName res

-- | Something unusual happened but nothing that indicates an error or warning.
logNotice :: String -> IO ()
logNotice msg = do
    res <- stamp msg
    noticeM rootLoggerName res

-- | Something unusual is going on that implies something may be wrong.
-- However, operation can continue.
logWarning :: String -> IO ()
logWarning msg = do
    res <- stamp msg
    warningM rootLoggerName res

