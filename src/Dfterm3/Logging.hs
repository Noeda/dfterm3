-- | Logging in Dfterm3
--
-- We use the hslogging package and this module exposes the interface as it is
-- convenient in Dfterm3.
--

module Dfterm3.Logging
    ( initializeLogging
    , logInfo )
    where

import System.Log.Logger
import Data.Time

-- | Sets up whatever is necessary to start logging properly.
initializeLogging :: IO ()
initializeLogging =
    updateGlobalLogger rootLoggerName $ setLevel INFO

-- | Log normal operations. Things that are not unusual or indicate anything
-- could be wrong.
logInfo :: String -> IO ()
logInfo msg = do
    utc_time <- getCurrentTime
    tz <- getTimeZone utc_time
    let ZonedTime (LocalTime day (TimeOfDay hour minute sec)) _ =
         utcToZonedTime tz utc_time

    infoM rootLoggerName $ showGregorian day ++ "T" ++ show hour ++
                           ":" ++ show minute ++ ":" ++
                           show (floor sec :: Int) ++
                           " : " ++ msg

