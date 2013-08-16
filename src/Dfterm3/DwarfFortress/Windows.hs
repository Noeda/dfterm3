module Dfterm3.DwarfFortress.Windows
    ( launchDwarfFortress
    , trackRunningFortress
    , untrackRunningFortress
    , DFPid )
    where

import Data.Word ( Word32 )
import Dfterm3.DwarfFortress.Types

type DFPid = Word32

trackRunningFortress :: DFPid -> IO ()
trackRunningFortress _ = return ()

untrackRunningFortress :: DFPid -> IO ()
untrackRunningFortress _ = return ()

launchDwarfFortress :: DwarfFortress
                    -> (Maybe DwarfFortressInstance -> IO ())
                    -> IO ()
launchDwarfFortress _ action = action Nothing

