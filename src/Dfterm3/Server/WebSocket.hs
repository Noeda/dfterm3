module Dfterm3.Server.WebSocket
    ( runWebSocket )
    where

import Dfterm3.Dfterm3State

import Data.Word
import Control.Applicative

runWebSocket :: Word16 -> Storage -> IO ()
runWebSocket port storage = pure ()

