module Main ( main ) where

import qualified Dfterm3.WebsocketAccepter as WS
import Dfterm3.Logging

import GHC.Conc ( setNumCapabilities, getNumCapabilities, getNumProcessors )
import Network ( withSocketsDo )
import Control.Concurrent ( threadDelay )
import Control.Monad ( forever, void )

main :: IO ()
main = withSocketsDo $ do
    initializeLogging
    logInfo "Dfterm3 starting up."

    -- Make sure we get to use all the cores. The unfortunate side effect is
    -- that the rtsopt -N is ignored.
    setNumCapabilities =<< getNumProcessors
    capabilities <- getNumCapabilities

    logInfo $ "Using " ++ show capabilities ++ " operating system threads."

    void $ WS.listen 8000

    -- The server is service oriented and the main function has nothing to do.
    -- Let us loop forever.
    forever $ threadDelay 1000000000

