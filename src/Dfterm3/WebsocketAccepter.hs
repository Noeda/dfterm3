-- | The module that exposes functions to listen and accept websocket
-- connections.
--
--

module Dfterm3.WebsocketAccepter
    ( listen )
    where

import Dfterm3.ServiceKiller
import Dfterm3.Safe
import Dfterm3.Logging
import Dfterm3.WebsocketClient

import Network
import System.IO
import Data.Word ( Word16 )
import qualified Network.Socket as S
import Control.Applicative ( (<$>) )
import Control.Concurrent
import Control.Monad ( forever )

-- | Listen for websocket connections and accept them.
--
-- To stop listening for connections, invoke `Dfterm3.ServiceKiller.kill` on
-- the returned handle.
listen :: Word16 -> IO KillHandle
listen port_number = do
    listener_socket <- listenOn (PortNumber $ safeFromIntegral port_number)
    logInfo $ "Listening for WebSocket connections on port " ++
              show port_number ++ "."

    newKillHandle <$> (forkFinally (listener listener_socket) $ \_ -> do
        S.close listener_socket)   -- Don't wait for garbage collector
  where
    listener listener_socket = forever $ do
        (new_client_handle, host_name, port) <- accept listener_socket
        logInfo $ "New connection to WebSocket interface on port " ++
                  show port_number ++ " from " ++ show host_name ++ ":" ++
                  show port

        forkFinally (websocketClient new_client_handle) $ \_ -> do
            hFlush new_client_handle
            hClose new_client_handle
            logInfo $ "Connection closed on WebSocket interface on port " ++
                      show port_number ++ " to " ++ show host_name ++ ":" ++
                      show port

