-- | The module that exposes functions to listen and accept websocket
-- connections.
--
--

module Dfterm3.WebsocketAccepter
    ( listen )
    where

import Dfterm3.GamePool
import Dfterm3.User
import Dfterm3.UserVolatile
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
listen :: GamePool -> UserSystem -> UserVolatile -> Word16 -> IO KillHandle
listen pool us uv port_number = do
    listener_socket <- listenOn (PortNumber $ safeFromIntegral port_number)
    logInfo $ "Listening for WebSocket connections on port " ++
              show port_number ++ "."

    newKillHandle <$> forkFinally (listener listener_socket) (\_ ->
        S.close listener_socket)   -- Don't wait for garbage collector
  where
    listener listener_socket = forever $ do
        (client_socket, saddr) <- S.accept listener_socket

        logInfo $ "New connection to WebSocket interface from " ++ show saddr

        -- Disable Nagle's algorithm
        S.setSocketOption client_socket S.NoDelay 1
        new_client_handle <- S.socketToHandle client_socket ReadWriteMode

        forkFinally (websocketClient pool us uv new_client_handle) $ \_ -> do
            hFlush new_client_handle
            hClose new_client_handle
            logInfo $ "Connection closed on WebSocket interface from " ++
                      show saddr

