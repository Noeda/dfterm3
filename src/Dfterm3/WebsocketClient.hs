-- | This module works with `Dfterm3.WebsocketAccepter`. This is the code that
-- talks to the client on the other side of the WebSocket connection.
--

module Dfterm3.WebsocketClient
    ( websocketClient )
    where

import System.IO
import Network.WebSockets
import Control.Monad.IO.Class ( liftIO )
import Control.Monad ( forever )
import Data.Enumerator
import Data.Enumerator.Binary

-- Probably should change to `Rfc6455` when (or if) that becomes available in
-- the websockets library.
type DftermProto = Hybi10

websocketClient :: Handle -> IO ()
websocketClient handle = do
    let enumerator = enumHandle 8192 handle
    run_ $ enumerator $$
           runWebSocketsHandshake True ar (iterHandle handle)
  where
    ar :: Request -> WebSockets DftermProto ()
    ar request = acceptRequest request >> client

client :: WebSockets DftermProto ()
client = forever $ do
    message <- receive
    liftIO $ print message


