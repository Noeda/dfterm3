-- | This module implements the HTTP server to serve the files required for the
-- WebSocket interface.
--

module Dfterm3.WebsocketHTTP
    ( startWebsocketHTTP )
    where

import qualified Happstack.Server as H
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as B
import Data.Word
import Control.Monad

-- | Starts the HTTP service. Does not return so you might want to wrap it
-- inside `forkIO`.
--
-- Give the list of ports as the second argument to tell which ports are open
-- for WebSockets.
startWebsocketHTTP :: Word16 -> [Word16] -> IO ()
startWebsocketHTTP port websocket_ports =
    H.simpleHTTP (H.nullConf { H.port = fromIntegral port }) $
        msum [ playing
             , do H.nullDir
                  H.movedPermanently "playing/" $
                      H.toResponse "Redirecting..." ]
  where
    playing = H.dir "playing" $
        msum [ H.dir "resources" $ H.serveDirectory H.DisableBrowsing []
                                   "./web-interface/resources"
             , H.dir "js" $ H.serveDirectory H.DisableBrowsing []
                                   "./web-interface/js"
             , H.dir "dfterm3_ports.js" $
                   H.ok $ H.toResponseBS (B.fromString "text/javascript")
                                         javascripting
             , H.serveFile (H.asContentType "text/html")
                           "./web-interface/playing.html" ]

    javascripting = B.fromStrict $ B.fromString $
        "dfterm3_ports = { websocket: [" ++ concatting ++ "] }\n"

    concatting | null websocket_ports = ""
               | otherwise = show (head websocket_ports) ++
                             concatMap (\x -> ", " ++ show x)
                                       (tail websocket_ports)

