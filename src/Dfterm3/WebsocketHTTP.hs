-- | This module implements the HTTP server to serve the files required for the
-- WebSocket interface.
--

module Dfterm3.WebsocketHTTP
    ( startWebsocketHTTP )
    where

import qualified Happstack.Server as H
import Data.Word
import Control.Monad

-- | Starts the HTTP service. Does not return so you might want to wrap it
-- inside `forkIO`.
startWebsocketHTTP :: Word16 -> IO ()
startWebsocketHTTP port =
    H.simpleHTTP (H.nullConf { H.port = fromIntegral port }) $
        H.dir "playing" $
            msum [ H.dir "resources" $ H.serveDirectory H.DisableBrowsing []
                                       "./web-interface/resources"
                 , H.dir "js" $ H.serveDirectory H.DisableBrowsing []
                                       "./web-interface/js"
                 , H.serveFile (H.asContentType "text/html")
                               "./web-interface/playing.html" ]

