{-# LANGUAGE LambdaCase #-}

module Dfterm3.Playing.WebSocket
    ( runWebSocket
    , runWebSocketHTTP )
    where

import Dfterm3.Prelude
import Dfterm3.Dfterm3State
import Dfterm3.Logging
import Dfterm3.Playing.Common

import Network.Simple.TCP hiding ( listen )
import Data.Aeson
import Network.WebSockets
import Network.WebSockets.Connection
import Control.Exception
import Control.Concurrent
import Data.ByteString.Internal ( c2w )
import qualified Data.ByteString as B
import qualified System.IO.Streams as ST
import qualified System.IO.Streams.Attoparsec as ST
import qualified Data.CaseInsensitive as CI
import qualified Data.Attoparsec as A
import qualified Happstack.Server as H
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU

runWebSocket :: String -> Word16 -> Storage -> IO ()
runWebSocket hostname port ps = serve (Host hostname) (show port) $ \(s, saddr) -> do
    -- ol == our logger
    let ol txt = logInfo $ "(<WS> " <> show saddr <> ") " <> txt

    flip finally (ol "Connection closed.") $ do
        ol "Connected."

        (sin, sout) <- ST.socketToStreams s
        bout <- ST.builderStream sout

        -- this safe_sin thing is to prevent DoS attacks by sending some
        -- veeeeery large byte string. We keep track how many bytes have been
        -- received that have no yet been handled by the websockets package and
        -- if it crosses the line, cut the connection.
        received_unhandled <- newIORef 0
        end_ref <- newIORef False

        safe_sin <- ST.makeInputStream $ do
                x <- readIORef end_ref
                if x
                  then ST.read sin >>= \case
                        Nothing -> return Nothing
                        Just x -> do
                            len <- atomicModifyIORef'
                                   received_unhandled $ \old ->
                                ( old+B.length x, old+B.length x )
                            if (len > 1000000)
                            then writeIORef end_ref True >>
                                return Nothing
                            else return $ Just x
                  else return Nothing

        request <- ST.parseFromStream (decodeRequestHead False) sin
        let pc = PendingConnection
                 { pendingOptions = defaultConnectionOptions
                 , pendingRequest = request
                 , pendingOnAccept = \_ -> return ()
                 , pendingIn = safe_sin
                 , pendingOut = bout }

        conn <- acceptRequest pc
        ol "Handshook."

        -- I don't know if using an MVar lock is really necessary but I'll do
        -- it just in case.
        send_lock <- newMVar ()

        let sender stcmsg = withMVar send_lock $ \_ ->
                            sendTextData conn $
                            encode stcmsg
            receiver = do
                msg <- unwrapAny <$> receiveDataMessage conn
                writeIORef received_unhandled 0
                case decode msg :: Maybe CTSMessage of
                    Nothing -> ol "Malformed or invalid JSON message." >>
                               receiver
                    Just decoded -> return decoded

        mytid <- myThreadId
        runPlayingSession sender receiver (killThread mytid) ps
  where
    unwrapAny (Text bs) = bs
    unwrapAny (Binary bs) = bs

{- These functions are (almost) directly from the websockets package. See
 - LICENSE.websockets -}
decodeRequestHead :: Bool -> A.Parser RequestHead
decodeRequestHead isSecure = RequestHead
    <$> requestLine
    <*> A.manyTill decodeHeaderLine newline
    <*> pure isSecure
  where
    space   = A.word8 (c2w ' ')
    newline = A.string "\r\n"
    requestLine = A.string "GET" *> space *> A.takeWhile1 (/= c2w ' ')
        <* space
        <* A.string "HTTP/1.1" <* newline

decodeHeaderLine :: A.Parser (CI.CI B.ByteString, B.ByteString)
decodeHeaderLine = (,)
    <$> (CI.mk <$> A.takeWhile1 (/= c2w ':'))
    <*  A.word8 (c2w ':')
    <*  A.option (c2w ' ') (A.word8 (c2w ' '))
    <*> A.takeWhile (/= c2w '\r')
    <*  A.string "\r\n"

-- | Starts the HTTP service. Does not return so you might want to wrap it
-- inside `forkIO`.
--
-- Give the list of ports as the second argument to tell which ports are open
-- for WebSockets.
runWebSocketHTTP :: Word16 -> [Word16] -> IO ()
runWebSocketHTTP port websocket_ports =
    H.simpleHTTP (H.nullConf { H.port = fromIntegral port }) $
        msum [ playing
             , do H.nullDir
                  H.movedPermanently ("playing/" :: String) $
                      H.toResponse ("Redirecting..." :: String)]
  where
    playing = H.dir "playing" $
        msum [ H.dir "resources" $ H.serveDirectory H.DisableBrowsing []
                                   "./web-interface/resources"
             , H.dir "js" $ H.serveDirectory H.DisableBrowsing []
                                   "./web-interface/js"
             , H.dir "dfterm3_ports.js" $
                   H.ok $ H.toResponseBS "text/javascript"
                                         (BL.fromStrict javascripting)
             , H.serveFile (H.asContentType "text/html")
                           "./web-interface/playing.html" ]

    javascripting = BU.fromString $
        "dfterm3_ports = { websocket: [" ++ concatting ++ "] }\n"

    concatting | null websocket_ports = ""
               | otherwise = show (head websocket_ports) ++
                             concatMap (\x -> ", " ++ show x)
                                       (tail websocket_ports)

