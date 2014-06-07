{-# LANGUAGE DeriveGeneric #-}

module Dfterm3.Playing.WebSocket
    ( runWebSocket
    , runWebSocketHTTP )
    where

import Dfterm3.Prelude
import Dfterm3.Dfterm3State
import Dfterm3.Logging
import Dfterm3.Game.DwarfFortress

import GHC.Generics
import Network.Simple.TCP hiding ( listen )
import Data.Aeson
import Network.WebSockets
import Network.WebSockets.Connection
import Control.Exception
import Data.ByteString.Internal ( c2w )
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified System.IO.Streams.Attoparsec as ST
import qualified System.IO.Streams.Network as ST
import qualified System.IO.Streams.Builder as ST
import qualified Data.CaseInsensitive as CI
import qualified Data.Attoparsec as A
import qualified Happstack.Server as H
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU

type Logger = String -> IO ()

runWebSocket :: String -> Word16 -> Storage -> IO ()
runWebSocket hostname port _ = serve (Host hostname) (show port) $ \(s, saddr) -> do
    -- ol == our logger
    let ol txt = logInfo $ "(<WS> " <> show saddr <> ") " <> txt

    flip finally (ol "Connection closed.") $ do
        ol "Connected."

        (sin, sout) <- ST.socketToStreams s
        bout <- ST.builderStream sout

        request <- ST.parseFromStream (decodeRequestHead False) sin
        let pc = PendingConnection
                 { pendingOptions = defaultConnectionOptions
                 , pendingRequest = request
                 , pendingOnAccept = \_ -> return ()
                 , pendingIn = sin
                 , pendingOut = bout }

        conn <- acceptRequest pc
        ol "Handshook."

        jsonLoop ol conn

jsonLoop :: Logger -> Connection -> IO ()
jsonLoop ol conn = forever $ do
    msg <- unwrapAny <$> receiveDataMessage conn
    case decode msg :: Maybe CTSMessage of
        Nothing -> ol "Malformed or invalid JSON message."
        Just _ -> return ()
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

data CTSMessage =
    Unsubscribe
  | Chat !T.Text
  | DoInput !KeyDirection !Input
  | Subscribe !Int
  deriving ( Eq, Ord, Show, Read, Typeable, Generic )

instance FromJSON CTSMessage
instance ToJSON CTSMessage

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

