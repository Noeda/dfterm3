-- | WebSocket-based playing interface.
--

{-# LANGUAGE CPP, OverloadedStrings #-}

module Dfterm3.Playing.WebInterface
    ( runWebSocket
    , runWebsocketHTTP )
    where

import Dfterm3.Util
import Dfterm3.User
import Dfterm3.Dfterm3State
import Dfterm3.GameSubscription
import Dfterm3.Game.DwarfFortress
import Dfterm3.Logging
import Dfterm3.Terminal
import Dfterm3.CP437ToUnicode
import System.IO

import Data.Array.IArray ( assocs, bounds )
import Data.Serialize.Put
import Data.Bits

import Data.Word
import Data.Vector ( fromList )
import Network
import Network.WebSockets
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent
import Control.Lens
import qualified Network.Socket as S
import Data.Enumerator hiding ( length, filter, head, concatMap )
import Data.Enumerator.Binary hiding ( zipWith, filter, head, concatMap )
import qualified Data.Set as S
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Happstack.Server as H

-- Probably should change to `Rfc6455` when (or if) that becomes available in
-- the websockets library.
type DftermProto = Hybi10

-- | Starts listening for WebSocket connections on the given port.
--
-- You also need a web server that serves the files that instruct the user
-- browser to connect to this port. For that you can use `listenWebSocketHTTP`
-- or have your own web server.
--
-- This call does not return. Use `forkIO` to run it on the background.
runWebSocket :: Word16 -> Storage -> IO ()
runWebSocket port_number ps = do
    listener_socket <- listenOn (PortNumber $ safeFromIntegral port_number)
    logInfo $ "Listening for WebSocket connections on port " ++
              show port_number ++ "."

    forever $ do
        ( client_socket, saddr ) <- S.accept listener_socket

        logInfo $ "New connection WebSocket interface from " ++ show saddr
        -- Disable Nagle's algorithm
        S.setSocketOption client_socket S.NoDelay 1

        new_client_handle <- S.socketToHandle client_socket ReadWriteMode
        forkFinally (websocketClient ps new_client_handle) $ \_ -> do
            hFlush new_client_handle
            hClose new_client_handle
            logInfo $ "Connection closed on WebSocket interface from " ++
                      show saddr


websocketClient :: Storage -> Handle -> IO ()
websocketClient ps handle = do
    hSetBuffering handle NoBuffering
    hSetBinaryMode handle True
    hFlush handle
    -- For horrible reasons, Windows needs the buffer size to be 1. 1! The
    -- reason is misbehaving hGetBufNonBlocking that is used behind the scenes.
#ifdef WINDOWS
    let enumerator = enumHandle 1 handle
#else
    let enumerator = enumHandle 8192 handle
#endif
    run_ $ enumerator $$
           runWebSocketsHandshake True ar (iterHandle handle)
  where
    ar :: Request -> WebSockets DftermProto ()
    ar request = acceptRequest request >> client ps

client :: Storage -> WebSockets DftermProto ()
client ps = do
    user <- liftIO $ newGuestUser ps
    receiveLogin user ps
    forever $ chooseYourGame user ps

sendJSON :: B.ByteString -> WebSockets DftermProto ()
sendJSON bs = sendBinaryData $ BL.singleton 2 `BL.append` BL.fromStrict bs

receiveLogin :: User -> Storage -> WebSockets DftermProto ()
receiveLogin user ps = do
    Text msg <- receiveDataMessage
    case B.head $ BL.toStrict msg of
        -- Login
        4 -> let name = T.take 20 $
                            T.decodeUtf8 $ BL.toStrict $ BL.tail msg
              in do result <- liftIO $ loginUser user name ps
                    if result
                      then sendBinaryData (BL.singleton 4)
                      else sendBinaryData (BL.singleton 3) >> recurse

        _ -> recurse
  where
    recurse = receiveLogin user ps

chooseYourGame :: User -> Storage -> WebSockets DftermProto ()
chooseYourGame user ps = do
    games <- liftIO $
        runSubscriptionIO ps
        (lookForPublishedGames :: SubscriptionIO [DwarfFortressPersistent])

    sendJSON $ BL.toStrict $ J.encode (fromList
                [ J.String "game_list"
                , J.Array $ fromList $
                    zipWith pairify
                        (fmap (J.String . (^. customName)) games)
                        (fmap J.toJSON [(0::Int)..]) ] )

    maybe_choice <- receiveChoice
    case maybe_choice of
        Nothing -> return ()
        Just choice -> choseGame $ games !! choice
  where
    choseGame :: DwarfFortressPersistent -> WebSockets DftermProto ()
    choseGame chosen_df = do
        maybe_ginstance <- liftIO $ procureInstance chosen_df ps
        case maybe_ginstance of
            Nothing -> return ()
            Just ginstance ->
                whenLoggedIn user $ \name -> do
                    maybe_subscription <- liftIO $ subscribe ginstance name
                    case maybe_subscription of
                        Left _ -> return ()
                        Right subscription -> gameLoop user ps subscription

    pairify :: J.Value -> J.Value -> J.Value
    pairify a b = J.Array $ fromList [a, b]

    receiveChoice :: WebSockets DftermProto (Maybe Int)
    receiveChoice = do
        Text msg <- receiveDataMessage
        return $ case B.head (BL.toStrict msg) of
            1 -> Just $ read $ BU.toString (B.tail (BL.toStrict msg))
            _ -> Nothing

gameLoop :: User
         -> Storage
         -> GameSubscription DwarfFortressPersistent
         -> WebSockets DftermProto ()
gameLoop user ps subscription = do
    sink <- getSink

    tid <- liftIO $
        forkIOWithUnmask $ \unmask ->
            unmask (gameEventHandler subscription sink True Nothing)

    -- Haxory to make sure those two above threads can be killed.
    finref <- liftIO $ newFinalizableFinRef $
        killThread tid

    liftIO $ updatePlayerList sink subscription

    webSocketHandler user
                     ps
                     subscription
                     (killThread tid)

    liftIO $ touchFinRef finref

updatePlayerList :: Sink DftermProto
                 -> GameSubscription DwarfFortressPersistent
                 -> IO ()
updatePlayerList sink subscription = do
    names <- getSubscriberNames' subscription
    sendSink sink $ DataMessage $ Binary $ BL.singleton 2 `BL.append`
        J.encode (fromList $ (J.String "player_list"):
                             (fmap J.String $ S.toList names))

gameEventHandler :: GameSubscription DwarfFortressPersistent
                 -> Sink DftermProto
                 -> Bool
                 -> Maybe T.Text
                 -> IO ()
gameEventHandler subscription sink first last_player = do
    event <- waitForEvent subscription
    case event of
        InstanceClosed -> do
            sendSink sink $ DataMessage $ Binary $ BL.singleton 5
            recurse

        ChatEvent (Joined who) -> do
            sendJSON $
                J.encode (fromList [ J.String "chat_joined"
                                   , J.String who ] )
            recurse

        ChatEvent (Parted who) -> do
            sendJSON $
                J.encode (fromList [ J.String "chat_parted"
                                   , J.String who ] )
            recurse

        ChatEvent (ChatMessage who msg) -> do
            sendJSON $ J.encode (fromList [ J.String "chat"
                                          , J.String who
                                          , J.String msg ])
            recurse

        GameChangesets (DwarfFortressChangesets terminal
                                                changes
                                                new_last_player) -> do
            when (new_last_player /= last_player) $
                case new_last_player of
                    Nothing ->
                        sendJSON $ J.encode
                            (fromList [ J.String "who_is_playing" ] )
                    Just plr ->
                        sendJSON $ J.encode
                            (fromList [ J.String "who_is_playing"
                                      , J.String plr ])

            sendSink sink $ DataMessage $ Binary $ BL.fromStrict $
                if first
                  then encodeStateToBinary terminal
                  else encodeChangesToBinary changes

            gameEventHandler subscription sink False new_last_player
  where
    sendJSON x =
        sendSink sink $ DataMessage $ Binary $ BL.singleton 2 `BL.append` x
    recurse = gameEventHandler subscription sink first last_player


webSocketHandler :: User
                 -> Storage
                 -> GameSubscription DwarfFortressPersistent
                 -> IO ()
                 -> WebSockets DftermProto ()
webSocketHandler user ps subscription killer = do
    Text msg <- receiveDataMessage
    case B.head $ BL.toStrict msg of
        -- Unsubscribe
        2 -> liftIO killer >> stop

        -- Chat
        3 -> do whenLoggedIn user $ \_ ->
                    liftIO $ chat subscription
                                  (T.take 800 $ T.decodeUtf8 $
                                   BL.toStrict $ BL.tail msg)
                recurse

        -- Input
        5 -> inputMsg msg Down      >> recurse
        6 -> inputMsg msg Up        >> recurse
        7 -> inputMsg msg UpAndDown >> recurse

        _ -> recurse
  where
    recurse = webSocketHandler user ps subscription killer
    stop = return ()

    inputMsg msg up_or_down =
        let Just new_input = J.decode $ BL.tail msg 
         in whenLoggedIn user $ \name ->
                liftIO $ input subscription
                               (DwarfFortressInput
                                up_or_down
                                name
                                (new_input :: Input))

-- | Starts the HTTP service. Does not return so you might want to wrap it
-- inside `forkIO`.
--
-- Give the list of ports as the second argument to tell which ports are open
-- for WebSockets.
runWebsocketHTTP :: Word16 -> [Word16] -> IO ()
runWebsocketHTTP port websocket_ports =
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

serializeCellChange :: ((Int, Int), Cell) -> Put
serializeCellChange ((x, y), Cell code fcolor bcolor) = do
    putWord8 (unicodeToCP437 (T.head code))
    putWord8 $ colorSerialize fcolor .|.
               (colorSerialize bcolor `shift` 4)
    putWord16be (fromIntegral x)
    putWord16be (fromIntegral y)
  where
    colorSerialize :: ANSIColor -> Word8
    colorSerialize color = fromIntegral $
                           let intcolor = colorToInt color
                            in if intcolor == 16 then colorToInt White
                                                 else intcolor

putScreenDataPrefix :: Int -> Int -> Int -> Put
putScreenDataPrefix w h num_cells = do
    putWord16be $ fromIntegral w
    putWord16be $ fromIntegral h
    putWord32be $ fromIntegral num_cells

encodeStateToBinary :: Terminal -> B.ByteString
encodeStateToBinary terminal =
    runPut $ do
        putWord8 1
        putScreenDataPrefix w h (w*h)
        forM_ (tuckBounds left top $ assocs (terminal^.gridArray))
              serializeCellChange
  where
    ((left, top), _) = bounds $ terminal^.gridArray
    w = width terminal
    h = height terminal

encodeChangesToBinary :: TerminalChanges -> B.ByteString
encodeChangesToBinary (w, h, _, cell_changes) =
    runPut $ do
        putWord8 1
        putScreenDataPrefix w h num_changes
        forM_ cell_changes serializeCellChange
  where
    num_changes = length cell_changes

