-- | This module works with `Dfterm3.WebsocketAccepter`. This is the code that
-- talks to the client on the other side of the WebSocket connection.
--

{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns #-}

module Dfterm3.WebsocketClient
    ( websocketClient )
    where

import Dfterm3.GamePool
import Dfterm3.CP437Game
import Dfterm3.DwarfFortress.Types
import Dfterm3.DwarfFortress
import Dfterm3.User
import Dfterm3.Util
import Dfterm3.Dfterm3Monad
import Dfterm3.UserVolatile
import Dfterm3.ChatChannel

import qualified Data.Aeson as J

import System.IO
import Network.WebSockets
import Control.Concurrent
import Control.Monad
import Control.Lens
import Control.Monad.Reader
import Control.Applicative ( (<$>) )
import Control.Exception
import Data.Word
import Data.Bits
import Data.Array
import Data.Vector ( fromList )
import Data.Enumerator hiding ( length, filter )
import Data.Enumerator.Binary hiding ( zipWith, filter )
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- Probably should change to `Rfc6455` when (or if) that becomes available in
-- the websockets library.
type DftermProto = Hybi10

type Dfterm3TWS a = Dfterm3ClientT
                        (ReaderT Handle (WebSockets DftermProto)) a

liftWS :: WebSockets DftermProto a -> Dfterm3TWS a
liftWS = lift . lift

getHandle :: Dfterm3TWS Handle
getHandle = lift ask

websocketClient :: GamePool -> UserSystem -> UserVolatile -> Handle -> IO ()
websocketClient pool us uv handle = do
    let enumerator = enumHandle 8192 handle
    run_ $ enumerator $$
           runWebSocketsHandshake True ar (iterHandle handle)
  where
    ar :: Request -> WebSockets DftermProto ()
    ar request = acceptRequest request >>
                 runReaderT (runDfterm3ClientT client
                                               Nothing
                                               pool
                                               us
                                               uv)
                            handle

client :: Dfterm3TWS ()
client = forever chooseYourGame

-- | Sends a list of games to the client.
chooseYourGame :: Dfterm3TWS ()
chooseYourGame = do
    (games, instances) <- unzip <$> withGamePool
                              (liftIO . enumerateRunningGames')
    dfs <- withUserSystem $ liftIO . listDwarfFortresses

    let running_executables = fmap (^. (df . dfExecutable)) games
        playable_games = filter (\x -> x ^. dfExecutable `elem`
                                            running_executables)
                                dfs

    let bs = J.encode (fromList
            [ J.String $ T.pack "game_list"
            , J.Array $ fromList $
                zipWith pairify
                    (fmap (J.String . (^. dfName)) playable_games)
                    (fmap J.toJSON [(0::Int)..]) ] )

    liftWS $ sendBinaryData $ BL.singleton 2 `BL.append` bs

    maybe_choice <- receiveChoice
    case maybe_choice of
        Nothing -> chooseYourGame
        Just choice -> do
            maybe_client <- liftIO $ playGame (instances !! choice)
            case maybe_client of
                Nothing -> chooseYourGame
                Just client -> gameLoop client
  where
    pairify :: J.Value -> J.Value -> J.Value
    pairify a b = J.Array $ fromList [a, b]

    receiveChoice :: Dfterm3TWS (Maybe Int)
    receiveChoice = liftWS $ do
        Text msg <- receiveDataMessage
        return $ case B.head (BL.toStrict msg) of
            1 -> Just $ read $ BU.toString (B.tail (BL.toStrict msg))
            _ -> Nothing

-- | This loop sends out CP437 changes to the client, if a game has been
-- selected. It returns when the game disappears.
gameLoop :: DwarfFortressClient
         -> Dfterm3TWS ()
gameLoop game_client = do
    sink <- liftWS getSink
    handle <- getHandle

    listener <- liftIO $ registerAsListener (gameClientChannel game_client)

    (tid, tid2, ref) <- liftIO $ mask_ $ do
        tid <- forkIOWithUnmask $ \restore -> restore $
                                      updateLoop sink handle game_client True
        tid2 <- forkIOWithUnmask $ \restore -> restore $
                                      chatLoop sink listener
        ref <- newFinalizableIORef () $ killThread tid >> killThread tid2
        return (tid, tid2, ref)

    inputLoop tid tid2 ref handle
  where
    inputLoop tid tid2 ref handle = do
        Text msg <- liftWS receiveDataMessage
        case B.head $ BL.toStrict msg of
            2 -> do liftIO $ killThread tid >> killThread tid2
                    liftIO $ touchIORef ref
                    chooseYourGame
            3 -> do whenLoggedIn $ \(userName -> name) ->
                        liftIO $ chat name
                                    (T.take 800 $ T.decodeUtf8 $
                                     BL.toStrict $ BL.tail msg)
                                    (gameClientChannel game_client)
                    inputLoop tid tid2 ref handle
            4 -> let name = T.take 20 $
                                T.decodeUtf8 $ BL.toStrict $ BL.tail msg
                  in do maybe_user <- loginM name
                        liftWS $ sendBinaryData $ case maybe_user of
                            Nothing -> BL.singleton 3
                            Just  _ -> BL.singleton 4
                        inputLoop tid tid2 ref handle
            _ -> inputLoop tid tid2 ref handle

    chatLoop sink listener = forever $ do
        (user_name, msg) <- listen listener

        let bs = J.encode (fromList [ J.String $ T.pack "chat"
                                    , J.String user_name
                                    , J.String msg ])

        sendSink sink $ DataMessage $ Binary $ BL.singleton 2 `BL.append` bs

    updateLoop sink handle game_client first = do
        msg <- liftIO $ receiveGameUpdates game_client
        case msg of
            GameUnregistered -> return ()
            Message (_game -> new_state) (CP437 changes) -> do
                sendSink sink $ DataMessage $ Binary $ BL.fromStrict $ if first
                  then encodeStateToBinary new_state
                  else encodeChangesToBinary changes
                liftIO $ hFlush handle
                updateLoop sink handle game_client False
            _ -> error "Unexpected message from Dwarf Fortress game."

serializeCellChange :: ((Int, Int), CP437Cell) -> Put
serializeCellChange ((x, y), CP437Cell code fcolor bcolor) = do
    putWord8 code
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

encodeStateToBinary :: CP437Game -> B.ByteString
encodeStateToBinary game =
    BL.toStrict $ runPut $ do
        putWord8 1
        putScreenDataPrefix w h (w*h)
        forM_ (tuckBounds left top $ assocs (game^.cp437Array))
              serializeCellChange
  where
    ((left, top), _) = bounds $ game^.cp437Array
    w = cp437Width game
    h = cp437Height game

encodeChangesToBinary :: CP437Changes -> B.ByteString
encodeChangesToBinary (w, h, cell_changes) =
    BL.toStrict $ runPut $ do
        putWord8 1
        putScreenDataPrefix w h num_changes
        forM_ cell_changes serializeCellChange
  where
    num_changes = length cell_changes

