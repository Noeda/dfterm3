-- | This module works with `Dfterm3.WebsocketAccepter`. This is the code that
-- talks to the client on the other side of the WebSocket connection.
--

module Dfterm3.WebsocketClient
    ( websocketClient )
    where

import Dfterm3.GamePool
import Dfterm3.CP437Game

import System.IO
import Network.WebSockets
import Control.Concurrent
import Control.Monad.IO.Class ( liftIO )
import Control.Monad
import Control.Lens
import Data.Word
import Data.Bits
import Data.Array
import Data.Enumerator hiding ( length )
import Data.Enumerator.Binary
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- Probably should change to `Rfc6455` when (or if) that becomes available in
-- the websockets library.
type DftermProto = Hybi10

websocketClient :: GamePool -> Handle -> IO ()
websocketClient pool handle = do
    let enumerator = enumHandle 8192 handle
    run_ $ enumerator $$
           runWebSocketsHandshake True ar (iterHandle handle)
  where
    ar :: Request -> WebSockets DftermProto ()
    ar request = acceptRequest request >> client handle pool

client :: Handle -> GamePool -> WebSockets DftermProto ()
client handle pool = do
    -- Just pick the first game available
    games <- liftIO $ (enumerateGames pool ::
                       IO [GameInstance CP437Game () CP437Changes])
    case games of
        [] -> liftIO (threadDelay 1000000) >> client handle pool
        (game:_) -> do
            maybe_client <- liftIO $ playGame game
            case maybe_client of
                Nothing -> client handle pool
                Just client -> gameLoop client True
  where
    gameLoop :: GameClient CP437Game () CP437Changes
             -> Bool
             -> WebSockets DftermProto ()
    gameLoop game_client first = do
        msg <- liftIO $ receiveGameUpdates game_client
        case msg of
            GameUnregistered -> client handle pool
            Message new_state changes ->
                if first
                  then do sendBinaryData $ encodeStateToBinary new_state
                          liftIO $ hFlush handle
                          gameLoop game_client False
                  else do sendBinaryData $ encodeChangesToBinary changes
                          liftIO $ hFlush handle
                          gameLoop game_client False

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

encodeStateToBinary :: CP437Game -> B.ByteString
encodeStateToBinary game =
    BL.toStrict $ runPut $ do
        putWord16be (fromIntegral w)
        putWord16be (fromIntegral h)
        putWord32be (fromIntegral $ w*h)
        forM_ (tuckBounds left top $ assocs (game^.cp437Array))
              serializeCellChange
  where
    ((left, top), _) = bounds $ game^.cp437Array
    w = cp437Width game
    h = cp437Height game

encodeChangesToBinary :: CP437Changes -> B.ByteString
encodeChangesToBinary (w, h, cell_changes) =
    BL.toStrict $ runPut $ do
        putWord16be (fromIntegral w)
        putWord16be (fromIntegral h)
        putWord32be (fromIntegral num_changes)
        forM_ cell_changes serializeCellChange
  where
    num_changes = length cell_changes

