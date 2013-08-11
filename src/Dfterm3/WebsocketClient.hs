-- | This module works with `Dfterm3.WebsocketAccepter`. This is the code that
-- talks to the client on the other side of the WebSocket connection.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dfterm3.WebsocketClient
    ( websocketClient )
    where

import Dfterm3.GamePool
import Dfterm3.CP437Game
import Dfterm3.DwarfFortress.Types
import Dfterm3.User

import qualified Data.Aeson as J

import System.IO
import Network.WebSockets
import Control.Concurrent
import Control.Monad
import Control.Lens
import Control.Monad.Reader
import Control.Applicative ( (<$>) )
import Data.List
import Data.Maybe
import Data.Word
import Data.Bits
import Data.Array
import Data.Vector ( fromList )
import Data.Enumerator hiding ( length )
import Data.Enumerator.Binary
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- Probably should change to `Rfc6455` when (or if) that becomes available in
-- the websockets library.
type DftermProto = Hybi10

-- We carry around a handle, a gamepool and a user system all over so it makes
-- sense to monadify that.
newtype Client a =
    Client
    { runClient ::
        ReaderT (GamePool, Handle, UserSystem) (WebSockets DftermProto) a }
    deriving ( Monad, MonadIO, MonadReader (GamePool, Handle, UserSystem)
             , Functor )

-- Convenience functions to extract the stuff from the above monad
withGamePool :: (GamePool -> Client a) -> Client a
withGamePool action = do
    (pool, _, _) <- ask
    action pool

withHandle :: (Handle -> Client a) -> Client a
withHandle action = do
    (_, handle, _) <- ask
    action handle

withUserSystem :: (UserSystem -> Client a) -> Client a
withUserSystem action = do
    (_, _, us) <- ask
    action us

liftWS :: WebSockets DftermProto a -> Client a
liftWS = Client . lift

websocketClient :: GamePool -> UserSystem -> Handle -> IO ()
websocketClient pool us handle = do
    let enumerator = enumHandle 8192 handle
    run_ $ enumerator $$
           runWebSocketsHandshake True ar (iterHandle handle)
  where
    ar :: Request -> WebSockets DftermProto ()
    ar request = acceptRequest request >>
                 runReaderT (runClient client) (pool, handle, us)

client :: Client ()
client = do
    -- Currently at "select game" phase. Send the game list to the client and
    -- wait until a game is selected.
    game <- chooseYourGame
    return ()

-- | Sends a list of games to the client. Update every 10 seconds.
chooseYourGame :: Client ()
chooseYourGame = do
    (maybe_states, _) <- unzip <$> withGamePool (liftIO . enumerateGames)
    dfs <- withUserSystem $ liftIO . listDwarfFortresses

    let states = catMaybes maybe_states
        actual_games = unionBy (\x y -> x^.dfExecutable == y^.dfExecutable)
                               dfs (fmap _df states)

    let bs = J.encode (fromList
            [ J.String $ T.pack "game_list"
            , J.Array $ fromList (fmap (J.String . (^. dfName)) actual_games) ])
    liftWS $ sendBinaryData $ BL.singleton 2 `BL.append` bs

    -- TODO: have GamePool directly inform of any new games and removed games
    -- immediately instead of polling every 10 seconds.
    liftIO $ threadDelay 10000000

-- | This loop sends out CP437 changes to the client, if a game has been
-- selected. It returns when the game disappears.
gameLoop :: GameClient CP437Game () CP437Changes
         -> Bool
         -> Client ()
gameLoop game_client first = do
    msg <- liftIO $ receiveGameUpdates game_client
    (_, handle, _) <- ask
    case msg of
        GameUnregistered -> return ()
        Message new_state changes -> do
            liftWS $ sendBinaryData $ if first
              then encodeStateToBinary new_state
              else encodeChangesToBinary changes
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

