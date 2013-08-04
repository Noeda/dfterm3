-- | This module implements whatever you need to track and manage a bunch of
-- games running.

{-# LANGUAGE DeriveDataTypeable, GADTs, ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns, FunctionalDependencies, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dfterm3.GamePool
    (
    -- * The game pool
      newGamePool
    , GamePool()
    -- * Game interface
    , Game
    -- ** Creation, registration
    , registerGame
    , unregisterGame
    , enumerateGames
    , GameInstance()
    , GameProvider()
    , GameClient()
    -- ** Game providers
    , updateGame
    -- ** Game clients
    , playGame
    , receiveGameUpdates
    , GameMessage(..) )
    where

import Data.IORef
import Data.Typeable
import Data.Dynamic
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens hiding ( indices )
import Control.Applicative ( (<$>) )
import Control.Monad.State hiding ( forM_ )
import qualified Data.Map.Lazy as M

newtype GamePool = GamePool (MVar GamePoolI)
                   deriving ( Eq, Typeable )

-- | These are used as a unique IDs within a `GamePool` and they refer to
-- individual instances of games that are running (as opposed to games in
-- general).
newtype GameID = GameID Integer
                 deriving ( Eq, Ord, Show, Typeable )

data GamePoolI = GamePoolI { _nextID :: !Integer
                           , _games :: M.Map TypeRep (M.Map GameID Dynamic) }

-- | A game message. The first type variable is the game state type. The second
-- is the changeset type between two states.
data GameMessage a b = GameUnregistered  -- ^ Game was unregistered from the
                                         --   system. No further messages are
                                         --   guaranteed to follow.
                     | Message a b  -- ^ The first is the new state and the
                                    -- second is changesets between previous
                                    -- state and that new state.

data GameInstance a b c = GameInstance
    { writeOutput :: !(TChan (GameMessage a c))
    , receiveInput :: !(TChan b)
    , gone :: !(IORef Bool)
    , unregisterer :: IO () }
    deriving ( Typeable )

newtype GameProvider a b c = GameProvider (GameInstance a b c)
                             deriving ( Typeable )
newtype GameClient a b c = GameClient (TChan (GameMessage a c))
                           deriving ( Typeable )

makeLenses ''GamePoolI

---------------------------------------
-- The public interface (mostly)
---------------------------------------

-- | This class is implemented by games that can be registered to the game pool
-- and watched.
--
-- @
--  The type of the game itself
--       |
--       +---+ +--  The type of the inputs the game can receive.
--           | |
--           | | + The type of the changesets when the game is updated. This is
--           | | | meant to be able to represent small changes between two
--           | | | values of type 'a' but it probably should be able to
--           | | | represent an entire 'a' in case the whole game state needs
--           | | | to be refreshed.
--           | | |
--           v v v
--      Game a b c
-- @
class (Typeable a, Typeable b, Typeable c) => Game a b c | a -> b c where

-- | Creates a new game pool.
newGamePool :: IO GamePool
newGamePool =
    GamePool <$> newMVar GamePoolI { _nextID = 0
                                   , _games = M.empty }

registerGame :: forall a b c. Game a b c
             => GamePool
             -> IO ( GameProvider a b c
                   , GameInstance a b c )
registerGame pool = do
    receiver_chan <- newTChanIO
    sender_chan <- newBroadcastTChanIO
    gid <- withGamePool obtainNextGameID pool
    gone_ref <- newIORef False

    let typ = typeOf (undefined :: a)
        inst = GameInstance { writeOutput = sender_chan
                            , receiveInput = receiver_chan
                            , gone = gone_ref
                            , unregisterer =
                                unregisterMyself gone_ref sender_chan typ gid }
        dyn_inst = toDyn inst

    flip withGamePool pool $ do
        games %= \old_games ->
            if M.member typ old_games
              then M.adjust (M.insert gid dyn_inst) typ old_games
              else M.insert typ (M.singleton gid dyn_inst) old_games

        return (GameProvider inst, inst)
  where
    unregisterMyself gone_ref sender_chan typ gid = do
        flip withGamePool pool $
            games %= M.update (\old_map -> let new_map = M.delete gid old_map
                                            in if M.null new_map
                                                 then Nothing
                                                 else Just new_map)
                              typ

        atomicModifyIORef' gone_ref $ const ( True, () )
        atomically $ writeTChan sender_chan GameUnregistered

unregisterGame :: GameInstance a b c -> IO ()
unregisterGame = unregisterer

-- | Tells everyone that some changes happened in a game.
--
-- This does nothing if the game has been unregistered.
updateGame :: a                     -- ^ The new game state. This is used when
                                    -- a new player joins and needs to know the
                                    -- entire game state at once. This is
                                    -- lazily evaluated.
           -> c                     -- ^ The changesets from the previous
                                    -- state. This is used for players that
                                    -- were already watching and need not to
                                    -- know about every change.
           -> GameProvider a b c
           -> IO ()
updateGame new_state changes (GameProvider inst) = do
    is_gone <- readIORef (gone inst)
    unless is_gone $
        atomically $ writeTChan (writeOutput inst)
                                (Message new_state changes)

-- | Checks if there are any updates on some game.
--
-- This blocks until a message is available. If you receive `GameUnregistered`,
-- then it is no longer guaranteed any messages will arrive (which would make
-- you block forever if you called this again).
receiveGameUpdates :: GameClient a b c -> IO (GameMessage a c)
receiveGameUpdates (GameClient chan) =
    atomically $ readTChan chan

-- | Returns a list of all games of given type that are currently running.
enumerateGames :: forall a b c. Game a b c
               => GamePool -> IO [GameInstance a b c]
enumerateGames = withGamePool $ do
    maybe_games <- M.lookup (typeOf (undefined :: a)) <$>
                   use games
    case maybe_games of
        Nothing -> return []
        Just games ->
            return $ fmap (`fromDyn` undefined) $ M.elems games

-- | Given a game instance, indicate that you want to play it.
--
-- You are given a `GameClient` that you can use to obtain game changesets and
-- (maybe) play.
--
-- You get `Nothing` if the game was not active.
playGame :: GameInstance a b c -> IO (Maybe (GameClient a b c))
playGame inst = do
    my_tchan <- atomically $ dupTChan (writeOutput inst)
    is_gone <- readIORef (gone inst)
    return $ if is_gone
      then Nothing
      else Just $ GameClient my_tchan

-- Internal functions

obtainNextGameID :: GamePoolM GameID
obtainNextGameID = do
    id <- use nextID
    nextID %= (+1)
    return $ GameID id

type GamePoolM a = StateT GamePoolI IO a

-- | Runs the given `GamePoolM` actions atomically in respect to the game pool
-- state. Highly convenient! Will deadlock(?) or crash(?) if nested.
withGamePool :: GamePoolM a -> GamePool -> IO a
withGamePool state (GamePool mvar) =
    modifyMVar mvar $ \old -> do
        (result, new_state) <- runStateT state old
        return (new_state, result)

