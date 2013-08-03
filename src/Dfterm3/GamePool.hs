-- | This module implements whatever you need to track and manage a bunch of
-- games running.

-- TODO: does not scale well if there are many games running. Whenever a games
-- sends updates (which can be 60 times per second or even more) a lock is
-- held, preventing updates from being sent from other games.
--
-- Each `withGamePool` takes the lock. The most used functions could be
-- reworked into not needing this lock.

{-# LANGUAGE DeriveDataTypeable, GADTs, ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}

module Dfterm3.GamePool
    (
    -- * The game pool
      newGamePool
    , GamePool()
    -- * CP437 games
    -- ** Creation, registration
    , newCP437Game
    , registerCP437Game
    , registerCP437Watcher
    , unregisterCP437Watcher
    -- ** Data types
    , CP437Code
    , CP437Game()
    -- ** Lenses
    , cp437Name
    , cp437Array
    -- ** Views
    , cp437Width
    , cp437Height
    , cp437Dimensions )
    where

import Data.IORef
import Data.Word
import Data.Array
import Data.Typeable
import Data.Foldable
import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar
import Control.Lens hiding ( indices )
import Control.Applicative ( (<$>) )
import Control.Monad.State hiding ( forM_ )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

newtype GamePool = GamePool (MVar GamePoolI)
                   deriving ( Eq, Typeable )

data GamePoolI = GamePoolI
    { _games :: M.Map GameID (S.Set WatcherID, IORef CP437Game)
    , _cp437watchers :: M.Map WatcherID ( GameID
                                        , IORef Bool
                                        , Maybe CP437Changes -> IO () )
    , _nextID :: !Integer }

-- Avoid template haskell
-- makeLenses ''GamePoolI
games :: Lens' GamePoolI (M.Map GameID (S.Set WatcherID, IORef CP437Game))
games = lens _games (\gamepool new_games -> gamepool { _games = new_games })
nextID :: Lens' GamePoolI Integer
nextID = lens _nextID (\gamepool new_id -> gamepool { _nextID = new_id })
cp437watchers :: Lens' GamePoolI
                       (M.Map WatcherID ( GameID
                                        , IORef Bool
                                        , Maybe CP437Changes -> IO ()))
cp437watchers = lens _cp437watchers (\gamepool new_watchers ->
                               gamepool { _cp437watchers = new_watchers })

---------------------------------------
-- The public interface (mostly)
---------------------------------------

-- | CP437 characters conveniently fit in 8 bits.
type CP437Code = Word8
-- | A type synonym for a tuple that represents changes in CP437 represented
-- game. The first two integers are new width and height and the list is a list
-- of coordinates where cells have changed and their contents.
type CP437Changes = (Int, Int, [((Int, Int), CP437Cell)])

-- | These are used as a unique IDs within a `GamePool` and they refer to
-- individual instances of games that are running (as opposed to games in
-- general).
newtype GameID = GameID Integer
                 deriving ( Eq, Ord, Show, Typeable )

-- | These IDs refer to watchers.
newtype WatcherID = WatcherID Integer
                    deriving ( Eq, Ord, Show, Typeable )

-- | The 16 colors that appear in ANSI terminals.
--
-- There are actually 17 colors here, of which `Default` is really some other
-- color.
data ANSIColor = Black | Red | Green | Yellow | Blue | Magenta | Cyan |
                 White | Default | BrightBlack | BrightRed | BrightGreen |
                 BrightYellow | BrightBlue | BrightMagenta | BrightCyan |
                 BrightWhite
                 deriving ( Eq, Show, Ord, Read, Typeable )

-- | Represents what is inside one cell in a CP437 game.
data CP437Cell = CP437Cell {-# UNPACK #-} !CP437Code  -- ^ The characte code.
                           !ANSIColor  -- ^ Foreground.
                           !ANSIColor  -- ^ Background.
                 deriving ( Eq, Show, Ord, Read, Typeable )

-- | This data type is a game whose output can be wholly represented by a bunch
-- of CP437 encoded characters, with foregrounds and backgrounds.
data CP437Game = CP437Game { _cp437Name :: !T.Text
                           , _cp437Array :: !(Array (Int, Int) CP437Cell) }
                 deriving ( Eq, Show, Ord, Read, Typeable )

-- | Creates a new game pool.
newGamePool :: IO GamePool
newGamePool =
    GamePool <$> newMVar GamePoolI { _games = M.empty
                                   , _cp437watchers = M.empty
                                   , _nextID = 0 }

-- | Construct a `CP437Game`.
newCP437Game :: T.Text            -- ^ Name that identifies the game.
                                  --   Should refer to the game in general.
             -> Array (Int, Int) CP437Cell
                                  -- ^ Array of the cells.
             -> CP437Game
newCP437Game = CP437Game

-- | Lens to the name of a CP437 game.
cp437Name :: Lens' CP437Game T.Text
cp437Name = lens _cp437Name (\game new_name -> game { _cp437Name = new_name })
-- | Lens to the array in a CP437 game.
cp437Array :: Lens' CP437Game (Array (Int, Int) CP437Cell)
cp437Array = lens _cp437Array (\game new_arr -> game { _cp437Array = new_arr })

-- | Returns the width and height of a `CP437Game` as a tuple.
cp437Dimensions :: CP437Game -> (Int, Int)
cp437Dimensions game =
    let ((left, top), (right, bottom)) = bounds $ _cp437Array game
     in (max (right-left+1) 0, max (bottom-top) 0)

-- | Returns the width of a `CP437Game`.
cp437Width :: CP437Game -> Int
cp437Width = fst . cp437Dimensions

-- | Returns the height of a `CP437Game`.
cp437Height :: CP437Game -> Int
cp437Height = snd . cp437Dimensions

obtainNextGameID :: GamePoolM GameID
obtainNextGameID = do
    id <- use nextID
    nextID %= (+1)
    return $ GameID id

obtainNextWatcherID :: GamePoolM WatcherID
obtainNextWatcherID = do
    id <- use nextID
    nextID %= (+1)
    return $ WatcherID id

-- | Tells the game pool that some watcher now would like to watch a
-- CP437-based game.
--
-- Due to race conditions, it is possible that the `GamePoolID` you give to
-- watch a game has expired by the time you call this function. In that case,
-- `Nothing` is returned and this function does nothing.
registerCP437Watcher :: GamePool
                     -> GameID
                     -> (Maybe CP437Changes -> IO ())
                     -- ^ You are informed of changes through this function you
                     -- give to the game pool. The changes may be `Nothing` in
                     -- which case it means the game was closed and you are
                     -- being unregistered from being watching.
                     -> IO (Maybe WatcherID)
registerCP437Watcher pool game_id receiver = withGamePool pool $ do
    has_game <- M.member game_id <$> use games
    if has_game
      then registerCP437Watcher'
      else return Nothing
  where
    registerCP437Watcher' = do
        wid <- obtainNextWatcherID
        ( unregister_ref, protected ) <- liftIO unregisterProtected

        cp437watchers %= M.insert wid ( game_id
                                      , unregister_ref
                                      , protected )

        games %= M.adjust (over _1 (S.insert wid)) game_id
        return $ Just wid

    unregisterProtected = do
        unregister_ref <- newIORef False
        return ( unregister_ref
               , \changes -> do val <- readIORef unregister_ref
                                unless val $ receiver changes )

-- | If you are a watcher and would like to cease watching a CP437-based game,
-- this function is your friend. Give it the `GamePoolID` you received as a
-- return value from `registerCP437Watcher`.
--
-- Does nothing if watcher was not registered anyway.
unregisterCP437Watcher :: GamePool -> WatcherID -> IO ()
unregisterCP437Watcher pool = withGamePool pool . rawUnregisterCP437Watcher

-- | Tells the game pool that there is a CP437 game that could be watched.
registerCP437Game :: GamePool
                  -> CP437Game
                  -> IO (Maybe CP437Game -> IO ())
                  -- ^ You can use the returned value to keep the game pool
                  -- informed of any updates that have happened in the game.
                  -- This function is safe to use from a thread (and probably
                  -- should be called from a thread). Just pass the new state
                  -- of the game. If you pass `Nothing`, the game will be
                  -- unregistered. It will be bad if you try to call the
                  -- returned value after it has been unregistered.
registerCP437Game pool initial_game = withGamePool pool $ do
    my_id <- obtainNextGameID

    game_ref <- liftIO $ newIORef initial_game
    games %= M.insert my_id ( S.empty, game_ref )

    return $ \maybe_new_game ->
        case maybe_new_game of
            Just new_game -> do
                cp437gameUpdate pool my_id new_game
                atomicModifyIORef' game_ref $ const ( new_game, () )
            Nothing -> unregisterCP437Game pool my_id

----------------------------------
-- Internal functions
----------------------------------

-- | Convenience type synonym for the state thing monad.
type GamePoolM a = StateT GamePoolI IO a

-- | Runs the given `GamePoolM` actions atomically in respect to the game pool
-- state. Highly convenient! Will deadlock(?) or crash(?) if nested.
withGamePool :: GamePool -> GamePoolM a -> IO a
withGamePool (GamePool mvar) state =
    modifyMVar mvar $ \old -> do
        (result, new_state) <- runStateT state old
        return (new_state, result)

-- | "Raw" interface that removes a watcher from the pool and does nothing
-- else.
rawUnregisterCP437Watcher :: WatcherID -> GamePoolM ()
rawUnregisterCP437Watcher wid = do
    is_member <- M.member wid <$> use cp437watchers
    when is_member removeMember
  where
    removeMember = do
        (game_they_watched, unregister_ref, updater) <-
                             M.findWithDefault undefined wid <$>
                             use cp437watchers

        void $ liftIO $ forkIO $ do
            writeIORef unregister_ref True
            updater Nothing

        games %= M.adjust (over _1 (S.delete wid)) game_they_watched
        cp437watchers %= M.delete wid


-- | Unregisters a game from the game pool.
--
-- This is not a public function because it seems like it is not needed.
--
-- Doesn't do anything if the game wasn't part anyway.
unregisterCP437Game :: GamePool -> GameID -> IO ()
unregisterCP437Game pool gid = withGamePool pool $ do
    was_member <- M.member gid <$> use games
    when was_member unregister
  where
    unregister = do
        (watchers_who_watched, _) <-
            M.findWithDefault undefined gid <$> use games
        forM_ watchers_who_watched rawUnregisterCP437Watcher
        games %= M.delete gid


-- | Some updates came up in a CP437 game. This function propagates the changes
-- to all watchers who have registered interest.
--
--
-- TODO: make this not be part of `GamePoolM` to avoid the global game pool
-- lock.
cp437gameUpdate :: GamePool -> GameID -> CP437Game -> IO ()
cp437gameUpdate pool game_id new_game = do
    -- Keep the critical section short
    (g, w) <- withGamePool pool $ do
        g <- use games
        w <- use cp437watchers
        return (g, w)

    let (watcher_ids, game_ref) =
            M.findWithDefault undefined game_id g
        watchers = map (\id -> M.findWithDefault undefined
                                                 id
                                                 w)
                       (S.toList watcher_ids)

    unless (null watchers) $ do
        old_game <- readIORef game_ref

        -- Only inform everyone if something actually changed
        let changes@(_, _, list) = ( cp437Width new_game
                                   , cp437Height new_game
                                   , findCP437Changes old_game new_game )

        when (cp437Width new_game /= cp437Width old_game ||
              cp437Height new_game /= cp437Height old_game ||
              not (null list)) $
           propagateChanges watchers changes

  where
    propagateChanges :: [( GameID
                         , IORef Bool
                         , Maybe CP437Changes -> IO ())]
                     -> CP437Changes
                     -> IO ()
    propagateChanges watchers changes =
        forM_ watchers $ \(_, _, updater) -> updater (Just changes)


-- | Calculates what changes there are between two CP437 games, for the array
-- part.
findCP437Changes :: CP437Game       -- ^ The old game.
                 -> CP437Game       -- ^ The new game.
                 -> [((Int, Int), CP437Cell)]
findCP437Changes (_cp437Array -> old_arr)
                 (_cp437Array -> new_arr) =
    -- If bounds have changed, update everything.
    if bounds old_arr /= bounds new_arr
      then changeEverything
      else findChanges
  where
    changeEverything = assocs new_arr

    findChanges =
        foldl' (\changes coords ->
                   if new_arr ! coords /=
                      old_arr ! coords
                      then (coords, new_arr ! coords):changes
                      else changes)
               []
               (indices old_arr)

