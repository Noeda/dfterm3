{-# LANGUAGE RecordWildCards, ImpredicativeTypes, RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Dfterm3.Game
    (
      PublishableGame(..)
    , Procurement(..)
    , GameEvent(..)
    , GameAction(..)
    , GameSubscription()
    , TextGame(..)
    , GameKey
    , GameWideKey
    , writeAction
    , readEvent
    , isSubscriptionAlive
    , newGameSubscription
    , morphGameSubscription
    , stmMorphGameSubscription
    )
    where

import Dfterm3.Prelude
import Dfterm3.Game.TextGame
import Data.SafeCopy
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.ByteString as B

type GameWideKey = B.ByteString
type GameKey = B.ByteString

-- | Class of publishable games.
--
-- The type variable is:
--
--   * /game/ - Type of the game. It has to be serializable with `SafeCopy`
--   because this is saved on the disk. It should contain information on how to
--   launch the game and possibly other game-specific information that should
--   be saved.
--
-- Subscribers should not directly use any of the methods defined here.
--
class (Typeable game, SafeCopy game) => PublishableGame game where
    -- | The type of instances of this game.
    data GameRawInstance game :: *
    -- | The type of inputs this game takes. Subscribers can give input to the
    -- game and this type is used to communicate the input.
    data GameInputs game :: *
    -- | The type of changesets this game takes. Values of this type are used
    -- to communicate game events and changes to the subscribers.
    data GameChangesets game :: *

    -- | Returns a uniquely identifying key from a game value.
    --
    -- Make sure the name doesn't clash with anything. If games of two
    -- different types have the same key, bad things could happen.
    uniqueKey :: game -> GameKey

    -- | Returns a uniquely identifying key from a game type.
    --
    -- This differs from `uniqueKey` in that this key should be the same for
    -- all values that have the same `game` type.
    --
    -- The first argument is not evaluated.
    uniqueGameWideKey :: game -> GameWideKey

    -- | Returns a uniquely identifying key from a game instance value.
    --
    -- The keys need to be unique within the same game.
    uniqueInstanceKey :: GameRawInstance game -> B.ByteString

    -- | Given an instance, stop the instance.
    --
    -- If possible, it may be a good idea to save progress but the caller is
    -- going to expect that from its perspective, the instance is gone.
    stopInstance :: GameRawInstance game -> IO ()

    -- | Returns a name that describes, in human-readable way, what game this
    -- is.
    --
    -- This is purely for cosmetic purposes as this name is typically shown in
    -- the game lists for the end user.
    gameName :: game -> T.Text

    -- | Returns a list of games that could be published.
    --
    -- This is meant to inform the publishing system of games that it does not
    -- know about yet. For example, in the case of Dwarf Fortress, it can scour
    -- Dwarf Fortress processes on the computer and create corresponding games.
    --
    -- This operation does not make sense for all types of games so the default
    -- implementation returns an empty list.
    --
    -- User code should call `lookForPotentialGames`.
    lookForGames :: IO [game]
    lookForGames = return []

    -- | Procures an instance of a game.
    procureInstance_ :: game
                     -> TChan (GameEvent (GameChangesets game))
                     -> TChan (GameAction (GameInputs game))
                     -> IO (Procurement game)

data GameEvent changesets
    = GameDied
    | Changesets changesets
    deriving ( Eq, Ord, Show, Read, Typeable, Functor )

data GameAction action
    = InputAction action
    deriving ( Eq, Ord, Show, Read, Typeable, Functor )

data Procurement game =
    Instance (GameRawInstance game)
  | Failed
  deriving ( Typeable )

data GameSubscription input changesets = MkGameSubscription
    { actionChannel :: !(GameAction input -> STM ())
    , eventChannel :: !(STM (GameEvent changesets))
    , subscriptionAlive :: !(TVar Bool) }
    deriving ( Typeable )

isSubscriptionAlive :: GameSubscription input changesets -> STM Bool
isSubscriptionAlive = readTVar . subscriptionAlive

morphGameSubscription :: (b -> a)
                      -> (c -> d)
                      -> GameSubscription a c
                      -> GameSubscription b d
morphGameSubscription ba cd old_subscription = old_subscription
    { actionChannel = \ga -> actionChannel old_subscription (fmap ba ga)
    , eventChannel = fmap (fmap cd) (eventChannel old_subscription) }

stmMorphGameSubscription :: (b -> STM (Maybe a))
                         -> (STM (GameEvent c) -> STM (GameEvent d))
                         -> GameSubscription a c
                         -> GameSubscription b d
stmMorphGameSubscription ba cd old_subscription = old_subscription
    { actionChannel = \ga -> do
        result <- case ga of
            InputAction action -> ba action
        case result of
            Just ac -> actionChannel old_subscription (InputAction ac)
            Nothing -> return ()
    , eventChannel = do
        cd (eventChannel old_subscription)
    }

-- | Creates a new game subscription.
newGameSubscription :: (GameAction input -> STM ())
                    -> (STM (GameEvent changesets))
                    -> IO (GameSubscription input changesets)
newGameSubscription ac ec = do
    alive <- newTVarIO True
    return $ MkGameSubscription
        { actionChannel = ac
        , eventChannel = ec
        , subscriptionAlive = alive }

writeAction :: GameAction action
            -> GameSubscription action changesets
            -> STM ()
writeAction action (MkGameSubscription {..}) = do
    var <- readTVar subscriptionAlive
    if var
      then actionChannel action
      else return ()

readEvent :: GameSubscription game changesets
          -> STM (Maybe (GameEvent changesets))
readEvent (MkGameSubscription {..}) = do
    var <- readTVar subscriptionAlive
    if var
      then orElse (Just <$> eventChannel) (pure Nothing)
      else return Nothing
                -- we could drop the Maybe and retry instead but I think by
                -- using Maybe we'll lessen the chance someone deadlocks
                -- themselves unintentionally

class PublishableGame game => TextGame game where
    toTextGameInput :: GameInputs game -> TextGameInput
    fromTextGameInput :: TextGameInput -> GameInputs game

    toTextGameChangesets :: GameChangesets game -> TextGameChangesets
    fromTextGameChangesets :: TextGameChangesets -> GameChangesets game

