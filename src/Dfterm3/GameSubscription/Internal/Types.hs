-- | Internal module to `Dfterm3.GameSubscription`. Implements data types.
--

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Dfterm3.GameSubscription.Internal.Types
    (
      SubscriptionStatePersistent(..)
    , SubscriptionStateVolatile(..)
    , PublishableGame(..)
    , GameInstance(..)
    , GameSubscription(..)
    , ChatEvent(..)
    , AnyGameInstance(..)
    , Procurement(..)

    , initialSubscriptionStatePersistent

    , publishedGames
    , runningInstances
    , runningInstancesByGameKey
    , gameInstance
    , chatBroadcastChannel
    , inputsInstanceChannel
    , outputsInstanceChannel
    , subscribeLock
    , onlineUsers
    , gameKey
    , lock
    )
    where

import Data.SafeCopy
import Data.IORef
import Control.Lens
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Typeable ( Typeable )

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Class of publishable games.
--
-- Minimum implementation: `uniqueKey`, `uniqueInstanceKey`, `gameName`,
-- `procureInstance_`, `stopInstance`, `uniqueGameWideKey`.
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
    uniqueKey :: game -> B.ByteString

    -- | Returns a uniquely identifying key from a game type.
    --
    -- This differs from `uniqueKey` in that this key should be the same for
    -- all values that have the same `game` type.
    --
    -- The first argument is not evaluated.
    uniqueGameWideKey :: game -> B.ByteString

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
    procureInstance_ :: game -> IO (Procurement game)

data Procurement game =
    LaunchedNewInstance ( GameRawInstance game
                        , GameInstance game -> IO () )
  | ShareWithInstance (GameInstance game)
  | Failed
  deriving ( Typeable )

-- | An instance of a game.
data GameInstance game =
    GameInstance { _gameInstance :: GameRawInstance game
                 , _gameKey :: B.ByteString
                 , _onlineUsers :: IORef (M.Map T.Text Int)
                 , _inputsInstanceChannel :: TChan (GameInputs game)
                 , _outputsInstanceChannel
                     :: TChan (Maybe (GameChangesets game))
                 , _chatBroadcastChannel :: TChan ChatEvent
                 , _subscribeLock :: MVar Bool }

-- | An event that relates to chatting.
data ChatEvent = Joined T.Text              -- ^ Someone joined the chat. The
                                            -- text is the nickname.
               | Parted T.Text              -- ^ Someone left the chat.
               | ChatMessage T.Text T.Text  -- ^ Someone talked in the chat.
                                            --   The first text is the
                                            --   nickname, the second is the
                                            --   message.
               deriving ( Eq, Ord, Show, Read, Typeable )

data GameSubscription game =
    GameSubscription { _inputsChannel :: TChan (GameInputs game)
                     , _outputsChannel :: TChan (Maybe (GameChangesets game))
                     , _chatChannel :: TChan ChatEvent
                     , _chatReceivingChannel :: TChan ChatEvent
                     , _subscriberGameInstance :: GameInstance game
                     , _name :: T.Text
                     , _ref :: IORef () }

data SubscriptionStatePersistent =
    SubscriptionStatePersistent
    { _publishedGames :: M.Map B.ByteString (B.ByteString, B.ByteString) }

initialSubscriptionStatePersistent :: SubscriptionStatePersistent
initialSubscriptionStatePersistent = SubscriptionStatePersistent M.empty

data AnyGameInstance = AnyGameInstance (IO ())
                                       (IO ())

data SubscriptionStateVolatile =
    SubscriptionStateVolatile
    { _runningInstances :: M.Map B.ByteString AnyGameInstance
    , _runningInstancesByGameKey :: M.Map B.ByteString (S.Set B.ByteString)
    , _lock :: MVar () }
makeLenses ''SubscriptionStateVolatile
makeLenses ''GameInstance

makeLenses ''SubscriptionStatePersistent
deriveSafeCopy 0 'base ''SubscriptionStatePersistent

