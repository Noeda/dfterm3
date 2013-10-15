-- | Internal module to Dfterm3.Dfterm3State
--

{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Dfterm3.Dfterm3State.Internal.Types
    (
      Storage(..)
    , PersistentStorageState(..)
    , VolatileStorageState(..)
    , gameSubscriptions
    , gameSubscriptionsVolatile
    , admin
    , loggedInUsers
    , readPersistentStorage
    , readVolatileStorage
    , modifyVolatileStorage
    , modifyVolatileStorage'
    )
    where

import Control.Lens
import Data.Typeable ( Typeable )
import Data.Acid
import Data.IORef
import Data.SafeCopy
import Dfterm3.GameSubscription.Internal.Types
import Dfterm3.Admin.Internal.Types

import qualified Data.Set as S
import qualified Data.Text as T

data PersistentStorageState =
    PersistentStorageState
    { _gameSubscriptions :: SubscriptionStatePersistent
    , _admin :: AdminStatePersistent }
    deriving ( Typeable )

data VolatileStorageState =
    VolatileStorageState
    { _gameSubscriptionsVolatile :: SubscriptionStateVolatile
    , _loggedInUsers :: S.Set T.Text }
    deriving ( Typeable )
makeLenses ''PersistentStorageState
makeLenses ''VolatileStorageState
deriveSafeCopy 0 'base ''PersistentStorageState

-- | Handle to Dfterm3 state.
newtype Storage =
    Storage ( AcidState PersistentStorageState, IORef VolatileStorageState )
    deriving ( Typeable )

readPersistentStorage :: Storage -> AcidState PersistentStorageState
readPersistentStorage (Storage (persistent, _)) = persistent

readVolatileStorage :: Storage -> IO VolatileStorageState
readVolatileStorage (Storage (_, ref)) = readIORef ref

modifyVolatileStorage' :: Storage
                       -> (VolatileStorageState -> VolatileStorageState)
                       -> IO ()
modifyVolatileStorage' (Storage (_, ref)) modifier =
    atomicModifyIORef' ref $ \old -> ( modifier old, () )

modifyVolatileStorage :: Storage
                      -> (VolatileStorageState -> ( VolatileStorageState, b ) )
                      -> IO b
modifyVolatileStorage (Storage (_, ref)) =
    atomicModifyIORef' ref

