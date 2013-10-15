{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Dfterm3.GameSubscription.Internal.SubscriptionIO
    ( SubscriptionIO(..)
    , runSubscriptionIO
    , useAcidState )
    where

import Dfterm3.GameSubscription.Internal.Types
import Dfterm3.Dfterm3State.Internal.Types

import Data.IORef
import Data.Acid
import Data.Typeable ( Typeable )
import Control.Lens
import Control.Concurrent.MVar
import Control.Monad.State
import Control.Applicative

-- | Actions that require reading or changing values on the disk live in the
-- `SubscriptionIO` monad.
newtype SubscriptionIO a =
    SubscriptionIO (StateT ( SubscriptionStateVolatile
                           , AcidState PersistentStorageState ) IO a)
    deriving ( Monad, MonadIO, MonadFix, Applicative, Functor, Typeable )

-- | Runs `SubscriptionIO` actions.
--
-- These actions can be issued concurrently; however internally there is a lock
-- that prevents actions from running at the same time.
runSubscriptionIO :: Storage -> SubscriptionIO a -> IO a
runSubscriptionIO (Storage (pss, ref)) (SubscriptionIO action) = do
    insides' <- readIORef ref
    let insides = _gameSubscriptionsVolatile insides'
    withMVar (_lock insides) $ \_ -> do
        ( result, new_state ) <- runStateT action ( insides, pss )
        atomicModifyIORef' ref $ \old ->
            ( set gameSubscriptionsVolatile (fst new_state) old, () )
        return result

useAcidState :: SubscriptionIO (AcidState PersistentStorageState)
useAcidState = SubscriptionIO $ use _2

