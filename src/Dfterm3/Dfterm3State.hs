-- | Module that consolidates persistent and volatile storage for various
-- components.
--

{-# LANGUAGE DeriveDataTypeable, Rank2Types #-}

module Dfterm3.Dfterm3State
    ( openStorage
    , Storage() )
    where

import Dfterm3.GameSubscription.Internal.Types
import Dfterm3.Admin.Internal.Types
import Dfterm3.Dfterm3State.Internal.Types
import Dfterm3.Dfterm3State.Internal.Transactions
import System.Directory
import System.FilePath
import Control.Concurrent.MVar
import Data.Acid
import Data.IORef

import qualified Data.Map as M

openStorage :: FilePath -> IO Storage
openStorage directory = do
    lock <- newMVar ()
    ref <- newIORef $ VolatileStorageState
        { _gameSubscriptionsVolatile =
          SubscriptionStateVolatile M.empty M.empty lock }

    createDirectoryIfMissing True directory
    st <- openLocalStateFrom directory $
          PersistentStorageState
          { _gameSubscriptions = initialSubscriptionStatePersistent
          , _admin = initialAdminStatePersistent }

    return $ Storage ( st, ref )

