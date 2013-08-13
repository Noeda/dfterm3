-- | All sorts of functions that have no clear place where else they could be.
--

module Dfterm3.Util
    ( whenJust
    , touchIORef
    , newFinalizableIORef
    , forkDyingIO )
    where

import Data.IORef
import Control.Monad ( void )
import Control.Concurrent
import Control.Exception

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) action = action x

touchIORef :: IORef a -> IO ()
touchIORef ref = ref `seq` return ()
{-# NOINLINE touchIORef #-}

newFinalizableIORef :: a -> IO () -> IO (IORef a)
newFinalizableIORef value finalizer = do
    ref <- newIORef value
    void $ mkWeakIORef ref finalizer
    return ref

-- | Launches a thread that will be killed when the given action finishes.
forkDyingIO :: IO ()        -- ^ Computation to run in thread.
            -> IO a         -- ^ Computation to run after forking.
            -> IO a
forkDyingIO thread_action action = mask $ \restore -> do
    tid <- forkIOWithUnmask $ \unmask -> unmask thread_action
    finally (restore action) (killThread tid)

