-- | Miscellaneous utiliy functions.
--

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, BangPatterns #-}

module Dfterm3.Util
    ( newFinalizableIORef
    , newFinalizableFinRef
    , finalizeFinRef
    , touchFinRef
    , FinRef()
    , forkDyingIO
    , forkExceptionTaggedIO
    , exceptionTaggedIO
    , whenJust
    , whenJustM )
    where

import Dfterm3.Prelude
import Control.Concurrent
import Control.Exception
import System.IO

data FinRef = FinRef (IORef ()) (IORef Bool) (IO ())
              deriving ( Typeable )

instance Eq FinRef where
    (FinRef ref _ _) == (FinRef ref2 _ _) = ref == ref2

-- | Creates an IORef with a finalizer.
newFinalizableIORef :: a -> IO () -> IO (IORef a)
newFinalizableIORef value finalizer = do
    ref <- newIORef value
    void $ mkWeakIORef ref finalizer
    return ref

-- | Creates a `FinRef`. This is similar to `newFinalizableIORef` but the
-- handle is a `FinRef` instead.
newFinalizableFinRef :: IO () -> IO FinRef
newFinalizableFinRef finalizer = do
    ref <- newIORef ()
    bool_ref <- newIORef False
    void $ mkWeakIORef ref $ do
        dont_finalize <- atomicModifyIORef' bool_ref $ \old -> ( True, old )
        unless dont_finalize finalizer

    return $ FinRef ref bool_ref finalizer

-- | Runs a finalizer a `FinRef` immediately. The finalizer will not be run
-- again later.
--
-- If the finalizer was already run once before, this will not run it again.
finalizeFinRef :: FinRef -> IO ()
finalizeFinRef (FinRef _ bool_ref finalizer) = do
    dont_finalize <- atomicModifyIORef' bool_ref $ \old -> ( True, old )
    unless dont_finalize finalizer

touchFinRef :: FinRef -> IO ()
touchFinRef (FinRef !_ !_ !_) = return ()
{-# NOINLINE touchFinRef #-}

-- | The same as `forkIO` but if an uncaught exception is thrown, it tags it
-- with the given string.
forkExceptionTaggedIO :: String -> IO () -> IO ThreadId
forkExceptionTaggedIO tag action =
    forkIO $ exceptionTaggedIO tag action

-- | Similar to `forkExceptionTaggedIO` but does not fork a thread.
--
-- This catches all exceptions so you probably only want to use it at top-level
-- of a thread where those exceptions would not be caught anyway.
exceptionTaggedIO :: String -> IO () -> IO ()
exceptionTaggedIO tag action =
    catch action $ \e ->
        hPutStrLn stderr $ "[" ++ tag ++ "] " ++ show (e :: SomeException)

-- | Launches a thread that will be killed when the given action finishes.
forkDyingIO :: IO ()        -- ^ Computation to run in thread.
            -> IO a         -- ^ Computation to run after forking.
            -> IO a
forkDyingIO thread_action action = mask $ \restore -> do
    tid <- forkIOWithUnmask $ \unmask -> unmask thread_action
    finally (restore action) (killThread tid)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) action = action x

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM maybe_action action = do
    result <- maybe_action
    forM_ result action

