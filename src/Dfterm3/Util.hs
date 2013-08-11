-- | All sorts of functions that have no clear place where else they could be.
--

module Dfterm3.Util
    ( whenJust
    , touchIORef
    , newFinalizableIORef )
    where

import Data.IORef
import Control.Monad ( void )

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
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

