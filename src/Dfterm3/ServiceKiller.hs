-- | In Dfterm3 different servers provide a `KillHandle` that can be used to
-- asynchronously stop the server. This module provides the common interface to
-- these handles.
--

module Dfterm3.ServiceKiller
    ( KillHandle()
    , newKillHandle
    , kill )
    where

import Control.Concurrent
import Control.Monad ( void )

-- | `KillHandle` is the handle through which you can terminate services. It is
-- essentially just a wrapper around `ThreadId` that is hidden.
newtype KillHandle = KillHandle ThreadId

-- | Makes a kill handle out of a thread id. When the handle is killed, it's
-- the same as sending `ThreadKilled` to the thread.
newKillHandle :: ThreadId -> KillHandle
newKillHandle = KillHandle

-- | Kills a service.
--
-- This is done asynchronously by spawning another thread (with `forkIO`) and
-- then killing the thread. This has some implications such as that threads are
-- able to kill each other.
--
-- If the service is already dead, then does nothing.
kill :: KillHandle -> IO ()
kill (KillHandle tid) = void $ forkIO $ killThread tid

