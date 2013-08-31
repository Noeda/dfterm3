{-# LANGUAGE CPP #-}

module Dfterm3.DwarfFortress.Process
    ( launchDwarfFortress
    , trackRunningFortress
    , untrackRunningFortress
    , DFPid )
    where

import Data.Word ( Word32 )
import Dfterm3.DwarfFortress.Types
import Dfterm3.Util
import Dfterm3.Logging
import Control.Lens
import Control.Concurrent
import Control.Monad
import Control.Exception ( mask )
import System.Process
import System.Process.Internals
#ifdef WINDOWS
import qualified System.Win32.Process as W
#endif
import System.Environment
import System.IO
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type DFPid = Word32

trackRunningFortress :: DFPid -> DwarfFortressInstance -> IO ()
trackRunningFortress df_pid game_instance =
    atomicModifyIORef' runningFortresses $ \old ->
        ( M.insert df_pid (game_instance, Nothing) old, () )

untrackRunningFortress :: DFPid -> IO ()
untrackRunningFortress df_pid =
    atomicModifyIORef' runningFortresses $ \old ->
        ( M.delete (fromIntegral df_pid) old, () )

executingDwarfFortresses :: MVar (S.Set String)
executingDwarfFortresses = unsafePerformIO $ newMVar S.empty
{-# NOINLINE executingDwarfFortresses #-}

runningFortresses :: IORef (M.Map DFPid ( DwarfFortressInstance
                                        , Maybe (IORef ProcessHandle)) )
runningFortresses = unsafePerformIO $ newIORef M.empty
{-# NOINLINE runningFortresses #-}

standardStreamHandles :: IO StdStream
#ifdef WINDOWS
standardStreamHandles = return Inherit
#else
standardStreamHandles = do
    handle <- openFile "/dev/null" ReadWriteMode
    hSetBuffering handle NoBuffering
    return $ UseHandle handle
#endif

launchDwarfFortress :: DwarfFortress
                    -> (Maybe DwarfFortressInstance -> IO ())
                    -> IO ()
launchDwarfFortress df action = void $ forkIO $ mask $ \restore -> do
    maybe_pid_ref <- modifyMVar executingDwarfFortresses $ \set ->
        if S.member executable set
          then return (set, Nothing)
          else do
            env <- getEnvironment
            handle <- standardStreamHandles
            (_, _, _, process) <-
              createProcess CreateProcess { cmdspec = RawCommand executable []
                                          , cwd = Just $ df^.dfWorkingDirectory
                                          , env = Just (env ++
                                                        [("START_DFTERM3", "1")])
                                          , std_in = handle
                                          , std_out = handle
                                          , std_err = handle
                                          , close_fds = True
                                          , create_group = True }
            ref <- newFinalizableIORef process $ reapProcess process
            handle <- withProcessHandle process $ \proc ->
                      return $ case proc of
                                   OpenHandle handle -> (proc, Just handle)
                                   ClosedHandle _ -> (proc, Nothing)
            case handle of
                Nothing -> return (S.insert executable set, Nothing)
                Just handle' -> do
#ifdef WINDOWS
                    real_pid <- W.getProcessId handle'
#else
                    let real_pid = fromIntegral handle'
#endif
                    logInfo $ "Forked process '" ++ executable ++ "' to pid " ++
                              show real_pid
                    return (S.insert executable set, Just (real_pid, ref))

    case maybe_pid_ref of
        Just pid_ref -> restore $ wait (43 :: Int) pid_ref
        Nothing -> action Nothing

  where
    executable = df^.dfExecutable

    reapProcess :: ProcessHandle -> IO ()
    reapProcess handle = do
        modifyMVar_ executingDwarfFortresses $ return . S.delete executable
        terminateProcess handle
        void $ waitForProcess handle

    wait 0 _ = action Nothing
    wait ticks (pid, ref) = do
        threadDelay 500000
        maybe_df_instance <- atomicModifyIORef' runningFortresses $ \old ->
                                 case M.lookup pid old of
                                     Nothing -> ( old, Nothing )
                                     Just (inst, _) ->
                                         ( M.insert pid (inst, Just ref) old
                                         , Just inst )

        case maybe_df_instance of
            Nothing -> wait (ticks-1) (pid, ref)
            Just df_instance ->
                action (Just df_instance)

