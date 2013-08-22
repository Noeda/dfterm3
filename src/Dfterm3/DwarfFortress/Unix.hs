module Dfterm3.DwarfFortress.Unix
    ( launchDwarfFortress
    , trackRunningFortress
    , untrackRunningFortress
    , DFPid )
    where

import Dfterm3.DwarfFortress.Types
import Dfterm3.Logging
import Dfterm3.Util ( newFinalizableIORef )
import Data.IORef
import Control.Concurrent
import Control.Lens
import Control.Exception ( mask )
import Control.Applicative ( (<$>) )

import qualified Data.Map as M
import qualified Data.Set as S

import System.Posix.Types
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Directory
import System.Posix.Terminal
import System.Posix.IO
import System.Exit
import System.Environment
import System.IO.Unsafe
import Control.Monad

type DFPid = ProcessID
newtype DwarfFortressExec = DwarfFortressExec (IORef ProcessID)

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

runningFortresses :: IORef (M.Map ProcessID ( DwarfFortressInstance
                                            , Maybe DwarfFortressExec) )
runningFortresses = unsafePerformIO $ newIORef M.empty
{-# NOINLINE runningFortresses #-}

launchDwarfFortress :: DwarfFortress
                    -> (Maybe DwarfFortressInstance -> IO ())
                    -> IO ()
launchDwarfFortress df action =
    void $ forkIO $ mask $ \restore -> do
    maybe_pid <- modifyMVar executingDwarfFortresses $ \set ->
        if S.member executable set
          then return (set, Nothing)
          else do
            env <- getEnvironment
            print (df^.dfWorkingDirectory)

            (master, slave) <- openPseudoTerminal

            pid <- forkProcess $ do
                _ <- createSession

                closeFd 0
                closeFd 1
                closeFd 2
                _ <- dupTo slave 0
                _ <- dupTo slave 1
                _ <- dupTo slave 2

                changeWorkingDirectory (df^.dfWorkingDirectory)
                _ <- executeFile executable
                                 True
                                 []
                                 (Just $ ("START_DFTERM3", "1"):env)
                exitImmediately (ExitFailure (-1))

            closeFd master

            logInfo $ "Forked process '" ++ executable ++ "' to pid " ++
                      show pid ++ "."
            ref <- DwarfFortressExec <$> newFinalizableIORef pid (reapPid pid)
            return (S.insert executable set, Just (pid, ref))

    case maybe_pid of
        Just pid -> restore $ wait (43 :: Int) pid
        Nothing -> action Nothing

  where
    executable = df^.dfExecutable

    reapPid pid = do
        logInfo $ "Reaping Dwarf Fortress process " ++ show pid
        modifyMVar_ executingDwarfFortresses $ return . S.delete executable
        pid_group <- getProcessGroupIDOf pid
        signalProcessGroup sigTERM pid_group
        threadDelay 5000000
        signalProcessGroup sigKILL pid_group
        void $ getProcessStatus True False pid

    wait 0 (pid, _) = do
        logNotice $ "Could not connect to forked Dwarf Fortress " ++ show pid
                    ++ "."
        action Nothing
    wait ticks (pid, ref) = do
        print ticks
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
