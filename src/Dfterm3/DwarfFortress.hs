-- | This module talks to Dfhack to get Dwarf Fortress games into Dfterm3.

{-# LANGUAGE Rank2Types, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dfterm3.DwarfFortress
    ( monitorDwarfFortresses
    , DwarfFortress(..)
    , game, df
    , dfExecutable
    , dfArgs
    , dfWorkingDirectory

    , launchDwarfFortress

    , enumerateRunningGames
    , enumerateRunningGames' )
    where

import Dfterm3.GamePool
import Dfterm3.CP437Game
import Dfterm3.Logging
import Dfterm3.Safe

import Dfterm3.DwarfFortress.Types
import Dfterm3.Util

import System.IO
import System.IO.Error
import System.IO.Unsafe ( unsafePerformIO )
import System.Random
import System.FilePath
import System.Exit

import Control.Applicative ( (<$>) )
import Control.Monad.IO.Class ( liftIO )
import Control.Lens
import Data.Word ( Word8, Word16, Word32, Word64 )
import Foreign.Storable ( sizeOf )
import Data.IORef
import Data.Typeable
import Data.Foldable
import Data.Maybe ( catMaybes )
import Data.ProtocolBuffers
import Data.Array
import Data.TypeLevel hiding ( Eq, (-), (*), Bool )
import Data.Bits
import GHC.Generics ( Generic )
import Control.Concurrent
import Control.Monad ( forever, void, forM, replicateM )
import Control.Exception
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import Network

import System.Posix.Types
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Directory
import System.Posix.Terminal
import System.Posix.IO
import System.Environment

import qualified Data.Serialize.Get as S
import qualified Data.Serialize.Put as S

data SilentlyDie = SilentlyDie
                   deriving ( Eq, Show, Typeable )

instance Exception SilentlyDie

ports :: [Word16]
ports = [48000..48100]

executingDwarfFortresses :: MVar (S.Set String)
executingDwarfFortresses = unsafePerformIO $ newMVar S.empty
{-# NOINLINE executingDwarfFortresses #-}

runningFortresses :: IORef (M.Map ProcessID ( DwarfFortressInstance
                                            , Maybe DwarfFortressExec) )
runningFortresses = unsafePerformIO $ newIORef M.empty
{-# NOINLINE runningFortresses #-}

newtype DwarfFortressExec = DwarfFortressExec (IORef ProcessID)

-- | Monitors system ports for Dwarf Fortress processes and registers them to
-- the game pool as they are found.
--
-- This launches a thread. You can safely kill it if you want it gone. This
-- call unmasks asynchronous exceptions for the returned ThreadId.
monitorDwarfFortresses :: GamePool -> IO ThreadId
monitorDwarfFortresses pool =
    forkIOWithUnmask $ \um -> um $ mask $ \unmask -> do

    tids <- forM ports $ \port -> forkIOWithUnmask $ portChecker pool port
    finally (unmask $ forever $ threadDelay 1000000000) $
        forM_ tids $ \tid -> killThread tid

portChecker :: GamePool -> Word16 -> (forall a. IO a -> IO a) -> IO ()
portChecker pool port unmask = forever $ do
    is_done <- newEmptyMVar
    tid <- forkIO $ finally (unmask $ portChecker' pool port) $
                        putMVar is_done ()
    finally (unmask $ takeMVar is_done) $
        killThread tid
    -- Have somewhat random delay.
    -- This is to evenly distribute load.
    delay <- randomRIO (5000000, 20000000)
    threadDelay delay

portChecker' :: GamePool -> Word16 -> IO ()
portChecker' pool port = catch (do
    -- squelch error messages in case of an IO error. That's why we use
    -- tryIOError instead of just letting it fail.
    maybe_handle <- tryIOError $
                        connectTo "127.0.0.1" (PortNumber $ fromIntegral port)
    case maybe_handle of
        Right handle -> finally (dfhackConnection pool handle) (hClose handle)
        Left _ -> return ())
        (\e -> do
            e `seq` void $ return (e :: SilentlyDie)
            return ())

--silentlyDie :: IO a
--silentlyDie = throwIO SilentlyDie

-- The protocol buffer thingy. Keep in sync with dfhack:
-- plugins/proto/dfterm3.proto in the dfhack repository.

data Introduction = Introduction
    { dfVersion        :: Optional D1 (Value T.Text)
    , workingDirectory :: Optional D2 (Value T.Text)
    , executable       :: Optional D3 (Value T.Text)
    , pid              :: Optional D4 (Value Word64) }
    deriving ( Generic, Show )

instance Decode Introduction

data ScreenData = ScreenData
    { width :: Optional D5 (Value Word32)
    , height :: Optional D6 (Value Word32)
    , screenCP437 :: Optional D7 (Value B.ByteString)
    , colors :: Optional D8 (Value B.ByteString) }
    deriving ( Generic, Show )

instance Decode ScreenData

hGetOrDie :: Handle -> Int -> IO B.ByteString
hGetOrDie _ 0          = return B.empty
hGetOrDie handle bytes = do
    result <- B.hGet handle bytes
    if B.null result
      then error "Connection lost."
      else return result

hGetMessage :: Decode a => Handle -> IO a
hGetMessage handle = do
    len_bs <- hGetOrDie handle (sizeOf (undefined :: Word32))
    let Right len = S.runGet S.getWord32be len_bs
    msg <- hGetOrDie handle (fromIntegral len)
    case S.runGet decodeMessage msg of
        Left _ -> error "Malformed data received from Dfhack to Dfterm3."
        Right x -> return x

hSendByteString :: Handle -> B.ByteString -> IO ()
hSendByteString handle bytestring = do
    let len_bs = S.runPut $
                     S.putWord32be (safeFromIntegral $ B.length bytestring)
    B.hPut handle len_bs
    B.hPut handle bytestring
    hFlush handle

dfhackConnection :: GamePool -> Handle -> IO ()
dfhackConnection pool handle = do
    info <- hGetMessage handle :: IO Introduction
    let Just version = getField $ dfVersion info
        Just working_dir = getField $ workingDirectory info
        Just df_executable = getField $ executable info
        Just df_pid' = getField $ pid info
        df_pid = fromIntegral df_pid' :: ProcessID
        df_info = DwarfFortress (T.unpack df_executable)
                                []
                                (T.unpack working_dir)
                                T.empty

    print df_pid
    flip finally (clearPid df_pid) $ do

    cookie_contents <- readMagicCookieFile (T.unpack working_dir ++
                                            "/.dfterm3-cookie")
    hSendByteString handle cookie_contents

    msg <- hGetMessage handle :: IO ScreenData

    logInfo "Successfully formed a connection to a Dfhack \
            \Dfterm3 plugin."
    mask $ \restore -> do
        ( provider, game_instance ) <- registerGame pool

        atomicModifyIORef' runningFortresses $ \old ->
            ( M.insert df_pid (game_instance, Nothing) old, () )

        forkDyingIO (input_processer provider handle) $ do
            let title = "Dwarf Fortress " `T.append` version

            finally
                (restore $ rec title
                      (provider :: DwarfFortressProvider)
                      (CP437 . emptyCP437Changes)
                      df_info
                      msg)
                (unregisterGame game_instance)
  where
    clearPid pid = do
        atomicModifyIORef' runningFortresses $ \old ->
            ( M.delete (fromIntegral pid) old, () )

    input_processer :: DwarfFortressProvider -> Handle -> IO ()
    input_processer provider handle = forever $ do
        input <- receiveGameInput provider
        case input of
            DwarfFortressInput who (Input code code_point shift alt ctrl) -> do
                hSendByteString handle $ B.pack [1] `B.append` S.runPut (do
                    S.putWord32be (fromIntegral code)
                    S.putWord32be (fromIntegral code_point)
                    S.putWord8 (integerify shift)
                    S.putWord8 (integerify alt)
                    S.putWord8 (integerify ctrl))
                updateGameWithoutState (Playing who) provider

    integerify :: Bool -> Word8
    integerify True = 1
    integerify False = 0

    rec :: T.Text
        -> DwarfFortressProvider
        -> (CP437Game -> DwarfFortressCP437Changes)
        -> DwarfFortress
        -> ScreenData
        -> IO ()
    rec title provider changer df_info msg = do
        let Just cp437Data = getField $ screenCP437 msg
            Just colorsData = getField $ colors msg
            Just w = getField $ width msg
            Just h = getField $ height msg

            assocs = [(fromIntegral x, fromIntegral y) |
                      y <- [0..h-1], x <- [0..w-1]]
            assocs_len = fromIntegral $ w*h
            Right chars =
                flip S.runGet cp437Data $ replicateM assocs_len S.getWord8
            Right clrs =
                flip S.runGet colorsData $ replicateM assocs_len S.getWord8


            cp437game = newCP437Game title $
                            array ( (0, 0)
                                  , (fromIntegral w-1, fromIntegral h-1) )
                                  (zip assocs $ zipWith cp437nator chars clrs)

        updateGame (DwarfFortressCP437 cp437game df_info)
                   (changer cp437game)
                   provider

        rec title provider (CP437 . findCP437Changes cp437game) df_info
            =<< hGetMessage handle

cp437nator :: Word8 -> Word8 -> CP437Cell
cp437nator character_code colors =
    CP437Cell character_code foreground_color background_color
  where
    foreground_color = intToColor $ fromIntegral $
        (colors .&. 0xf0) `shift` (-4)
    background_color = intToColor $ fromIntegral (colors .&. 0x0f)
{-# INLINE cp437nator #-}


readMagicCookieFile :: FilePath -> IO B.ByteString
readMagicCookieFile = B.readFile

enumerateRunningGames :: GamePool -> IO [DwarfFortressCP437]
enumerateRunningGames pool = do
    (maybe_states, _) <- unzip <$> liftIO (enumerateGames pool)
    let states = catMaybes maybe_states
    traverseOf (each . df . dfExecutable) detectDFHackScript states

enumerateRunningGames' :: GamePool -> IO [( DwarfFortressCP437
                                          , DwarfFortressInstance )]
enumerateRunningGames' pool = do
    (maybe_states, instances) <- unzip <$> liftIO (enumerateGames pool)
    let zippified' = zipWith (\a b -> case a of
                                         Nothing -> Nothing
                                         Just x  -> Just (x, b))
                            maybe_states instances
        zippified = catMaybes zippified'

    traverseOf (each . _1 . df . dfExecutable) detectDFHackScript zippified

detectDFHackScript :: FilePath -> IO FilePath
detectDFHackScript actual_executable = do

    let alternative_location = potential_dfhack_location actual_executable
    maybe_handle <- tryIOError $ openFile alternative_location ReadMode

    case maybe_handle of
        Left _ -> return actual_executable
        Right handle -> do
            hClose handle
            return alternative_location
  where
    potential_dfhack_location ex =
        replaceFileName (takeDirectory ex)
                        "dfhack"

launchDwarfFortress :: DwarfFortress
                    -> (Maybe DwarfFortressInstance -> IO ())
                    -> IO ()
launchDwarfFortress _ action = action Nothing

{- The Linux process launching does not work properly now. Dwarf Fortress
 - freezes.
    void $ forkIO $ mask $ \restore -> do
    maybe_pid <- modifyMVar executingDwarfFortresses $ \set ->
        if S.member executable set
          then return (set, Nothing)
          else do
            env <- getEnvironment
            print (df^.dfWorkingDirectory)

            (master, slave) <- openPseudoTerminal

            pid <- forkProcess $ do
                pgid <- createSession

                closeFd 0
                closeFd 1
                closeFd 2
                _ <- dupTo slave 0
                _ <- dupTo slave 1
                _ <- dupTo slave 2

                changeWorkingDirectory (df^.dfWorkingDirectory)
                executeFile executable
                            True
                            []
                            (Just $ [("START_DFTERM3", "1")] ++
                                    env)
                exitImmediately (ExitFailure (-1))

            closeFd master

            logInfo $ "Forked process '" ++ executable ++ "' to pid " ++
                      show pid ++ "."
            ref <- DwarfFortressExec <$> (newFinalizableIORef pid $ do
                       reapPid pid)
            return $ (S.insert executable set, Just (pid, ref))

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
            Just df_instance -> do
                action (Just df_instance)
-}

