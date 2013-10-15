-- | Dfhack plugin based Dwarf Fortress game module.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies, TemplateHaskell, OverloadedStrings #-}

module Dfterm3.Game.DwarfFortress
    ( monitorDwarfFortresses
    , DwarfFortressPersistent()
    , GameInputs( DwarfFortressInput )
    , GameChangesets( DwarfFortressChangesets )
    , Input(..)
    , KeyDirection(..)
    , mkDwarfFortressPersistent
    , executable
    , workingDirectory
    , args
    , customName )
    where

import Dfterm3.Game.DwarfFortress.Internal.Running

import Dfterm3.GameSubscription
import Dfterm3.Util
import Dfterm3.Logging
import Dfterm3.Terminal
import Dfterm3.CP437ToUnicode ( cp437ToUnicode )

import Data.Array.IArray
import Data.Bits

import System.Random
import System.IO
import System.IO.Unsafe
import System.IO.Error
import System.Timeout
import System.FilePath
import System.Environment
import System.Process
import System.Process.Internals

import Network

import Data.IORef
import Data.Typeable ( Typeable )
import Data.SafeCopy
import Foreign.Storable ( sizeOf )
import Foreign.C.Types
import Data.Word
import Data.ProtocolBuffers
import Data.TypeLevel hiding ( Eq, (-), (*), Bool, (==) )
import GHC.Generics ( Generic )
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import qualified Data.Text as T

import qualified Data.Serialize.Get as S
import qualified Data.Serialize.Put as S

import qualified Data.Aeson as J
import Data.Aeson ( (.:) )

#ifdef WINDOWS
import qualified System.Win32.Process as W
#endif

#ifndef WINDOWS
import System.Posix.Signals
foreign import ccall safe "wait_pid" c_wait_pid :: CInt -> IO ()
#endif
foreign import ccall safe "terminate_process"
    c_terminate_process :: CInt -> IO ()

-- The protocol buffer thingy. Keep in sync with dfhack:
-- plugins/proto/dfterm3.proto in the dfhack repository.

data Introduction = Introduction
    { dfVersion        :: Optional D1 (Value T.Text)
    , introductionWorkingDirectory :: Optional D2 (Value T.Text)
    , introductionExecutable       :: Optional D3 (Value T.Text)
    , pid              :: Optional D4 (Value Word64) }
    deriving ( Generic, Show )

data ScreenData = ScreenData { swidth :: !Word32
                             , sheight :: !Word32
                             , screenData :: !B.ByteString
                             , colorData :: !B.ByteString }

instance Decode Introduction

-- | The data that needs to be persistently stored about a Dwarf Fortress.
data DwarfFortressPersistent =
    DwarfFortressPersistent { _executable :: FilePath
                            , _workingDirectory :: FilePath
                            , _args :: [String]
                            , _customName :: T.Text }
    deriving ( Eq, Ord, Show, Read, Typeable )
deriveSafeCopy 0 'base ''DwarfFortressPersistent
makeLenses ''DwarfFortressPersistent

mkDwarfFortressPersistent :: FilePath
                          -> FilePath
                          -> [String]
                          -> T.Text
                          -> DwarfFortressPersistent
mkDwarfFortressPersistent =
    DwarfFortressPersistent

data KeyDirection = Up | Down | UpAndDown
                    deriving ( Eq, Ord, Read, Show, Typeable )

data Input = Input !Int !Word32 !Bool !Bool !Bool
             deriving ( Eq, Ord, Read, Show, Typeable )

instance J.FromJSON Input where
    parseJSON (J.Object v) = Input <$>
                             v .: "which" <*>
                             v .: "code_point" <*>
                             v .: "shift" <*>
                             v .: "alt" <*>
                             v .: "ctrl"
    parseJSON _ = mzero

instance PublishableGame DwarfFortressPersistent where
    data GameRawInstance DwarfFortressPersistent =
        DwarfFortressRunning
        { _pid :: !Word64
        , _handle :: !Handle
        , _executableInstance :: FilePath
        , _workingDirectoryInstance :: FilePath
        , _inputsChan :: TChan (GameInputs DwarfFortressPersistent)
        , _outputsChan :: TChan (GameChangesets DwarfFortressPersistent)
        , _deathChan :: TChan ()
        , _dfVersionInstance :: T.Text
        , _runTid :: ThreadId
        , _parentInstance ::
            IORef (Either () (Maybe (GameInstance DwarfFortressPersistent))) }

    data GameInputs DwarfFortressPersistent =
        DwarfFortressInput !KeyDirection !T.Text !Input

    data GameChangesets DwarfFortressPersistent =
        -- keep these lazy
        DwarfFortressChangesets Terminal TerminalChanges (Maybe T.Text)

    uniqueKey = B8.fromString . _executable
    uniqueInstanceKey = S.runPut . S.putWord64be . _pid
    uniqueGameWideKey _ = B8.fromString "Dfterm3.Game.DwarfFortress"
    gameName game = T.pack "Dwarf Fortress: " `T.append` _customName game
    stopInstance = killThread . _runTid

    lookForGames = lookForDwarfFortresses

    procureInstance_ = procureDwarfFortress

dfstate :: DFState (GameRawInstance DwarfFortressPersistent)
dfstate = unsafePerformIO newDFState
{-# NOINLINE dfstate #-}

lookForDwarfFortresses :: IO [DwarfFortressPersistent]
lookForDwarfFortresses =
    fmap (fmap toDFP) $ allAliveInstances dfstate
  where
    toDFP ginstance =
        DwarfFortressPersistent
            { _executable = _executableInstance ginstance
            , _workingDirectory = _workingDirectoryInstance ginstance
            , _args = []
            , _customName = _dfVersionInstance ginstance }

-- | Monitors system ports for Dwarf Fortresses. You run this once and then
-- forget about it. You need to run it for Dfterm3 to be able to use Dwarf
-- Fortresses.
monitorDwarfFortresses :: IO ()
monitorDwarfFortresses = void $ forkIO $
    void $ forM_ ports $ \port -> forkIO $ portChecker port

ports :: [Word16]
ports = [48000..48100]

portChecker :: Word16 -> IO ()
portChecker port = mask $ \restore -> forever $ do
    is_done <- newEmptyMVar
    tid <- forkIO $ finally (restore $ portChecker' port) $
                        putMVar is_done ()
    finally (restore $ takeMVar is_done) $
        killThread tid
    -- Have somewhat random delay.
    -- This is to evenly distribute load.
    delay <- randomRIO (5000000, 10000000)
    threadDelay delay

-- | Data type to throw an exception without writing to stderr or stdout or
-- somewhere else.
data SilentlyDie = SilentlyDie
                   deriving ( Eq, Show, Typeable )

instance Exception SilentlyDie

catchSilents :: IO a -> IO (Maybe a)
catchSilents action =
    catch (Just `fmap` action)
        (\e -> do
            e `seq` void $ return (e :: SilentlyDie)
            return Nothing)

catchSilents_ :: IO a -> IO ()
catchSilents_ = void . catchSilents

portChecker' :: Word16 -> IO ()
portChecker' port = catchSilents_ $ do
    -- squelch error messages in case of an IO error. That's why we use
    -- tryIOError instead of just letting it fail.
    maybe_handle <- tryIOError $
                        connectTo "127.0.0.1" (PortNumber $ fromIntegral port)
    case maybe_handle of
        Right handle -> finally (dfhackConnection handle) (hClose handle)
        Left _ -> return ()

dfhackConnection :: Handle -> IO ()
dfhackConnection handle = do
    Left info <- hGetMessage handle :: IO (Either Introduction ScreenData)
    let Just working_dir = getField $ introductionWorkingDirectory info
        Just df_executable' = getField $ introductionExecutable info
        Just df_pid' = getField $ pid info
        Just df_version = getField $ dfVersion info
        df_pid = fromIntegral df_pid' :: DFPid

    df_executable <- T.pack <$> detectDFHackScript (T.unpack df_executable')
    let df_executable_bs = B8.fromString (T.unpack df_executable)

    cookie_contents <- readMagicCookieFile (T.unpack working_dir ++
                                            "/.dfterm3-cookie")
    hSendByteString handle cookie_contents
    Right msg <- hGetMessage handle :: IO (Either Introduction ScreenData)

    logInfo $ "Successfully formed a connection to a Dfhack "
              ++ "Dfterm3 plugin. Pid: " ++ show df_pid ++ ", " ++
              "working directory: '" ++ T.unpack working_dir ++
              "', executable: '" ++ T.unpack df_executable ++ "'" ++
              if df_executable /= df_executable'
                then " (inferred from the location of DFHack script)."
                else "."

    input_channel <- newTChanIO
    output_channel <- newBroadcastTChanIO
    death_channel <- newTChanIO

    tid <- myThreadId

    last_player_ref <- newIORef Nothing

    flip finally (do
        atomically $ writeTChan death_channel ()
        reapPid df_pid
        unregister df_executable_bs dfstate) $ do

    parent_ref <- newIORef (Left ())

    let running_instance = DwarfFortressRunning { _pid = df_pid
                                                , _handle = handle
                                                , _executableInstance =
                                                    T.unpack df_executable
                                                , _workingDirectoryInstance =
                                                    T.unpack working_dir
                                                , _inputsChan = input_channel
                                                , _outputsChan = output_channel
                                                , _deathChan = death_channel
                                                , _dfVersionInstance =
                                                    df_version
                                                , _parentInstance = parent_ref
                                                , _runTid = tid
                                                }

    registerNew df_executable_bs dfstate running_instance

    forkDyingIO (input_processer input_channel handle last_player_ref) $
        rec output_channel
            (newTerminal (listArray ((0, 0), (79, 24))
                                    [Cell (T.pack " ")
                                          White
                                          Black])
                         (0, 0))
            msg
            last_player_ref
  where
    input_processer :: TChan (GameInputs DwarfFortressPersistent)
                    -> Handle
                    -> IORef (Maybe T.Text)
                    -> IO ()
    input_processer channel handle last_player_ref = forever $ do
        input <- atomically $ readTChan channel
        case input of
            DwarfFortressInput key_direction
                               who
                               (Input code code_point shift alt ctrl) -> do
                writeIORef last_player_ref (Just who)
                hSendByteString handle $ B.pack [1] `B.append` S.runPut (do
                    S.putWord32be (fromIntegral code)
                    S.putWord32be (fromIntegral code_point)
                    S.putWord8 (directionIntegerify key_direction)
                    S.putWord8 (integerify shift)
                    S.putWord8 (integerify alt)
                    S.putWord8 (integerify ctrl))

    rec :: TChan (GameChangesets DwarfFortressPersistent)
        -> Terminal
        -> ScreenData
        -> IORef (Maybe T.Text)
        -> IO ()
    rec channel terminal msg last_player_ref = do
        let grid_data = screenData msg
            colors_data = colorData msg
            w = swidth msg
            h = sheight msg

            assocs = [(fromIntegral x, fromIntegral y) |
                      y <- [0..h-1], x <- [0..w-1]]
            chars = B.unpack grid_data
            clrs = B.unpack colors_data

            new_terminal = newTerminal
                            (array ( (0, 0)
                                   , (fromIntegral w-1, fromIntegral h-1) )
                                   (zip assocs $ zipWith unicode_nator
                                                         chars
                                                         clrs))
                            (0, 0)

        last_player <- readIORef last_player_ref

        atomically $ writeTChan channel
                                (DwarfFortressChangesets
                                    new_terminal
                                    (findChanges terminal new_terminal)
                                    last_player)

        Right next_message <- hGetMessage handle
            :: IO (Either Introduction ScreenData)

        rec channel new_terminal next_message last_player_ref

    unicode_nator :: Word8 -> Word8 -> Cell
    unicode_nator code colors = Cell (T.singleton $ cp437ToUnicode code)
                                     foreground_color
                                     background_color
      where
        foreground_color = intToColor $ fromIntegral $
            (colors .&. 0xf0) `shift` (-4)
        background_color = intToColor $ fromIntegral (colors .&. 0x0f)

directionIntegerify :: KeyDirection -> Word8
directionIntegerify Up = 0
directionIntegerify Down = 1
directionIntegerify UpAndDown = 2

integerify :: Bool -> Word8
integerify True = 1
integerify False = 0

reapPid :: DFPid -> IO ()
reapPid pid = void $ forkIO $ do
    logInfo $ "Reaping process: " ++ show pid
    c_terminate_process (fromIntegral pid)
#ifndef WINDOWS
    threadDelay 5000000
    signalProcess sigKILL (fromIntegral pid)
    c_wait_pid (fromIntegral pid)
#endif
    logInfo $ "Reaped process: " ++ show pid

hGetOrDie :: Handle -> Int -> IO B.ByteString
hGetOrDie _ 0          = return B.empty
hGetOrDie handle bytes = do
    result <- B.hGet handle bytes
    if B.null result
      then do logNotice "Connection lost to Dwarf Fortress."
              throwIO SilentlyDie
      else return result

hGetMessage :: Decode a => Handle -> IO (Either a ScreenData)
hGetMessage handle = do
    len_bs <- hGetOrDie handle (sizeOf (undefined :: Word32))
    let Right len = S.runGet S.getWord32be len_bs
    is_protobuf_msg <- antiIntegerify . B.head <$> hGetOrDie handle 1
    if is_protobuf_msg
       then do msg <- hGetOrDie handle (fromIntegral len)
               case S.runGet decodeMessage msg of
                 Left _ -> error $ "Malformed data received from Dfhack " ++
                                   "to Dfterm3."
                 Right x -> return $ Left x
       else Right <$> hGetScreenData handle

hGetScreenData :: Handle -> IO ScreenData
hGetScreenData handle = do
    Right w <- get32
    Right h <- get32
    let data_size = fromIntegral $ w*h
    chars <- hGetOrDie handle data_size
    colors <- hGetOrDie handle data_size
    return $ ScreenData w h chars colors
  where
    get32 = S.runGet S.getWord32be <$>
                hGetOrDie handle (sizeOf (undefined :: Word32))

hSendByteString :: Handle -> B.ByteString -> IO ()
hSendByteString handle bytestring = do
    let len_bs = S.runPut $
                     S.putWord32be (safeFromIntegral $ B.length bytestring)
    B.hPut handle len_bs
    B.hPut handle bytestring
    hFlush handle

-- | Detects if there is a Dfhack script under a file path.
--
-- On Linux, when you run the /dfhack/ shells script, the actual process that
-- we see is ./libs/Dwarf_Fortress. But we want to use the Dfhack script.
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

readMagicCookieFile :: FilePath -> IO B.ByteString
readMagicCookieFile = B.readFile

antiIntegerify :: Word8 -> Bool
antiIntegerify 0 = False
antiIntegerify _ = True

procureDwarfFortress :: DwarfFortressPersistent
                     -> IO (Procurement DwarfFortressPersistent)
procureDwarfFortress df = do
    mvar_status <- takeOldOrStartProcuring
                       (B8.fromString $ _executable df)
                       dfstate
    case mvar_status of
        PleaseProcure mvar -> launchDwarfFortress df mvar
        AlreadyProcuring mvar -> do
            maybe_ginst <- timeout 25000000 $ readMVar mvar
            case maybe_ginst of
                Nothing    -> return Failed
                Just ginst -> getParent ginst
  where
    getParent ginst = do
        result <- atomicModifyIORef' (_parentInstance ginst) $ \old ->
            case old of
                Left ()       -> ( Right Nothing, Left (0 :: Int))
                Right Nothing -> ( Right Nothing, Left 1 )
                Right (Just inst) -> ( Right (Just inst), Right inst )

        case result of
            Left 0 -> return $ LaunchedNewInstance
                               ( ginst, procurement ginst )
            Left 1 -> threadDelay 500000 >> getParent ginst
            Left _ -> error "Impossible!"
            Right inst -> return $ ShareWithInstance inst

launchDwarfFortress :: DwarfFortressPersistent
                    -> MVar (GameRawInstance DwarfFortressPersistent)
                    -> IO (Procurement DwarfFortressPersistent)
launchDwarfFortress df mvar = do
    old_env <- getEnvironment
#ifdef WINDOWS
    let stream = Inherit
#else
    devnull <- openFile "/dev/null" ReadWriteMode
    let stream = UseHandle devnull
    flip finally (hClose devnull) $ do
#endif

    (_, _, _, phandle) <-
        createProcess CreateProcess
                                { cmdspec = RawCommand
                                            (_executable df)
                                            []
                                , cwd = Just (_workingDirectory df)
                                , env = Just $ ("START_DFTERM3", "1"):old_env
                                , std_in = stream
                                , std_out = stream
                                , std_err = stream
                                , close_fds = True
                                , create_group = True }

    maybe_pid <- pidOfHandle phandle
    logInfo $
        "Created a process using executable '" ++ _executable df
        ++ "', working directory '" ++ _workingDirectory df ++ "'" ++
        case maybe_pid of
            Nothing -> ""
            Just pid -> " to pid " ++ show pid

    onException (do
        maybe_ginst <- timeout 25000000 $ readMVar mvar
        case maybe_ginst of
            Nothing    -> undoProcess phandle >> return Failed
            Just ginst -> return $ LaunchedNewInstance
                                   ( ginst, procurement ginst ))
        (undoProcess phandle)
  where
    undoProcess phandle = whenJustM (pidOfHandle phandle) reapPid

pidOfHandle :: ProcessHandle -> IO (Maybe DFPid)
pidOfHandle phandle =
    withProcessHandle phandle $ \p ->
#ifdef WINDOWS
        case p of
            ClosedHandle _ -> return (p, Nothing)
            OpenHandle pid -> do
                real_pid <- W.getProcessId pid
                return (p, Just $ fromIntegral real_pid)
#else
        return $ case p of
            ClosedHandle _ -> (p, Nothing)
            OpenHandle pid -> (p, Just $ fromIntegral pid)
#endif

procurement :: GameRawInstance DwarfFortressPersistent
            -> GameInstance DwarfFortressPersistent
            -> IO ()
procurement raw_instance game_instance = do
    writeIORef (_parentInstance raw_instance) (Right $ Just game_instance)
    outputs_chan <- atomically $ dupTChan (_outputsChan raw_instance)

    catchSilents_ $
        forever $ do
            ( maybe_new_changesets,
              maybe_new_inputs,
              maybe_new_death ) <-
                    atomically $ do
                        results <- liftM3 (,,)
                            (tryReadTChan outputs_chan)
                            (tryReceiveInputSTM game_instance)
                            (tryReadTChan death_chan)
                        case results of
                            (Nothing, Nothing, Nothing) -> retry
                            _ -> return results

            whenJust maybe_new_death $ \_ -> throwIO SilentlyDie
            whenJust maybe_new_changesets $ changesets game_instance
            whenJust maybe_new_inputs $ atomically . writeTChan inputs_chan
  where
    death_chan = _deathChan raw_instance
    inputs_chan = _inputsChan raw_instance

