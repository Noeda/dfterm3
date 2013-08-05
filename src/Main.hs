{-# LANGUAGE DeriveDataTypeable #-}

module Main ( main ) where

import ConfiguredDefaults

import qualified Dfterm3.WebsocketAccepter as WS
import Dfterm3.Logging
import Dfterm3.GamePool
import Dfterm3.DwarfFortress

import Data.Typeable
import Data.Word

import Control.Monad ( forM_ )

import System.Console.GetOpt
import System.Exit
import System.Environment
import System.IO

import System.Posix.Daemon

import GHC.Conc ( setNumCapabilities, getNumCapabilities, getNumProcessors )
import Network ( withSocketsDo )
import Control.Concurrent ( threadDelay )
import Control.Monad ( forever, void )

data StartupOption = Websocket !Word16
                   | ShowHelp
                   | StorageLocation FilePath
                   | Daemonize (Maybe FilePath)
                   | UseSyslog
                   deriving ( Eq, Ord, Show, Read, Typeable )

options :: [OptDescr StartupOption]
options = [ Option "w" ["websocket"]
            (ReqArg (Websocket . read) "PORT")
            "listen for WebSocket connections on this port."
          , Option "s" ["storage"]
            (ReqArg StorageLocation "DIRECTORY")
            "specify which directory to use to store runtime data."
          , Option "h?" ["help"]
            (NoArg ShowHelp)
            "show valid command line options and exit."
          , Option "d" ["daemon", "daemonize"]
            (OptArg Daemonize "PIDFILE")
            "run as a daemon (background process)."
          , Option [] ["syslog"]
            (NoArg UseSyslog)
            "use syslog for logging." ]

isStorageOption :: StartupOption -> Bool
isStorageOption (StorageLocation _) = True
isStorageOption _ = False

isDaemonizeOption :: StartupOption -> Bool
isDaemonizeOption (Daemonize _) = True
isDaemonizeOption _ = False

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (options, [], []) -> run options
        (_, (e:_), []) -> do
            hPutStrLn stderr $
                "Unknown command line option " ++ show e
            exitFailure
        (_, _, errors) -> do
            hPutStrLn stderr $ "Invalid command line options. "
            forM_ errors (hPutStrLn stderr)
            hPutStrLn stderr
                "Use -h, -? or --help to see valid commane line options."
            exitFailure

run :: [StartupOption] -> IO ()
run options
    | ShowHelp `elem` options = showHelp >> exitSuccess
    | length (filter isStorageOption options) > 1 = do
        hPutStrLn
            stderr
            "Only one or zero storage location directories must be specified."
        exitFailure
    | length (filter isDaemonizeOption options) > 1 = do
        hPutStrLn
            stderr
            "Only one or zero daemonizing options must be specified."
        exitFailure
    | any isDaemonizeOption options = do
        let Daemonize maybe_filepath = head $ filter isDaemonizeOption options
        runDetached maybe_filepath
                    DevNull
                    (run (filter (not . isDaemonizeOption) options))
        exitSuccess
    | otherwise = withSocketsDo $ do
    pool <- newGamePool
    run' pool
  where
    use_syslog = UseSyslog `elem` options

    specified_storage =
        case filter isStorageOption options of
            [] -> defaultStorageDirectory
            [StorageLocation x] -> return x
            _ -> undefined

    run' pool = do
        if use_syslog
          then initializeLogging Syslog
          else initializeLogging Simple

        logInfo "Dfterm3 starting up."

        -- Make sure we get to use all the cores. The unfortunate side effect
        -- is that the rtsopt -N is ignored.
        setNumCapabilities =<< getNumProcessors
        capabilities <- getNumCapabilities

        storage_directory <- specified_storage

        logInfo $ "Using " ++ show capabilities ++ " operating system threads."
        logInfo $ "Using \'" ++ storage_directory ++ "\' as storage directory."

        -- Start the websocket services
        forM_ options $ applyOption

        void $ monitorDwarfFortresses pool

        -- The server is service oriented and the main function has nothing to
        -- do.  Let us loop forever.
        forever $ threadDelay 1000000000
      where
        applyOption (Websocket port) = void $ WS.listen pool port
        applyOption _ = return ()

showHelp :: IO ()
showHelp = do
    dir <- defaultStorageDirectory
    putStrLn (usageInfo "dfterm3 [options in any order]" options)
    putStrLn $ "You can specify several WebSocket ports to listen on more \
               \than one port."
    putStrLn $ "The default storage directory for the current user is \'" ++
               dir ++
               "\'. " ++
               "Specifying the --storage option overrides the default setting."
    putStrLn $ "If you don't specify the pidfile in any of the daemon options,\
               \ then no pidfile lock will be used."
    putStrLn $ "If --syslog is not specified, then logging will written to \
               \standard output."

