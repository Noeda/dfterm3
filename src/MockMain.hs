{-# LANGUAGE DeriveDataTypeable, CPP #-}

module MockMain ( dfterm3 ) where

import ConfiguredDefaults

import qualified Dfterm3.WebsocketAccepter as WS
import Dfterm3.Logging
import Dfterm3.GamePool
import Dfterm3.DwarfFortress
import Dfterm3.WebsocketHTTP
import Data.Maybe ( catMaybes )
import qualified Dfterm3.UserVolatile as UV
import qualified Dfterm3.User as U

import Dfterm3.AdminPanel

import Data.Typeable
import Data.Word
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import System.Console.GetOpt
import System.Exit
import System.IO

#ifndef WINDOWS
import System.Posix.Daemon
import System.Posix.Files
import OpenSSL ( withOpenSSL )
#endif

import GHC.Conc ( setNumCapabilities, getNumCapabilities, getNumProcessors )
import Network ( withSocketsDo )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Monad ( forever, void, forM_, when )

#ifdef WINDOWS
withOpenSSL :: IO a -> IO a
withOpenSSL = id
#endif

data StartupOption = Websocket !Word16
                   | AdminPanel !Word16
                   | ShowHelp
                   | StorageLocation FilePath
                   | Daemonize (Maybe FilePath)
                   | SetAdminPassword
                   | WebsocketHTTP !Word16
                   | UseSyslog
                   | DontUseDefaultPorts
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
          , Option "p" ["no-default-ports"]
            (NoArg DontUseDefaultPorts)
            "do not implicitly open services at some ports (see below)."
#ifndef WINDOWS
          , Option "d" ["daemon", "daemonize"]
            (OptArg Daemonize "PIDFILE")
            "run as a daemon (background process)."
          , Option [] ["syslog"]
            (NoArg UseSyslog)
            "use syslog for logging."
#endif
          , Option [] ["admin-password"]
            (NoArg SetAdminPassword)
            "ask for the administrator password and exit."
          , Option [] ["admin-panel"]
            (ReqArg (AdminPanel . read) "PORT")
            "serve administrator panel as a web interface on this port."
          , Option [] ["websocket-http"]
            (ReqArg (WebsocketHTTP . read) "PORT")
            ("serve websocket playing interface as a web interface "
             ++ "on this port.") ]

isStorageOption :: StartupOption -> Bool
isStorageOption (StorageLocation _) = True
isStorageOption _ = False

isDaemonizeOption :: StartupOption -> Bool
isDaemonizeOption (Daemonize _) = True
isDaemonizeOption _ = False

isAdminPanelOption :: StartupOption -> Bool
isAdminPanelOption (AdminPanel _) = True
isAdminPanelOption _ = False

isWebsocketPortOption :: StartupOption -> Maybe Word16
isWebsocketPortOption (Websocket port) = Just port
isWebsocketPortOption _ = Nothing

dfterm3 :: [String] -> IO ()
dfterm3 args = withOpenSSL $ do
#ifndef WINDOWS
    void $ setFileCreationMask 0o077
#endif
    case getOpt Permute options args of
        (options, [], []) -> run (maybeAddDefaultOptions options)
        (_, e:_, []) -> do
            hPutStrLn stderr $
                "Unknown command line option " ++ show e
            exitFailure
        (_, _, errors) -> do
            hPutStrLn stderr "Invalid command line options. "
            forM_ errors (hPutStrLn stderr)
            hPutStrLn stderr
                "Use -h, -? or --help to see valid commane line options."
            exitFailure

maybeAddDefaultOptions :: [StartupOption] -> [StartupOption]
maybeAddDefaultOptions options
    | DontUseDefaultPorts `notElem` options =
          options ++ [Websocket 8000, AdminPanel 8081, WebsocketHTTP 8080]
    | otherwise = options

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
    | should_daemonize && should_set_admin_password = do
        hPutStrLn
            stderr
            "Cannot daemonize and set administrator password at the same time."
        exitFailure
#ifndef WINDOWS
    | should_daemonize = do
        let Daemonize maybe_filepath = head $ filter isDaemonizeOption options
        runDetached maybe_filepath
                    DevNull
                    (run (filter (not . isDaemonizeOption) options))
        exitSuccess
#endif
    | otherwise = withSocketsDo $ do
        pool <- newGamePool
        run' pool (fmap unwrap . filter isAdminPanelOption $ options)
  where
    unwrap (AdminPanel x) = x
    unwrap _ = undefined

    should_daemonize = any isDaemonizeOption options
    should_set_admin_password = SetAdminPassword `elem` options
    use_syslog = UseSyslog `elem` options

    specified_storage =
        case filter isStorageOption options of
            [] -> defaultStorageDirectory
            [StorageLocation x] -> return x
            _ -> undefined

    run' pool admin_panels = do
        initializeLogging $ if use_syslog
                              then Syslog
                              else Simple

        logInfo "Dfterm3 starting up."

        -- Make sure we get to use all the cores. The unfortunate side effect
        -- is that the rtsopt -N is ignored.
        setNumCapabilities =<< getNumProcessors
        capabilities <- getNumCapabilities

        storage_directory <- specified_storage

        logInfo $ "Using " ++ show capabilities ++ " operating system threads."

        system <- U.openStorage storage_directory
        void $ U.periodicallyCleanStaleAdminHandles system

        when should_set_admin_password $ do
            setAdminPassword system
            exitSuccess

        uv <- UV.newUserVolatile

        -- Start the websocket services
        forM_ options $ applyOption system uv

        -- Start the admin panel
        forM_ admin_panels $ forkIO . startAdminPanel pool system

        void $ monitorDwarfFortresses pool

        -- The server is service oriented and the main function has nothing to
        -- do.  Let us loop forever.
        forever $ threadDelay 1000000000
      where
        applyOption system uv (Websocket port) =
            void $ WS.listen pool system uv port
        applyOption _ _ (WebsocketHTTP port) = do
            _ <- forkIO $ startWebsocketHTTP port websocket_ports
            when (null websocket_ports) $
               logWarning $ "WebSocket HTTP server started but there are "
                            ++ "no WebSocket servers specified."
          where
            websocket_ports = catMaybes $ fmap isWebsocketPortOption options

        applyOption _ _ _ = return ()

showHelp :: IO ()
showHelp = do
    dir <- defaultStorageDirectory
    putStrLn (usageInfo "dfterm3 [options in any order]" options)
    putStrLn $ "You can specify several WebSocket ports to listen on more "
               ++ "than one port."
    putStrLn $ "The default storage directory for the current user is \'" ++
               dir ++
               "\'. " ++
               "Specifying the --storage option overrides the default setting."
#ifndef WINDOWS
    putStrLn $ "If you don't specify the pidfile in any of the daemon options,"
               ++ " then no pidfile lock will be used."
    putStrLn $ "If --syslog is not specified, then logging will written to "
               ++ "standard output.\n"
#endif
    putStrLn $ "By default, there is no administrator password. If you want "
               ++ "to use the administrator interface, you need to set it at "
               ++ "least once. You can also clear the administrator password "
               ++ "with this command if you want to disable the administrator "
               ++ "interface.\n"
    putStrLn $ "The administrator interface is served by listening on the "
               ++ "local network device 127.0.0.1. This has the implication that "
               ++ "it cannot be directly accessed from outside the local "
               ++ "computer.\n\n"
    putStrLn $ "Normally, a port is opened for WebSockets at port 8000, "
               ++ "WebSockets HTTP at port 8080 and administrator panel at "
               ++ "port 8081. If you pass --no-default-ports, then none of "
               ++ "these ports are opened unless you explicitly set them."

setAdminPassword :: U.UserSystem -> IO ()
setAdminPassword us = do
    putStr "New password: "
    hFlush stdout
    old_echo <- hGetEcho stdin
    hSetEcho stdin False
    line <- getLine
    hSetEcho stdin old_echo
    putStrLn ""
    if null line
      then do putStrLn "Disable administrator? (y/n)"
              disabling <- getLine
              case disabling of
                  "y"   -> U.setAdminPassword Nothing us
                  "yes" -> U.setAdminPassword Nothing us
                  _ -> putStrLn "Doing nothing."
      else U.setAdminPassword (Just . T.encodeUtf8 . T.pack $ line) us




