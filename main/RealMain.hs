{-# LANGUAGE DeriveDataTypeable, CPP, ForeignFunctionInterface #-}

module Main ( main ) where

import ConfiguredDefaults

import Dfterm3.Game.DwarfFortress
import Dfterm3.Logging
import Dfterm3.Dfterm3State
import Dfterm3.Playing.WebInterface

import Dfterm3.AdminPanel
import qualified Dfterm3.Admin as A

import Data.Typeable
import Data.Word
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import System.Environment ( getArgs )
import System.Console.GetOpt
import System.Exit
import System.IO
import Data.List ( find )

#ifndef WINDOWS
import System.Posix.Daemon
import System.Posix.Files
import OpenSSL ( withOpenSSL )

#endif

import GHC.Conc ( setNumCapabilities, getNumCapabilities, getNumProcessors )
import Network ( withSocketsDo )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Monad ( forever, void, forM_, when, unless )

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

isWebsocketPortOption :: StartupOption -> Bool
isWebsocketPortOption (Websocket _) = True
isWebsocketPortOption _ = False

isWebsocketHTTPOption :: StartupOption -> Bool
isWebsocketHTTPOption (WebsocketHTTP _) = True
isWebsocketHTTPOption _ = False

main :: IO ()
main = do
    args <- getArgs
    withOpenSSL $ do
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
    | otherwise = withSocketsDo run'
  where
    admin_panels = filter isAdminPanelOption options
    websocket_ports = filter isWebsocketPortOption options
    websocket_http_ports = filter isWebsocketHTTPOption options

    unwrap_admin_port (AdminPanel port) = port

    should_daemonize = any isDaemonizeOption options
    should_set_admin_password = SetAdminPassword `elem` options
    use_syslog = UseSyslog `elem` options

    specified_storage =
        case filter isStorageOption options of
            [] -> defaultStorageDirectory
            [StorageLocation x] -> return x
            _ -> undefined

    run' = do
        initializeLogging $ if use_syslog
                              then Syslog
                              else Simple

        logInfo "Dfterm3 0.3 starting up."

        -- Make sure we get to use all the cores. The unfortunate side effect
        -- is that the rtsopt -N is ignored.
        setNumCapabilities =<< getNumProcessors
        capabilities <- getNumCapabilities
        logInfo $ "Using " ++ show capabilities ++ " operating system threads."

        -- Open up storage
        storage_directory <- specified_storage
        logInfo $ "Using '" ++ storage_directory ++ "' as storage directory."
        storage <- openStorage storage_directory

        when should_set_admin_password $ do
            setAdminPassword storage
            exitSuccess

        monitorDwarfFortresses

        forM_ admin_panels $ \(AdminPanel port) ->
                                forkIO $ runAdminPanel
                                             port
                                             storage

        threadDelay 500000

        -- Print out instructions for dummies. Just don't tell them they are dummies.
        unless (null admin_panels) $ do
            putStrLn ""
            putStrLn "Use any of the following URIs to access the administrator panel:"
            putStrLn ""
            forM_ admin_panels $ \(AdminPanel port) -> do
                putStrLn $ "http://127.0.0.1:" ++ show port ++ "/admin/"
            putStrLn ""

        unless (null websocket_http_ports) $ do
            putStrLn ""
            putStrLn "Use any of the following URIs to access the playing interface:"
            putStrLn ""
            forM_ websocket_http_ports $ \(WebsocketHTTP port) -> do
                putStrLn $ "http://127.0.0.1:" ++ show port ++ "/"
            putStrLn ""
            putStrLn "If you want to access the interface from outside your computer, you "
            putStrLn "need to replace '127.0.0.1' with your real IP address."
            putStrLn ""


        forM_ websocket_ports $ \(Websocket port) ->
                                   forkIO $ runWebSocket
                                                port
                                                storage

        forM_ websocket_http_ports $ \(WebsocketHTTP port) ->
                                        forkIO $ runWebsocketHTTP
                                                     port
                                                     (fmap unwrap_websocket
                                                           websocket_ports)

        -- The server is service oriented and the main function has nothing to
        -- do.  Let us loop forever.
        forever $ threadDelay 1000000000
      where
        unwrap_websocket (Websocket port) = port
        unwrap_websocket _ = error "Impossible!"

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

setAdminPassword :: Storage -> IO ()
setAdminPassword ps = do
    putStr "New password (not echoed): "
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
                  "y"   -> A.setAdminPassword Nothing ps
                  "yes" -> A.setAdminPassword Nothing ps
                  _ -> do putStrLn "Allow access without authentication? (y/n)"
                          no_auth <- getLine
                          case no_auth of
                              "y"   -> A.setNoAuthentication' ps
                              "yes" -> A.setNoAuthentication' ps
                              _ -> putStrLn "Doing nothing."
      else A.setAdminPassword (Just $ T.encodeUtf8 $ T.pack line) ps

