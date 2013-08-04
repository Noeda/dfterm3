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

import GHC.Conc ( setNumCapabilities, getNumCapabilities, getNumProcessors )
import Network ( withSocketsDo )
import Control.Concurrent ( threadDelay )
import Control.Monad ( forever, void )

data StartupOption = Websocket !Word16
                   | ShowHelp
                   | StorageLocation FilePath
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
            "show valid command line options and exit." ]

isStorageOption :: StartupOption -> Bool
isStorageOption (StorageLocation _) = True
isStorageOption _ = False

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
    | otherwise = withSocketsDo $ do
    pool <- newGamePool
    run' pool
  where
    specified_storage =
        case filter isStorageOption options of
            [] -> defaultStorageDirectory
            [StorageLocation x] -> return x
            _ -> undefined

    run' pool = do
        initializeLogging
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

