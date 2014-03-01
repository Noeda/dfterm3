{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main
    ( main )
    where

import Dfterm3.Server.Types

import Network
import Network.WebSockets
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Data.Text as T
import Prelude hiding ( sum )
import System.Console.GetOpt
import System.Environment
import System.IO
import Data.Semigroup hiding ( Option )
import Data.Foldable
import Data.Word
import Data.Typeable
import Data.Maybe
import Data.Aeson

data Flag =
    Host String
  | Port Word16
  deriving ( Eq, Ord, Show, Read, Typeable )

options :: [OptDescr Flag]
options = [
    Option "h" ["host"] (ReqArg Host "HOST")   "dfterm3 server to connect to"
  , Option "p" ["port"] (OptArg (maybe (Port 8080) (Port . read)) "PORT")
                        "connection port (default 8080)"
  ]

host :: Flag -> Maybe String
host (Host x) = Just x
host _ = Nothing

port :: Flag -> Maybe Word16
port (Port x) = Just x
port _ = Nothing

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    case getOpt Permute options args of
        (args, [], []) -> client args
        (_, extras, errs) -> do
            hPutStrLn stderr $
                "Invalid command line parameters: " <> show extras <> " " <>
                                                       show errs
            hPutStrLn stderr usage

usage :: String
usage = usageInfo "dfterm3-cli-client" options

client :: [Flag] -> IO ()
client flags
    | sum (fmap (\x -> if isJust (host x) then 1 else 0) flags) /= 1 =
        error "I need exactly one --host command line parameter."
    | sum (fmap (\x -> if isJust (port x) then 1 else 0) flags) > 1 =
        error "I need at most one --port command line parameter."
    | otherwise = client' actualHost actualPort
  where
    Just (Host actualHost) = find (isJust . host) flags
    actualPort =
        case find (isJust . port) flags of
            Just (Port x) -> x
            Nothing       -> 8080

client' :: String -> Word16 -> IO ()
client' host port = runClient host (fromIntegral port) "/" conn
  where
    conn c = flip runReaderT c $ do
        shake <- await
        if majorVersion shake == majorVersion handshake
          then loginAndGo
          else error $
              "Server major version does not match client major version. "

loginAndGo :: Client ()
loginAndGo = do
    liftIO $ putStrLn "Login as: (empty for guest)"
    username <- liftIO getLine
    if username == ""
      then guestLoginAndGo
      else do liftIO $ putStrLn "Password: (empty for no password)"
              passwd <- liftIO getLine
              login username passwd
  where
    login username passwd = do
        yield (Login { identity = User (T.pack username)
                     , password = if passwd /= ""
                                    then Just (T.pack passwd)
                                    else Nothing })
        verifyLogin

guestLoginAndGo :: Client ()
guestLoginAndGo = do
    yield (Login { identity = Guest
                 , password = Nothing })
    verifyLogin

verifyLogin :: Client ()
verifyLogin = do
    verification <- await
    case verification of
        LoginAck {..} -> do
            if status == False
              then do liftIO $ putStrLn $ "Login failed: " <>
                                          T.unpack notice
                      loginAndGo
              else do liftIO $ putStrLn $ "Login successful."
                      when (notice /= T.empty) $
                          liftIO $ putStrLn (T.unpack notice)
                      go
        _ -> verifyLogin

go :: Client ()
go = forever $ do
    msg <- await
    -- TODO: continue from here
    liftIO $ print msg

type Client a = ReaderT Connection IO a

await :: Client ServerToClient
await = do
    conn <- ask
    maybe_msg <- liftIO $ receiveDataMessage conn
    case maybe_msg of
        Binary _ -> await
        Text msg -> do
            let maybe_decoded = decode msg
            case maybe_decoded of
                Nothing -> do
                    liftIO $ hPutStrLn stderr $
                        "Warning: could not decode " <> show msg
                    await
                Just decoded -> do
                    return decoded

yield :: ClientToServer -> Client ()
yield cts = do
    conn <- ask
    liftIO $ sendDataMessage conn (Text (encode cts))

