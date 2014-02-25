{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}

module Dfterm3.Server.WebSocket
    ( runWebSocket )
    where

import Dfterm3.Dfterm3State
import Network.WebSockets

import Data.Typeable
import Data.Aeson
import Data.Word
import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import qualified Data.Text as T

data ServerToClient =
    Handshake
    { majorVersion :: !Int
    , minorVersion :: !Int
    , patchVersion :: !Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ClientToServer =
    Login
    { identity :: !Identity
    , password :: !(Maybe T.Text) }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Identity =
      User !T.Text
    | Guest
    deriving ( Eq, Ord, Show, Read, Typeable )

instance ToJSON ServerToClient where
    toJSON (Handshake {..}) =
        object [ "message" .= ("handshake" :: T.Text)
               , "server"  .= ("dfterm3" :: T.Text)
               , "major"   .= majorVersion
               , "minor"   .= minorVersion
               , "patch"   .= patchVersion ]

instance FromJSON Identity where
    parseJSON (Object v) = do
        msum [ User <$> v .: "user"
             , do True <- v .: "guest"
                  return Guest ]
    parseJSON _ = mzero

instance FromJSON ClientToServer where
    parseJSON (Object v) = do
        msg <- v .: "message"
        guard (msg == ("login" :: T.Text))
        iden <- v .: "identity"
        pass <-
            msum [ Just <$> (v .: "password")
                 , return Nothing ]

        return (Login { identity = iden
                      , password = pass })

    parseJSON _ = mzero

handshake :: ServerToClient
handshake = Handshake
    { majorVersion = 0
    , minorVersion = 1
    , patchVersion = 0 }

runWebSocket :: Word16 -> Storage -> IO ()
runWebSocket port storage =
    runServer "0.0.0.0" (fromIntegral port) $ \pending_conn -> do
        conn <- acceptRequest pending_conn
        client conn storage

type Client a = ReaderT (Connection, Storage) IO a

push :: ServerToClient -> Client ()
push msg = do
    conn <- fst <$> ask
    lift $ sendDataMessage conn (Text $ encode msg)

pull :: Client ClientToServer
pull = do
    conn <- fst <$> ask
    Text msg <- lift $ receiveDataMessage conn
    let Just decoded = decode msg
    return decoded

client :: Connection -> Storage -> IO ()
client conn storage = flip runReaderT (conn, storage) $ do
    push handshake
    forever $ do
        msg <- pull
        -- TODO: do something...
        liftIO $ print msg

