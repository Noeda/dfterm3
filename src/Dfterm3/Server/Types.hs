{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

module Dfterm3.Server.Types
    ( ServerToClient(..)
    , ClientToServer(..)
    , Identity(..)
    , handshake )
    where

import Data.Aeson
import qualified Data.Text as T

import Data.Typeable
import Control.Monad
import Control.Applicative

data ServerToClient =
    Handshake
    { majorVersion :: !Int
    , minorVersion :: !Int
    , patchVersion :: !Int }
  | LoginAck
    { status :: !Bool
    , notice :: !T.Text }
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
    toJSON (LoginAck {..}) =
        object [ "message" .= ("login_acknowledgement" :: T.Text)
               , "status"  .= status
               , "notice"  .= notice ]

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

