{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

module Dfterm3.Server.Types
    ( ServerToClient(..)
    , ClientToServer(..)
    , GameEntry(..)
    , Identity(..)
    , handshake )
    where

import Data.Aeson
import qualified Data.Text as T

import Dfterm3.Game.DwarfFortress

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
  | JoinAck
    { statusJoinAck :: !Bool
    , instanceKeyJoinAck :: !T.Text
    , noticeJoinAck :: !T.Text }
  | AllGames
    { games  :: [GameEntry] }
  | ChatToGame
    { instanceKeyCTG :: !T.Text
    , speaker        :: !T.Text
    , content        :: !T.Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ClientToServer =
    Login
    { identity :: !Identity
    , password :: !(Maybe T.Text) }
  | QueryAllGames
  | JoinGame
    { keyJoinGame :: !T.Text }
  | ChatToGameClient
    { instanceKeyCTGClient :: !T.Text
    , contentClient        :: !T.Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Identity =
    User !T.Text
  | Guest
    deriving ( Eq, Ord, Show, Read, Typeable )

data GameEntry =
    GameEntry
    { gameName :: T.Text
    , instanceName :: T.Text
    , gameItself :: DwarfFortressPersistent   -- TODO: if we ever generalize to
                                              -- other games, we have to
                                              -- somehow get rid of DF
                                              -- specificyness here
    , key :: T.Text }
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
    toJSON (JoinAck {..}) =
        object [ "message" .= ("join_acknowledgement" :: T.Text)
               , "status"  .= statusJoinAck
               , "notice"  .= noticeJoinAck ]
    toJSON (AllGames {..}) =
        object [ "message" .= ("all_games" :: T.Text)
               , "games"   .= games ]
    toJSON (ChatToGame {..}) =
        object [ "message" .= ("chat_to_game" :: T.Text)
               , "instance_key" .= instanceKeyCTG
               , "speaker" .= speaker
               , "content" .= content ]

instance ToJSON GameEntry where
    toJSON (GameEntry {..}) =
        object [ "game_name" .= gameName
               , "instance_name" .= instanceName
               , "key" .= key ]

instance FromJSON Identity where
    parseJSON (Object v) = do
        msum [ User <$> v .: "user"
             , do True <- v .: "guest"
                  return Guest ]
    parseJSON _ = mzero

instance FromJSON ClientToServer where
    parseJSON (Object v) = do
        msg <- v .: "message"
        msum [ loginMsg msg
             , queryAllGamesMsg msg
             , joinGameMsg msg
             , chatGameMsg msg ]
      where
        loginMsg msg = do
            guard (msg == ("login" :: T.Text))
            iden <- v .: "identity"
            pass <-
                msum [ Just <$> (v .: "password")
                     , return Nothing ]

            return (Login { identity = iden
                          , password = pass })

        queryAllGamesMsg msg = do
            guard (msg == ("query_all_games" :: T.Text))
            return QueryAllGames

        joinGameMsg msg = do
            guard (msg == ("join_game" :: T.Text))
            key <- v .: "key"
            return JoinGame { keyJoinGame = key }

        chatGameMsg msg = do
            guard (msg == ("chat_to_game" :: T.Text))
            key <- v .: "instance_key"
            content <- v .: "content"
            return ChatToGameClient
                   { instanceKeyCTGClient = key
                   , contentClient = content }

    parseJSON _ = mzero

handshake :: ServerToClient
handshake = Handshake
    { majorVersion = 0
    , minorVersion = 1
    , patchVersion = 0 }

