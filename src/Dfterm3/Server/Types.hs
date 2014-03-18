{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Dfterm3.Server.Types
    ( ServerToClient(..)
    , ClientToServer(..)
    , GameEntry(..)
    , AdminGameEntry(..)
    , Identity(..)
    , handshake )
    where

import Data.Aeson
import qualified Data.Text as T

import Data.Typeable
import Data.Semigroup
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
    { games  :: [Either GameEntry AdminGameEntry] }
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
    , key :: T.Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data AdminGameEntry =
    AdminGameEntry
    { gameNameA :: T.Text
    , instanceNameA :: T.Text
    , keyA :: T.Text
    , madeAvailable :: Bool
    , handler :: Maybe T.Text }
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
               , "instance_key" .= instanceKeyJoinAck
               , "notice"  .= noticeJoinAck ]
    toJSON (AllGames {..}) =
        object [ "message" .= ("all_games" :: T.Text)
               , "games"   .=
                   -- Avoid going through Either for aeson
                   fmap (\case
                       Left l ->  toJSON l
                       Right r -> toJSON r) games ]
    toJSON (ChatToGame {..}) =
        object [ "message" .= ("chat_to_game" :: T.Text)
               , "instance_key" .= instanceKeyCTG
               , "speaker" .= speaker
               , "content" .= content ]

instance FromJSON ServerToClient where
    parseJSON (Object v) = do
        msg <- v .: "message"
        msum [ parseHandshake msg
             , parseLoginAck msg
             , parseJoinAck msg
             , parseAllGames msg
             , parseChatToGame msg ]
      where
        parseHandshake msg = do
            guard (msg == ("handshake" :: T.Text))
            server <- v .: "server"
            guard (server == ("dfterm3" :: T.Text))
            major <- v .: "major"
            minor <- v .: "minor"
            patch <- v .: "patch"
            return Handshake { majorVersion = major
                             , minorVersion = minor
                             , patchVersion = patch }

        parseLoginAck msg = do
            guard (msg == ("login_acknowledgement" :: T.Text))
            status <- v .: "status"
            notice <- v .: "notice"
            return LoginAck { status = status
                            , notice = notice }

        parseJoinAck msg = do
            guard (msg == ("join_acknowledgement" :: T.Text))
            status <- v .: "status"
            notice <- v .: "notice"
            key <- v .: "instance_key"
            return JoinAck { statusJoinAck = status
                           , noticeJoinAck = notice
                           , instanceKeyJoinAck = key }

        parseAllGames msg = do
            guard (msg == ("all_games" :: T.Text))
            games <- v .: "games"
            return AllGames { games = games }

        parseChatToGame msg = do
            guard (msg == ("chat_to_game" :: T.Text))
            key <- v .: "instance_key"
            speaker <- v .: "speaker"
            content <- v .: "content"
            return ChatToGame { instanceKeyCTG = key
                              , speaker = speaker
                              , content = content }

    parseJSON _ = mzero

instance FromJSON GameEntry where
    parseJSON (Object v) = do
        game_name <- v .: "game_name"
        instance_name <- v .: "instance_name"
        key <- v .: "key"
        return GameEntry { gameName = game_name
                         , instanceName = instance_name
                         , key = key }

    parseJSON _ = mzero

instance FromJSON AdminGameEntry where
    parseJSON (Object v) = do
        game_name <- v .: "game_name"
        instance_name <- v .: "instance_name"
        key <- v .: "key"
        handler <- v .: "handler"
        made_available <- v .: "made-available"
        return AdminGameEntry
            { gameNameA = game_name
            , instanceNameA = instance_name
            , keyA = key
            , handler = handler
            , madeAvailable = made_available }

    parseJSON _ = mzero

instance ToJSON GameEntry where
    toJSON (GameEntry {..}) =
        object [ "game_name" .= gameName
               , "instance_name" .= instanceName
               , "key" .= key ]

instance ToJSON AdminGameEntry where
    toJSON (AdminGameEntry {..}) =
        object [ "game_name" .= gameNameA
               , "instance_name" .= instanceNameA
               , "key" .= keyA
               , "made-available" .= madeAvailable
               , "handler" .= handler ]

instance FromJSON Identity where
    parseJSON (Object v) = do
        msum [ User <$> v .: "user"
             , do True <- v .: "guest"
                  return Guest ]
    parseJSON _ = mzero

instance ToJSON ClientToServer where
    toJSON (Login {..}) =
        object $ (case password of
                    Nothing -> []
                    Just pw -> [ "password" .= pw ]) <>
                 [ "message" .= ("login" :: T.Text)
                 , "identity" .= identity ]

    toJSON QueryAllGames =
        object [ "message" .= ("query_all_games" :: T.Text) ]

    toJSON (JoinGame {..}) =
        object [ "message" .= ("join_game" :: T.Text)
               , "key"     .= keyJoinGame ]

    toJSON (ChatToGameClient {..}) =
        object [ "message" .= ("chat_to_game" :: T.Text)
               , "instance_key" .= instanceKeyCTGClient
               , "content"      .= contentClient ]

instance ToJSON Identity where
    toJSON (User x) = object [ "user" .= x ]
    toJSON Guest    = object [ "guest" .= True ]

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

