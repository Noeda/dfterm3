{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable #-}

module Dfterm3.Server.Logic
    ( client )
    where

import Dfterm3.Game.DwarfFortress
import Dfterm3.UserAccounting
import Dfterm3.Dfterm3State
import Dfterm3.GameSubscription
import Dfterm3.Server.Types
import Data.Typeable
import Data.Maybe
import Data.List
import Data.Semigroup
import Control.Monad.State.Strict
import Control.Applicative
import Pipes hiding ( yield, await ) -- sneaky sneaky
import qualified Pipes as PC
import qualified Data.Text as T

data ClientState = ClientState
    { storage :: !Storage
    , sender  :: ServerToClient -> IO ()
    , userName :: T.Text
    , lastGameList :: [GameEntry] }
    deriving ( Typeable )

type Client a = StateT ClientState
                       (Consumer ClientToServer IO)
                       a

getStorage :: Client Storage
getStorage = storage <$> get

runSubIO :: SubscriptionIO a -> Client a
runSubIO action = do
    st <- getStorage
    liftIO $ runSubscriptionIO st action

client :: Storage
       -> (ServerToClient -> IO ())
       -> Consumer ClientToServer IO ()
client storage sender = void $ flip execStateT (ClientState
                                               { storage = storage
                                               , sender  = sender
                                               , lastGameList = [] }) $ do
    yield handshake

    -- login
    doLogin

    -- and then just event loop
    forever $ do
        msg <- await
        case msg of
            QueryAllGames -> queryAllGames
            JoinGame key -> joinGame key
            _ -> error "Invalid message from client."

joinGame :: T.Text -> Client ()
joinGame gamekey = do
    last_entries <- lastGameList <$> get
    case find ((== gamekey) . key) last_entries of
        Nothing ->
            yield (JoinAck { statusJoinAck = False
                           , noticeJoinAck =
                               "No such game." })
        Just entry -> do
            inst <- getStorage >>= liftIO . procureInstance (gameItself entry)
            case inst of
                Nothing ->
                    yield (JoinAck { statusJoinAck = False
                                   , noticeJoinAck =
                                       "Failed to procure an instance." })
                Just inst -> do
                    nm <- userName <$> get
                    result <- liftIO $ subscribe inst nm
                    case result of
                        Left err ->
                            yield (JoinAck { statusJoinAck = False
                                           , noticeJoinAck =
                                               T.pack (show err) })
                        Right subscription ->
                            -- TODO: Fork to handle subscription
                            yield (JoinAck { statusJoinAck = True
                                           , noticeJoinAck = "" })

queryAllGames :: Client ()
queryAllGames = do
    games <- keyify <$> (runSubIO lookForPotentialGames)
    let entries = fmap (\(key, game) ->
                    GameEntry
                        { gameName = "Dwarf Fortress"
                        , instanceName = Dfterm3.GameSubscription.gameName game
                        , gameItself = game
                        , key = key })
                       (games :: [(T.Text, DwarfFortressPersistent)])
    yield (AllGames entries)
    modify (\x -> x { lastGameList = entries })
  where
    keyify = zip (fmap (T.pack . show) [1..])

doLogin :: Client ()
doLogin = do
    Login {..} <- await
    if identity == Guest
      then do allow_guests <- getStorage >>= liftIO . areGuestsEnabled
              if not allow_guests
                then do yield (LoginAck
                                 { status = False
                                 , notice = "Guest logins are disabled." })
                        doLogin
                else yield (LoginAck
                              { status = True
                              , notice = "" })
      else case identity of
               User username -> do
                   if isNothing password
                     then do yield (LoginAck
                                      { status = False
                                      , notice = "Password required." })
                             doLogin
                     else do valid <-
                                 getStorage >>= liftIO .
                                 isValidLogin username (fromJust password)
                             if valid
                               then yield (LoginAck
                                             { status = True
                                             , notice = "" })
                               else do yield (LoginAck
                                              { status = False
                                              , notice = "Invalid username " <>
                                                         "or password." })
                                       doLogin
               _ -> error "impossible"

-- Fake pipes. Maybe no one notices they are not quite real.
yield :: ServerToClient -> Client ()
yield msg = do
    sd <- sender <$> get
    liftIO $ sd msg

await :: Client ClientToServer
await = lift $ PC.await

