{-# LANGUAGE DeriveGeneric, LambdaCase, RecordWildCards #-}
{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}

module Dfterm3.Playing.Common
    ( STCMessage()
    , CTSMessage()
    , runPlayingSession )
    where

import Dfterm3.Prelude
import Dfterm3.Game.TextGame
import Dfterm3.CP437ToUnicode
import Dfterm3.Terminal
import Dfterm3.Storage
import Dfterm3.Logging
import Dfterm3.Game
import Dfterm3.Game.DwarfFortress
import qualified Dfterm3.Chat as GS
import Data.Bits
import Data.Array.IArray ( assocs, bounds )
import Data.Aeson hiding ( (.=) )
import Data.Serialize.Put
import GHC.Generics
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Lens
import Control.Exception
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict hiding ( put, get )
import Control.Monad.RWS.Class
import Control.Monad.RWS.Strict hiding ( forM_, (<>) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Control.Concurrent
import System.Timeout

data CTSMessage =
    Unsubscribe
  | Chat !T.Text
  | DoInput !KeyDirection !Input
  | Subscribe !Int
  | ListGames
  | Authenticate { name :: T.Text
                 , password :: (Maybe T.Text) }
  deriving ( Eq, Ord, Show, Read, Typeable, Generic )

data STCMessage = LoginSuccessful
                | LoginFailed
                | CannotSubscribe !T.Text
                | Subscribed
                | GameClosed
                | Joined !T.Text
                | Parted !T.Text
                | Chatted !T.Text !T.Text
                | PlayerChanged !(Maybe T.Text)
                | GameChangeset !T.Text
                | Games ![(Int, T.Text)]
                deriving ( Eq, Ord, Show, Read, Typeable, Generic )

instance FromJSON CTSMessage
instance ToJSON CTSMessage
instance FromJSON STCMessage
instance ToJSON STCMessage

newtype Session a =
    Session { runSession ::
              RWST SessionEnv () SessionVar IO a }
    deriving ( Monad, MonadIO, Applicative, Functor, Typeable
             , MonadReader SessionEnv, MonadState SessionVar )

data SessionEnv = SessionEnv
    { sender :: STCMessage -> IO ()
    , receiver :: IO CTSMessage
    , killer :: IO ()
    , storage :: Storage
    , aliveThreads :: IORef (S.Set ThreadId) }

type TextGameSubscription = GameSubscription
                            (Either GS.ChatText TextGameInput)
                            (Either GS.ChatEvent TextGameChangesets)

data SessionVar = SessionVar
    { _lastSentGames :: !(IM.IntMap DwarfFortressPersistent)
    , _user :: !(Maybe UserInstance)
    , _subscriptions :: [ ( ThreadId
                          , TextGameSubscription ) ] }
makeLenses ''SessionVar

recv :: Session CTSMessage
recv = Session $ do
    SessionEnv { receiver = receiver } <- ask
    liftIO receiver

send :: STCMessage -> Session ()
send msg = Session $ do
    SessionEnv { sender = sender } <- ask
    liftIO $ sender msg

die :: Session a
die = Session $ do
    SessionEnv { killer = killer } <- ask
    liftIO $ do
        killer
        -- if we are still alive, make sure we die too
        myThreadId >>= killThread
        undefined -- never reached

-- Do something in a session within a time limit. If the limit is reached, kill
-- the session.
--
-- Time is in microseconds.
timeoutOrDie :: Int -> Session a -> Session a
timeoutOrDie useconds (Session rws) = Session $ do
    env <- ask
    st <- get
    maybe_result <- liftIO $ timeout useconds $ runRWST rws env st
    case maybe_result of
        Nothing -> runSession die
        Just (result, new_st, _) -> do
            put new_st
            return result

runPlayingSession :: (STCMessage -> IO ())
                  -> IO CTSMessage
                  -> IO ()
                  -> Storage
                  -> IO ()
runPlayingSession sender receiver killer ps = do
    alive_threads <- newIORef S.empty
    flip finally (do ts <- readIORef alive_threads
                     for_ ts killThread) $
        void $ execRWST session SessionEnv
                                { sender = sender
                                , receiver = receiver
                                , killer = killer
                                , storage = ps
                                , aliveThreads = alive_threads }
                                SessionVar
                                {
                                  _lastSentGames = IM.empty
                                , _subscriptions = []
                                , _user = Nothing
                                }
  where
    session = runSession $ do
        -- if handshake is not complete within 60 seconds, cut the connection.
        timeoutOrDie 60000000 handshake
        nonGameLoop

nonGameLoop :: Session ()
nonGameLoop = forever $ recv >>= \case
    ListGames -> listGames
    Subscribe key -> do
        subs <- use subscriptions
        if null subs
          then subscribeToGame key
          else send $ CannotSubscribe "Already subscribed to a game."
    Unsubscribe -> do
        subs <- use subscriptions
        for_ subs $ \(tid, _) -> liftIO $ killThread tid
        subscriptions .= []
    DoInput keydir input ->
        use subscriptions >>= \case
            [] -> return ()
            ((_, sub):_) -> doInput sub keydir input
    Chat msg ->
        use subscriptions >>= \case
            [] -> return ()
            ((_, sub):_) -> do
                liftIO $ atomically $ writeAction (InputAction $ Left $ T.take 800 msg) sub

    _ -> return ()

whenLoggedIn :: (T.Text -> Session ()) -> Session ()
whenLoggedIn action = do
    musr <- _user <$> get
    case musr of
        Nothing -> return ()
        Just usr -> do
            alive <- liftIO $ isAliveUser usr
            when alive $ action (viewName usr)

doInput :: TextGameSubscription
        -> KeyDirection
        -> Input
        -> Session ()
doInput sub keydir inp = do
    whenLoggedIn $ \name ->
        liftIO $ atomically $
            writeAction (InputAction $ Right $ MkTextGameInput keydir name inp) sub

subscribeToGame :: Int -> Session ()
subscribeToGame key = handle $ do
    sent_games <- lift $ _lastSentGames <$> get
    ps <- storage <$> lift ask

    -- The game must be in the list we sent to the client
    persistent <- case IM.lookup key sent_games of
        Nothing -> throwE $ CannotSubscribe "Invalid game key."
        Just p -> return p

    -- We have to properly procure a game instance for the client
    ginst <- liftIO $ procureTextGameInstance ps persistent
    ginstance <- case ginst of
        Failed -> throwE $ CannotSubscribe "Failed to procure a game."
        Instance ginstance -> return ginstance

    -- Attempt to subscribe the user to the just procured game
    lift $ whenLoggedIn $ \name -> do
        maybe_subscription <- liftIO $ subscribeToTextGame ps name ginstance
        case maybe_subscription of
            Nothing -> send $ CannotSubscribe $
                "Cannot subscribe to this game."
            Just subscription -> do
                send Subscribed
                SessionEnv { sender = sender } <- ask
                alive <- aliveThreads <$> ask
                tid <- liftIO $ mask $ \restore -> do
                    tid <- forkIO $
                            gameEventHandler restore
                                             alive
                                             subscription
                                             sender
                                             Nothing
                    atomicModifyIORef_' alive $ S.insert tid
                    return tid
                subscriptions %= (:) (tid, subscription)
  where
    handle action = do
        result <- runExceptT action
        case result of
            Left exc -> send exc
            Right r -> return r

data DieSilently = DieSilently
                   deriving ( Show, Typeable )

catchDieSilently :: IO a -> IO ()
catchDieSilently action = mask $ \restore -> do
    result <- try $ restore action
    case result of
        Left DieSilently -> return ()
        _ -> return ()

instance Exception DieSilently

gameEventHandler :: (IO () -> IO ())
                 -> IORef (S.Set ThreadId)
                 -> TextGameSubscription
                 -> (STCMessage -> IO ())
                 -> (Maybe T.Text)
                 -> IO ()
gameEventHandler restore alive_refs subscription sender last_player =
    catchDieSilently $ do
        tid <- myThreadId
        flip finally (atomicModifyIORef_' alive_refs $ S.delete tid) $ restore $
            void $ execStateT rec (True, last_player)
  where
    senderI = liftIO . sender

    rec = forever $ do
        event <- liftIO $ atomically $ do
            alive <- isSubscriptionAlive subscription
            if alive
              then do mevent <- readEvent subscription
                      case mevent of
                          Nothing -> retry
                          Just ev -> return $ Just ev
              else return Nothing

        ev <- case event of
            Nothing -> senderI GameClosed >> liftIO (throwIO DieSilently)
            Just ev -> return ev

        case ev of
            Changesets (Left (GS.Joined who)) -> senderI (Joined who)
            Changesets (Left (GS.Parted who)) -> senderI (Parted who)
            Changesets (Left (GS.Messaged (GS.ChatMessage who msg))) ->
                senderI (Chatted who msg)

            Changesets (Right (MkTextGameChangesets
                               terminal
                               changes
                               new_last_player)) -> do
                when (new_last_player /= last_player) $ do
                    senderI $ PlayerChanged new_last_player

                (first, _) <- get
                _1 .= False
                _2 .= new_last_player
                senderI $ GameChangeset $ T.decodeUtf8 $ B64.encode $ if first
                    then encodeStateToBinary terminal
                    else encodeChangesToBinary changes

            GameDied -> senderI GameClosed >> liftIO (throwIO DieSilently)

listGames :: Session ()
listGames = do
    ps <- storage <$> ask
    games <- liftIO $ listPublishedGames ps
    lastSentGames .= (IM.fromList $
                      zip [0..] (games :: [DwarfFortressPersistent]))

    send $ Games $ zip [0..] (fmap (^.customName) games)

handshake :: Session ()
handshake =
    recv >>= \case
        Authenticate {..} -> do
            st <- storage <$> ask
            if T.length name > 50
              then send LoginFailed >> handshake
              else do
                muserinstance <- liftIO $ case password of
                    Nothing -> loginNonPersistentUser name st
                    Just password' -> loginUser name password' st
                case muserinstance of
                    Nothing -> send LoginFailed >> handshake
                    Just userinstance -> do
                        liftIO $ logInfo $ "User with the name " <> show name <>
                                           " has logged in." <>
                                           if isNothing password
                                             then " (non-persistent)"
                                             else ""

                        user .= Just userinstance
                        send LoginSuccessful
        _ -> handshake

putScreenDataPrefix :: Int -> Int -> Int -> Put
putScreenDataPrefix w h num_cells = do
    putWord16be $ fromIntegral w
    putWord16be $ fromIntegral h
    putWord32be $ fromIntegral num_cells

serializeCellChange :: ((Int, Int), Cell) -> Put
serializeCellChange ((x, y), Cell code fcolor bcolor) = do
    putWord8 (unicodeToCP437 (T.head code))
    putWord8 $ colorSerialize fcolor .|.
               (colorSerialize bcolor `shift` 4)
    putWord16be (fromIntegral x)
    putWord16be (fromIntegral y)
  where
    colorSerialize :: ANSIColor -> Word8
    colorSerialize color = fromIntegral $
                           let intcolor = colorToInt color
                            in if intcolor == 16 then colorToInt White
                                                 else intcolor

encodeStateToBinary :: Terminal -> B.ByteString
encodeStateToBinary terminal =
    runPut $ do
        putWord8 1
        putScreenDataPrefix w h (w*h)
        for_ (tuckBounds left top $ assocs (terminal^.gridArray))
             serializeCellChange
  where
    ((left, top), _) = bounds $ terminal^.gridArray
    w = width terminal
    h = height terminal

encodeChangesToBinary :: TerminalChanges -> B.ByteString
encodeChangesToBinary (w, h, _, cell_changes) =
    runPut $ do
        putWord8 1
        putScreenDataPrefix w h num_changes
        for_ cell_changes serializeCellChange
  where
    num_changes = length cell_changes

