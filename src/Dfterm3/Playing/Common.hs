{-# LANGUAGE DeriveGeneric, LambdaCase, RecordWildCards #-}

module Dfterm3.Playing.Common
    ( STCMessage()
    , CTSMessage()
    , runPlayingSession )
    where

import Dfterm3.Prelude
import Dfterm3.User
import Dfterm3.Dfterm3State.Internal.Types
import Dfterm3.Game.DwarfFortress
import Dfterm3.GameSubscription hiding ( Joined, Parted )
import Dfterm3.CP437ToUnicode
import qualified Dfterm3.GameSubscription as GS
import Dfterm3.Terminal
import Data.Bits
import Data.Array.IArray ( assocs, bounds )
import Data.Aeson hiding ( (.=) )
import Data.Serialize.Put
import GHC.Generics
import Control.Monad.IO.Class
import Control.Lens
import Control.Exception
import Control.Monad.Trans.State.Strict hiding ( put, get )
import Control.Monad.RWS.Class
import Control.Monad.RWS.Strict hiding ( forM_ )
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
  | Authenticate { name :: T.Text }
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
    , aliveThreads :: IORef (S.Set ThreadId)
    , user :: User }

data SessionVar = SessionVar
    { _lastSentGames :: IM.IntMap DwarfFortressPersistent
    , _subscriptions :: [ ( ThreadId
                          , GameSubscription DwarfFortressPersistent) ] }
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
    usr <- liftIO $ newGuestUser ps
    alive_threads <- newIORef S.empty
    flip finally (do ts <- readIORef alive_threads
                     for_ ts killThread) $
        void $ execRWST session SessionEnv
                                { sender = sender
                                , receiver = receiver
                                , killer = killer
                                , storage = ps
                                , aliveThreads = alive_threads
                                , user = usr }
                                SessionVar
                                {
                                  _lastSentGames = IM.empty
                                , _subscriptions = []
                                }
  where
    session = runSession $ do
        -- if handshake is not complete within 10 seconds, cut the connection.
        timeoutOrDie 10000000 handshake
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
            ((_, sub):_) -> liftIO $ chat sub (T.take 800 msg)

    _ -> return ()

doInput :: GameSubscription DwarfFortressPersistent
        -> KeyDirection
        -> Input
        -> Session ()
doInput sub keydir inp = do
    usr <- user <$> ask
    whenLoggedIn usr $ \name ->
        liftIO $ input sub (DwarfFortressInput keydir name inp)

subscribeToGame :: Int -> Session ()
subscribeToGame key = do
    sent_games <- use lastSentGames
    ps <- storage <$> ask
    case IM.lookup key sent_games of
        Nothing -> send $ CannotSubscribe "Invalid game key."
        Just persistent -> do
            maybe_ginstance <- liftIO $ procureInstance persistent ps
            case maybe_ginstance of
                Nothing -> send $ CannotSubscribe "Failed to procure a game."
                Just ginstance -> do
                    usr <- user <$> ask
                    whenLoggedIn usr $ \name -> do
                        maybe_subscription <-
                            liftIO $ subscribe ginstance name
                        case maybe_subscription of
                            Left fail -> send $ CannotSubscribe $ showT fail
                            Right subscription -> do
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

gameEventHandler :: (IO () -> IO ())
                 -> IORef (S.Set ThreadId)
                 -> GameSubscription DwarfFortressPersistent
                 -> (STCMessage -> IO ())
                 -> (Maybe T.Text)
                 -> IO ()
gameEventHandler restore alive_refs subscription sender last_player = do
    tid <- myThreadId
    flip finally (atomicModifyIORef_' alive_refs $ S.delete tid) $ restore $
        void $ flip execStateT (True, True, last_player) $ loop
  where
    loop = do
        rec
        (_, should_continue, _) <- get
        when should_continue loop

    senderI = liftIO . sender

    rec = do
        event <- liftIO $ waitForEvent subscription
        case event of
            InstanceClosed -> senderI GameClosed

            ChatEvent (GS.Joined who) -> senderI (Joined who)
            ChatEvent (GS.Parted who) -> senderI (Parted who)
            ChatEvent (ChatMessage who msg) -> senderI (Chatted who msg)

            GameChangesets (DwarfFortressChangesets terminal
                                                    changes
                                                    new_last_player) -> do
                when (new_last_player /= last_player) $ do
                    senderI $ PlayerChanged new_last_player

                (first, _, _) <- get
                _1 .= False
                _3 .= new_last_player
                senderI $ GameChangeset $ T.decodeUtf8 $ B64.encode $ if first
                    then encodeStateToBinary terminal
                    else encodeChangesToBinary changes

listGames :: Session ()
listGames = do
    ps <- storage <$> ask
    games <- liftIO $
        runSubscriptionIO ps
        (lookForPublishedGames :: SubscriptionIO [DwarfFortressPersistent])

    lastSentGames .= (IM.fromList $ zip [0..] games)

    send $ Games $ zip [0..] (fmap (^.customName) games)

handshake :: Session ()
handshake =
    recv >>= \case
        Authenticate {..} -> do
            usr <- Session $ user <$> ask
            st <- Session $ storage <$> ask
            x <- liftIO $ loginUser usr name st
            if x
              then send LoginSuccessful
              else send LoginFailed >> handshake
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

