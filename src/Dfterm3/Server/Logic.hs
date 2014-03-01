{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable #-}

module Dfterm3.Server.Logic
    ( client )
    where

import Prelude hiding ( mapM_ )
import Dfterm3.Game.DwarfFortress
import Dfterm3.UserAccounting
import Dfterm3.Dfterm3State
import Dfterm3.GameSubscription
import Dfterm3.Server.Types
import Data.Foldable
import Data.Typeable
import Data.Maybe
import Data.IORef
import Data.Semigroup
import Data.Char
import System.Random
import Control.Monad.Catch
import Control.Monad.State.Strict hiding ( mapM_ )
import Control.Applicative
import Control.Concurrent hiding ( yield )
import Pipes hiding ( yield, await ) -- sneaky sneaky
import Pipes.Safe ()
import qualified Pipes as PC
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Subscription = Subscription
    { thread :: ThreadId
    , sub    :: GameSubscription DwarfFortressPersistent
    , key    :: T.Text }
    deriving ( Typeable )

data ClientState = ClientState
    { storage :: !Storage
    , sender  :: ServerToClient -> IO ()
    , userName :: T.Text
    , subscriptions :: IORef (M.Map T.Text Subscription)
    , lastGameList :: [(GameEntry, DwarfFortressPersistent)] }
    deriving ( Typeable )

type Client a = StateT ClientState
                       (Consumer ClientToServer IO)
                       a

forkClientThread :: T.Text
                 -> GameSubscription DwarfFortressPersistent
                 -> IO ()
                 -> Client ()
forkClientThread key subscription action = do
    ref <- subscriptions <$> get
    liftIO $ mask_ $ do
        mvar <- newEmptyMVar
        tid <- forkIOWithUnmask $ \unmask -> do
            () <- takeMVar mvar
            finally (unmask action)
                    (atomicModifyIORef' ref $ \old -> (M.delete key old, () ))
        old_result <-
            atomicModifyIORef' ref $ \old -> ( M.insert key
                                               Subscription
                                               { thread = tid
                                               , sub = subscription
                                               , key = key } old
                                             , M.lookup key old )
        case old_result of
            Just old -> killThread (thread old)
            _ -> return ()
        putMVar mvar ()

getStorage :: Client Storage
getStorage = storage <$> get

runSubIO :: SubscriptionIO a -> Client a
runSubIO action = do
    st <- getStorage
    liftIO $ runSubscriptionIO st action

-- multithread proof an IO action.
--
-- the returned action behaves like given action but will use locking to make
-- sure it is not called simultaneously.
--
-- Note that this also makes the resulting action subject to asynchronous
-- exceptions.
mtProof :: (a -> IO b) -> IO (a -> IO b)
mtProof action = do
    mvar <- newMVar ()
    return $ \x -> withMVar mvar $ \_ -> action x

client :: Storage
       -> (ServerToClient -> IO ())
       -> Consumer ClientToServer IO ()
client storage sender' = do
    sender <- liftIO $ mtProof sender'
    ref <- liftIO $ newIORef M.empty
    flip finally (liftIO $ readIORef ref >>= mapM_ (killThread . thread)) $
                 client' ref sender
  where
    client' ref sd = void $ flip execStateT (ClientState
                                     { storage = storage
                                     , sender  = sd
                                     , subscriptions = ref
                                     , userName = ""
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
                ChatToGameClient key content -> chatToGame key content
                _ -> error "Invalid message from client."

chatToGame :: T.Text -> T.Text -> Client ()
chatToGame key content = do
    subscriptions <- liftIO . readIORef =<< subscriptions <$> get
    case M.lookup key subscriptions of
        Nothing -> return ()
        Just subscription -> liftIO $ chat (sub subscription) content

joinGame :: T.Text -> Client ()
joinGame gamekey = do
    last_entries <- lastGameList <$> get
    subscribe_key <- makeRandomInstanceKey
    let ack = ack' subscribe_key
    case find ((== gamekey) . Dfterm3.Server.Types.key . fst) last_entries of
        Nothing ->
            yield (ack { noticeJoinAck = "No such game." })
        Just entry -> do
            inst <- getStorage >>= liftIO . procureInstance (snd entry)
            case inst of
                Nothing ->
                    yield (ack { noticeJoinAck =
                                   "Failed to procure an instance." })
                Just inst -> do
                    nm <- userName <$> get
                    result <- liftIO $ subscribe inst nm
                    case result of
                        Left err ->
                            yield (ack { noticeJoinAck =
                                         T.pack (show err) })
                        Right subscription -> do
                            sd <- sender <$> get
                            yield (ack { statusJoinAck = True
                                       , noticeJoinAck = "" })
                            forkClientThread
                                subscribe_key
                                subscription
                                (subscriptionHandling subscription
                                                      subscribe_key
                                                      sd)
  where
    ack' key = JoinAck { statusJoinAck = False
                       , instanceKeyJoinAck = key
                       , noticeJoinAck = "" }

makeRandomInstanceKey :: Client T.Text
makeRandomInstanceKey = do
    first <- liftIO randomCh
    rec (T.singleton first)
  where
    randomCh = do
        ch <- randomRIO (33, 80) -- There's no meaning to this range other than
                                 -- 33 is just above space bar so these are
                                 -- printable ASCII characters.
        return (chr ch)

    rec accum = do
        st <- liftIO . readIORef =<< subscriptions <$> get
        case M.lookup accum st of
            Nothing -> return accum
            Just _  -> do
                second <- T.singleton <$> liftIO randomCh
                rec (accum <> second)

subscriptionHandling :: GameSubscription DwarfFortressPersistent
                     -> T.Text
                     -> (ServerToClient -> IO ())
                     -> IO ()
subscriptionHandling subscription key sender = rec
  where
    rec = do
        event <- waitForEvent subscription
        case event of
            InstanceClosed -> return ()
            ChatEvent cevent -> handleChatEvent cevent *> rec
            GameChangesets changesets -> handleChangesets changesets *> rec

    -- Right now, scrap the join/part messages
    handleChatEvent (Joined _) = return ()
    handleChatEvent (Parted _) = return ()
    handleChatEvent (ChatMessage name content) =
        sender (ChatToGame
                { instanceKeyCTG = key
                , speaker = name
                , content = content })

    handleChangesets _ = return ()

queryAllGames :: Client ()
queryAllGames = do
    games <- keyify <$> (runSubIO lookForPotentialGames)
    let entries = fmap (\(key, game) ->
                    ( GameEntry
                         { gameName = "Dwarf Fortress"
                         , instanceName = Dfterm3.GameSubscription.gameName game
                         , key = key }
                    , game ))
                       (games :: [(T.Text, DwarfFortressPersistent)])
    yield (AllGames $ fmap fst entries)
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
                                             , notice = "" }) *>
                                    modify (\old ->
                                               old { userName = username })
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

