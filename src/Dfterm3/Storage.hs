module Dfterm3.Storage
    ( Storage()
    , newMemoryStorage
    , registerTextGame
    , isAliveUser
    , loginUser
    , loginNonPersistentUser
    , registerUser
    , unloginUser
    , viewName
    , procureTextGameInstance
    , subscribeToTextGame
    , listPublishedGames
    , UserInstance() )
    where

import Dfterm3.Prelude
import Dfterm3.Game
import Dfterm3.Game.TextGame
import Dfterm3.Chat
import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Crypto.Scrypt
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Data.Map.Strict as M
import Data.Unique
import Data.Serialize.Get
import Data.SafeCopy
import System.Mem.Weak
import qualified Data.ByteString as B

newtype Storage = MkStorage (MVar Storage_)

data Storage_ = MkStorage_
    { _persistentStorage :: !PersistentStorage
    , _runningInstances :: !(M.Map B.ByteString
                             (STM (STM (GameEvent TextGameChangesets))
                             ,GameAction TextGameInput -> STM ()
                             ,ChatRoom))
    , _aliveUsers ::
      !(M.Map T.Text (M.Map Unique (Weak (IORef UserInstance_)))) }
    deriving ( Typeable )

data PersistentStorage = MkPersistentStorage
    { _userInfo :: !(M.Map T.Text UserInfo)
    , _textGames :: !(M.Map GameKey (GameWideKey, B.ByteString))
    , _registrationOpen :: !Bool }
    deriving ( Eq, Show, Typeable )

data UserInfo = MkUserInfo
    { _password :: !(Maybe EncryptedPass)
    , _admin :: !Bool }
    deriving ( Eq, Show, Typeable )

data UserInstance = MkUserInstance
    { _userInstanceId :: !Unique
    , _userInstanceStorage :: !Storage
    , _userInstanceName :: !T.Text
    , _userInstanceRef :: !(IORef UserInstance_) }
    deriving ( Typeable )

viewName :: UserInstance -> T.Text
viewName = _userInstanceName

-- these instances are sane as long as we don't export anything that lets
-- someone modify a `UserInstance`.
instance Eq UserInstance where
    ui1 == ui2 = _userInstanceId ui1 == _userInstanceId ui2
    {-# INLINE (==) #-}
instance Ord UserInstance where
    ui1 `compare` ui2 = _userInstanceId ui1 `compare` _userInstanceId ui2
    {-# INLINE compare #-}

data UserInstance_ = MkUserInstance_
    { _alive :: !Bool }
    deriving ( Eq, Ord, Show, Read, Typeable )
makeLenses ''Storage_
makeLenses ''PersistentStorage
makeLenses ''UserInfo
makeLenses ''UserInstance
makeLenses ''UserInstance_

newMemoryStorage :: IO Storage
newMemoryStorage = MkStorage <$> do
    newMVar MkStorage_
        { _persistentStorage = emptyPersistentStorage
        , _runningInstances = M.empty
        , _aliveUsers = M.empty }

emptyPersistentStorage :: PersistentStorage
emptyPersistentStorage = MkPersistentStorage
    { _userInfo = M.empty
    , _textGames = M.empty
    , _registrationOpen = True }

registerTextGame :: TextGame game
                 => game
                 -> Storage
                 -> IO ()
registerTextGame game (MkStorage storage) = undefined

registerUser :: T.Text -> T.Text -> Storage -> IO Bool
registerUser name pass (MkStorage mvar) = mask_ $
    modifyMVar mvar $ \old ->
        if old ^. persistentStorage.registrationOpen &&
           isNothing (old ^. persistentStorage.userInfo.at name) &&
           isNothing (old ^. aliveUsers.at name)
          then doRegistration old
          else return (old, False)
  where
    doRegistration old = do
        pass <- encryptPassIO defaultParams (Pass $ T.encodeUtf8 pass)
        return ( old &
                 persistentStorage.userInfo.at name .~
                 (Just $ MkUserInfo { _password = Just pass
                                    , _admin = False })
               , True )

isAliveUser :: UserInstance -> IO Bool
isAliveUser (MkUserInstance _ _ _ ref) = (^.alive) <$> readIORef ref

unloginUser :: UserInstance -> IO ()
unloginUser (MkUserInstance uniq (MkStorage mvar) name ref) = mask_ $ do
    modifyMVar_ mvar $ return . removeAliveUser name uniq
    atomicModifyIORef' ref $ \old ->
        ( old & alive .~ False, () )

removeAliveUser :: T.Text -> Unique -> Storage_ -> Storage_
removeAliveUser name uniq =
    aliveUsers %~
    (\m ->
        case M.lookup name m of
            Nothing -> m
            Just uniq_map ->
               let new_uniq_map = M.delete uniq uniq_map
                in if M.null new_uniq_map
                    then M.delete name m
                    else M.insert name new_uniq_map m)

loginNonPersistentUser :: T.Text -> Storage -> IO (Maybe UserInstance)
loginNonPersistentUser name storage@(MkStorage mvar) = mask_ $
    modifyMVar mvar $ \old -> do
        if isJust $ old ^.persistentStorage.userInfo.at name
          then return ( old, Nothing )
          else do usr <- newIORef MkUserInstance_ { _alive = True }
                  uniq <- newUnique
                  weak_ref <- mkWeakIORef usr $ modifyMVar_ mvar $ return .
                      removeAliveUser name uniq
                  let user_instance = MkUserInstance
                                      { _userInstanceId = uniq
                                      , _userInstanceRef = usr
                                      , _userInstanceName = name
                                      , _userInstanceStorage = storage }
                  return ( old &
                           aliveUsers %~ M.alter
                              (\case
                                   Nothing -> Just $
                                       M.singleton uniq weak_ref
                                   Just um -> Just $
                                       M.insert uniq weak_ref um)
                              name
                         , Just user_instance )

loginUser :: T.Text -> T.Text -> Storage -> IO (Maybe UserInstance)
loginUser name pass storage@(MkStorage mvar) = mask_ $
    modifyMVar mvar $ \old -> do
        case _password <$> old ^. persistentStorage.userInfo.at name of
            Just (Just encrypted_pass) -> loginIfVerified encrypted_pass old
            _ -> return (old, Nothing)

  where
    loginIfVerified :: EncryptedPass
                    -> Storage_
                    -> IO (Storage_, Maybe UserInstance)
    loginIfVerified encrypted_pass old =
        if not valid
          then return ( old, Nothing )
          else do usr <- newIORef MkUserInstance_ { _alive = True }
                  uniq <- newUnique
                  -- hang a finalizer that removes the user instance from the
                  -- system eventually; if they are not removed normally
                  weak_ref <- mkWeakIORef usr $ modifyMVar_ mvar $ return .
                      removeAliveUser name uniq
                  let user_instance = MkUserInstance
                                      { _userInstanceId = uniq
                                      , _userInstanceRef = usr
                                      , _userInstanceName = name
                                      , _userInstanceStorage = storage }
                  return ( old &
                           (aliveUsers %~ M.alter
                               (\case
                                    Nothing -> Just $
                                        M.singleton uniq weak_ref
                                    Just um -> Just $
                                        M.insert uniq weak_ref um)
                               name) .
                           case new_pass of
                               Nothing -> id
                               Just pp ->
                                 (persistentStorage.
                                  userInfo.
                                  at name.
                                  _Just.
                                  password
                                  .~ Just pp)
                         , Just user_instance )

      where
        (valid, new_pass) = verifyPass defaultParams
                                       (Pass $ T.encodeUtf8 pass)
                                       encrypted_pass

listPublishedGames :: forall a. TextGame a
                   => Storage
                   -> IO [a]
listPublishedGames (MkStorage mvar) = withMVar mvar $ \st -> return $ catMaybes $
    flip fmap (M.assocs $ _textGames $ _persistentStorage st) $
        \(gamekey, (gamewidekey, bin)) ->
        if gamewidekey == key
          then Just $ case runGet safeGet bin of
                          Left err -> error $
                              "Cannot unserialize game description, " <>
                              "incompatible migration? (" <> err <> ")"
                          Right ok -> ok
          else Nothing
  where
    key = uniqueGameWideKey (undefined :: a)

subscribeToTextGame :: TextGame a
                    => Storage
                    -> T.Text
                    -> (GameRawInstance a)
                    -> IO (Maybe
                           (GameSubscription
                            (Either ChatText TextGameInput)
                            (Either ChatEvent TextGameChangesets)))
subscribeToTextGame (MkStorage mvar) name raw_inst =
    modifyMVar mvar $ \old ->
        case M.lookup (uniqueInstanceKey raw_inst) (old^.runningInstances) of
            Nothing -> return ( old, Nothing )
            Just (recv_stm, sender, chat_room) -> do
                recv_action <- atomically recv_stm
                result' <- newGameSubscription sender
                                               recv_action

                (send_to_chat, receive_from_chat) <- joinChatRoom chat_room
                let result = stmMorphGameSubscription
                        (\tgi -> case tgi of
                            Left ct -> send_to_chat (ChatMessage name ct) *>
                                       return Nothing
                            Right x -> return $ Just x)
                        (\old_stm ->
                            orElse (do result <- readTChan receive_from_chat
                                       return $ Changesets $ Left result)
                                   (do result <- old_stm
                                       return $ fmap Right result))
                        result'
                return ( old, Just result )

procureTextGameInstance :: forall a. TextGame a
                        => Storage
                        -> a
                        -> IO (Procurement a)
procureTextGameInstance (MkStorage mvar) textgame = do
    -- don't waste cycles trying to procure a game if the game is not
    -- registered.
    result <- withMVar mvar $ return . isMember
    if not result
      then return Failed
      else do
        event_chan <- newBroadcastTChanIO
        action_chan <- newTChanIO
        let receiver = do new_chan <- dupTChan event_chan
                          return $ fmap toTextGameChangesets <$>
                                        readTChan new_chan
            sender action = writeTChan action_chan
                                       (fromTextGameInput <$> action)

        procurement <- procureInstance_ textgame event_chan action_chan
        case procurement of
            Failed -> return Failed
            Instance raw_inst -> do
                -- add the game to running instances...if that's allowed
                room <- newChatRoom
                modifyMVar_ mvar $ \old ->
                    if isMember old
                      then return $ old & runningInstances %~
                                    M.insert (uniqueInstanceKey raw_inst)
                                             (receiver, sender, room)
                      else stopInstance raw_inst >> return old
                return $ Instance raw_inst
  where
    isMember old = M.member
                   (uniqueKey textgame)
                   (old ^. persistentStorage.textGames)

