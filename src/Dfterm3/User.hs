-- | Tis module implements the user and security model.
--
-- The `acid-state` package is used to back all this data on the filesystem.
--

{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dfterm3.User
    ( openStorage
    , openAdmin
    , setAdminPassword
    , openAdminBySessionID
    , adminSessionID
    , periodicallyCleanStaleAdminHandles
    , registerDwarfFortress
    , unregisterDwarfFortress
    , listDwarfFortresses
    , expireAdmin
    , AddingResult(..)
    , UserSystem()
    , Dfterm3.User.Types.Admin() )
    where

import Dfterm3.Util ( whenJust )
import Dfterm3.DwarfFortress.Types

import Dfterm3.User.Types
import Dfterm3.Logging
import Data.Acid
import Control.Lens
import Control.Monad.State.Class
import Control.Monad ( void, forever )
import Control.Concurrent ( forkIO, ThreadId, threadDelay )
import System.Directory
import Control.Applicative ( (<$>) )
import Crypto.Scrypt
import Data.Time.Clock
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

#ifndef WINDOWS
import OpenSSL.Random ( randBytes )
#else
import System.Random

-- This is not quite secure but at least it works.
randBytes :: Int -> IO B.ByteString
randBytes 0 = return B.empty
randBytes x = do
    b <- randomIO
    fmap (B.singleton b `B.append`) $
         randBytes (x-1)
#endif



wasThisFirst :: Update UserSystemState Bool
wasThisFirst = do
    was_first <- use first
    modify (set first False)
    return was_first

getEncryptedAdminPassword :: Query UserSystemState (Maybe EncryptedPass)
getEncryptedAdminPassword = view adminPassword

setEncryptedAdminPassword :: Maybe EncryptedPass
                          -> Update UserSystemState ()
setEncryptedAdminPassword epass = adminPassword .= epass

addAdminSession :: Admin -> Update UserSystemState ()
addAdminSession admin = adminSessions %= M.insert (admin^.sessionID) admin

cleanStaleAdminSessions :: UTCTime -> Update UserSystemState ()
cleanStaleAdminSessions now =
    adminSessions %= M.filter (\x -> x^.validUntil >= now)

getAdminSessionByID :: B.ByteString -> Query UserSystemState (Maybe Admin)
getAdminSessionByID session_id = M.lookup session_id <$> view adminSessions

registerNewDwarfFortress :: DwarfFortress
                         -> Update UserSystemState AddingResult
registerNewDwarfFortress new_df = do
    old_df <- M.lookup exec <$> use dwarfFortressesWithPerms
    case old_df of
        Nothing -> do dwarfFortressesWithPerms %= M.insert exec df_with_perms
                      return New
        Just _  -> do dwarfFortressesWithPerms %= M.adjust (set rawDf new_df)
                                                           exec
                      return Modified
  where
    exec = new_df^.dfExecutable

    df_with_perms = DwarfFortressWithPerms
            { _rawDf = new_df
            , _dfEnabled = False
            , _allowWatchingByDefault = True
            , _allowPlayingByDefault = True
            , _allowChattingByDefault = True
            , _allowedWatchers = S.empty
            , _allowedPlayers = S.empty
            , _allowedChatters = S.empty
            , _disallowedWatchers = S.empty
            , _disallowedPlayers = S.empty
            , _disallowedChatters = S.empty }

unregisterSomeDwarfFortress :: String -> Update UserSystemState ()
unregisterSomeDwarfFortress executable =
    dwarfFortressesWithPerms %= M.delete executable

listAllDwarfFortresses :: Query UserSystemState [DwarfFortress]
listAllDwarfFortresses =
    (fmap _rawDf) . M.elems <$> view dwarfFortressesWithPerms

expireSomeAdmin :: Admin -> Update UserSystemState ()
expireSomeAdmin (Admin _ session_id) =
    adminSessions %= M.delete session_id

makeAcidic ''UserSystemState [ 'wasThisFirst, 'getEncryptedAdminPassword
                             , 'setEncryptedAdminPassword, 'addAdminSession
                             , 'cleanStaleAdminSessions
                             , 'registerNewDwarfFortress
                             , 'unregisterSomeDwarfFortress
                             , 'listAllDwarfFortresses
                             , 'getAdminSessionByID
                             , 'expireSomeAdmin ]

emptyUserSystem :: UserSystemState
emptyUserSystem = UserSystemState { _first = True
                                  , _dwarfFortressesWithPerms = M.empty
                                  , _adminPassword = Nothing
                                  , _adminSessions = M.empty
                                  , _registeredUsers = M.empty }

-- | Opens a storage for the system and returns a handle to the user system.
--
-- The argument is a directory. It will be created if it doesn't exist.
openStorage :: FilePath -> IO UserSystem
openStorage directory = do

    createDirectoryIfMissing True directory

    st <- openLocalStateFrom
              directory
              emptyUserSystem

    was_this_first <- update st WasThisFirst
    if was_this_first
      then logNotice $ "Created storage \'" ++ directory ++ "\'."
      else logInfo   $ "Opened storage \'" ++ directory ++ "\'."

    return $ UserSystem st

-- | Opens a thread that periodically cleans up stale admin handles.
--
-- It cleans up everything immediately after called and after that, waits for a
-- few minutes before doing it again.
periodicallyCleanStaleAdminHandles :: UserSystem -> IO ThreadId
periodicallyCleanStaleAdminHandles (UserSystem st) = forkIO $ forever $ do
    now <- getCurrentTime
    void $ update st $ CleanStaleAdminSessions now
    threadDelay 1800000000  -- 30 minutes

-- | Sets the administrator password to the UserSystem.
--
-- Make sure this cannot be called by anyone who is not supposed to.
setAdminPassword :: Maybe B.ByteString -> UserSystem -> IO ()
setAdminPassword password (UserSystem st) = do
    result <- case password of
                  Nothing -> return Nothing
                  Just  p -> Just <$> encryptPass' (Pass p)
    update st $ SetEncryptedAdminPassword result

-- | Opens an administrator handle to the UserSystem.
--
-- The verification of the password takes some time (by design). The password
-- is stored by using scrypt. It's also possible that no password is set. In
-- that case, `openAdmin` will never return you the admin handle.
--
-- The `Admin` handle will expire after the given number of seconds. Trying to
-- use the `Admin` handle after that will result in an error.
openAdmin :: B.ByteString    -- ^ Password to access the administrator
          -> Int             -- ^ How many seconds until the handle expires.
          -> UserSystem
          -> IO (Maybe Admin)  -- ^ Returns the admin if password was correct.
openAdmin password expiry (UserSystem st) = do
    encrypted_pass <- query st GetEncryptedAdminPassword
    case encrypted_pass of
        Nothing -> return Nothing
        Just epass -> checkPass epass
  where
    checkPass epass = do
        let ( succeeded, maybe_new_pass ) =
                verifyPass defaultParams (Pass password) epass
        if succeeded
          then do
                session_id <- makeSessionID
                now <- getCurrentTime
                let admin = Admin { _validUntil = fromIntegral expiry
                                                  `addUTCTime`
                                                  now
                                  , _sessionID = session_id }

                whenJust maybe_new_pass $ \new_pass ->
                    update st $ SetEncryptedAdminPassword (Just new_pass)
                void $ update st $ AddAdminSession admin

                return $ Just admin

          else return Nothing

-- | Returns the session ID of an admin handle. This can be uniquely identify a
-- handle.
adminSessionID :: Admin -> B.ByteString
adminSessionID = (^. sessionID)

-- | Immediately expires an admin.
expireAdmin :: Admin -> UserSystem -> IO ()
expireAdmin admin (UserSystem st) = update st $ ExpireSomeAdmin admin

-- | Returns an admin handle by its session ID, if the `Admin` is still
-- available.
openAdminBySessionID :: B.ByteString -> UserSystem -> IO (Maybe Admin)
openAdminBySessionID session_id (UserSystem st) = do
    maybe_admin <- query st $ GetAdminSessionByID session_id
    now <- getCurrentTime
    return $ case maybe_admin of
                 Nothing -> Nothing
                 Just admin ->
                     if admin^.validUntil >= now
                       then Just admin
                       else Nothing

makeSessionID :: IO B.ByteString
makeSessionID = randBytes 33   -- I made up this number all by myself

-- | Registers a Dwarf Fortress to the user system.
--
-- Replaces an old Dwarf Fortress if the executable is the same as some other
-- Dwarf Fortress added earlier. (That is, Dwarf Fortresses are indexed by
-- their executable path.)
registerDwarfFortress :: FilePath  -- ^ Path to the Dwarf Fortress executable.
                      -> [String]  -- ^ Command line arguments to the Dwarf
                                   -- Fortress. You don't need to supply the
                                   -- first argument that is the program name
                                   -- as that will be done for you.
                      -> FilePath  -- ^ Working directory for Dwarf Fortress.
                      -> String    -- ^ Arbitrary name for the game.
                      -> UserSystem
                      -> IO AddingResult
registerDwarfFortress executable args working_directory name (UserSystem st) =
 do executable' <- canonicalizePath executable
    working_directory' <- canonicalizePath working_directory
    update st $ RegisterNewDwarfFortress $
        DwarfFortress executable'
                      args
                      working_directory'
                      (T.pack name)

-- | Unregisters a Dwarf Fortress from the system. The given argument is the
-- executable name of the Dwarf Fortress.
unregisterDwarfFortress :: FilePath -> UserSystem -> IO ()
unregisterDwarfFortress executable (UserSystem st) =
    update st $ UnregisterSomeDwarfFortress executable

-- | Returns all registered Dwarf Fortresses in the system.
listDwarfFortresses :: UserSystem -> IO [DwarfFortress]
listDwarfFortresses (UserSystem st) =
    query st ListAllDwarfFortresses

