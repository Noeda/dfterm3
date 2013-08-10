-- | This module implements the user and security model.
--
-- The `acid-state` package is used to back all this data on the filesystem.
--

{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}

module Dfterm3.User
    ( openStorage
    , openAdmin
    , setAdminPassword
    , adminSessionID
    , periodicallyCleanStaleAdminHandles
    , UserSystem()
    , Dfterm3.User.Types.Admin() )
    where

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
import OpenSSL.Random ( randBytes )
import Data.Time.Clock
import qualified Data.ByteString as B
import qualified Data.Map as M

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
cleanStaleAdminSessions now = do
    adminSessions %= M.filter (\x -> x^.validUntil >= now)

makeAcidic ''UserSystemState [ 'wasThisFirst, 'getEncryptedAdminPassword
                             , 'setEncryptedAdminPassword, 'addAdminSession
                             , 'cleanStaleAdminSessions ]

emptyUserSystem :: UserSystemState
emptyUserSystem = UserSystemState { _first = True
                                  , _adminPassword = Nothing
                                  , _adminSessions = M.empty }

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

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust Nothing _ = return ()
whenJust (Just x) action = action x

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
        case succeeded of
            False -> return Nothing
            True -> do
                session_id <- makeSessionID
                now <- getCurrentTime
                let admin = Admin { _validUntil = (fromIntegral expiry)
                                                  `addUTCTime`
                                                  now
                                  , _sessionID = session_id }

                whenJust maybe_new_pass $ \new_pass ->
                    update st $ SetEncryptedAdminPassword (Just new_pass)
                void $ update st $ AddAdminSession admin

                return $ Just admin

-- | Returns the session ID of an admin handle. This can be uniquely identify a
-- handle.
adminSessionID :: Admin -> B.ByteString
adminSessionID = (^. sessionID)

makeSessionID :: IO B.ByteString
makeSessionID = do
    session_id <- randBytes 33   -- I made up this number all by myself
    return session_id


