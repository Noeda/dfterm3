-- | This module implements the user and security model.
--
-- The `acid-state` package is used to back all this data on the filesystem.
--

{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}

module Dfterm3.User
    ( openStorage
    , openAdmin
    , setAdminPassword
    , UserSystem() )
    where

import Dfterm3.User.Types
import Dfterm3.Logging
import Data.Acid
import Control.Lens
import Control.Monad.State.Class
import System.Directory
import Control.Applicative ( (<$>) )
import Crypto.Scrypt
import qualified Data.ByteString as B

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

makeAcidic ''UserSystemState [ 'wasThisFirst, 'getEncryptedAdminPassword
                             , 'setEncryptedAdminPassword ]

emptyUserSystem :: UserSystemState
emptyUserSystem = UserSystemState { _first = True
                                  , _adminPassword = Nothing }

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
openAdmin :: B.ByteString    -- ^ Password to access the administrator
          -> UserSystem
          -> IO (Maybe Admin)  -- ^ Returns the admin if password was correct.
openAdmin password (UserSystem st) = do
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
                whenJust maybe_new_pass $ \new_pass ->
                    update st $ SetEncryptedAdminPassword (Just new_pass)
                return $ Just Admin


