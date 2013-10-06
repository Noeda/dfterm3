-- | Module that contains functions to deal with authentication. It maintains
-- session IDs in persistent storage and saves and checks passwords.
--

{-# LANGUAGE ViewPatterns, CPP #-}

module Dfterm3.Admin
    ( newSessionByPassword
    , SessionID()
    , sessionIDToByteString
    , invalidateSessionID
    , isValidSessionID
    , byteStringToSessionID
    , setAdminPassword
    , setNoAuthentication
    , setNoAuthentication'
    , hasNoAuthentication
    , changePassword )
    where

import Dfterm3.Admin.Internal.Types
import Dfterm3.Dfterm3State.Internal.Types
import Dfterm3.Dfterm3State.Internal.Transactions
import Data.Acid
import Data.Time
import qualified Data.ByteString as B

import Crypto.Scrypt

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

-- | Attempts to make a new admin session by checking a password.
--
-- Returns `Nothing` if authentication fails for some reason (incorrect
-- password or disabled password).
--
-- The returned `SessionID` is used to perform administrator actions.
newSessionByPassword :: B.ByteString           -- ^ The password.
                     -> Int                    -- ^ How many seconds the
                                               --   session ID should be valid.
                     -> Storage
                     -> IO (Maybe SessionID)
newSessionByPassword password expiry_time (readPersistentStorage -> ps) = do
    new_session_id <- newSessionID
    now <- getCurrentTime
    let session = Session new_session_id (fromIntegral expiry_time
                                          `addUTCTime`
                                          now)

    did_it_work <- update ps (MaybeAddSessionByPassword session password)
    return $ if did_it_work
      then Just new_session_id
      else Nothing

newSessionID :: IO SessionID
newSessionID = SessionID `fmap` randBytes 33

isValidSessionID :: SessionID -> Storage -> IO Bool
isValidSessionID sid (readPersistentStorage -> ps) = do
    now <- getCurrentTime
    query ps (IsValidSessionID sid now)

setNoAuthentication' :: Storage -> IO ()
setNoAuthentication' (readPersistentStorage -> ps) =
    update ps (SetEncryptedAdminPassword (NoAuthentication))

setNoAuthentication :: B.ByteString -> Storage -> IO Bool
setNoAuthentication old_password (readPersistentStorage -> ps) =
    update ps (ChangePassword old_password NoAuthentication)

setAdminPassword :: Maybe B.ByteString
                 -> Storage
                 -> IO ()
setAdminPassword password (readPersistentStorage -> ps) = do
    encrypted_password <- case password of
        Nothing -> return AlwaysDenied
        Just  p -> Password `fmap` encryptPass' (Pass p)
    update ps (SetEncryptedAdminPassword encrypted_password)

changePassword :: B.ByteString
               -> B.ByteString
               -> Storage
               -> IO Bool
changePassword old_password new_password (readPersistentStorage -> ps) = do
    encrypted_pass <- encryptPass' (Pass new_password)
    update ps (ChangePassword old_password (Password encrypted_pass))

invalidateSessionID :: SessionID -> Storage -> IO ()
invalidateSessionID sid (readPersistentStorage -> ps) =
    update ps (InvalidateSessionID sid)

hasNoAuthentication :: Storage -> IO Bool
hasNoAuthentication (readPersistentStorage -> ps) = do
    scheme <- query ps GetEncryptedAdminPassword
    return $ case scheme of
        NoAuthentication -> True
        _ -> False

