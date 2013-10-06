module Dfterm3.Admin.Internal.Transactions
    ( getEncryptedAdminPassword
    , setEncryptedAdminPassword
    , maybeAddSessionByPassword
    , isValidSessionID
    , changePassword
    , invalidateSessionID )
    where

import Dfterm3.Dfterm3State.Internal.Types
import Dfterm3.Admin.Internal.Types
import Dfterm3.Util ( whenJust )

import Data.Acid
import Data.Time ( UTCTime )
import Control.Lens
import Crypto.Scrypt

import qualified Data.ByteString as B
import qualified Data.Map as M

getEncryptedAdminPassword :: Query PersistentStorageState AccessRequired
getEncryptedAdminPassword = view (admin . adminPassword)

setEncryptedAdminPassword :: AccessRequired
                          -> Update PersistentStorageState ()
setEncryptedAdminPassword pass = admin . adminPassword .= pass

changePassword :: B.ByteString
               -> AccessRequired
               -> Update PersistentStorageState Bool
changePassword old_password encrypted_new_pass = do
    maybe_old_encrypted_pass <- liftQuery getEncryptedAdminPassword
    case maybe_old_encrypted_pass of
        AlwaysDenied -> set_new
        NoAuthentication -> set_new
        Password old_encrypted_pass ->
            let ( succeeded, _ ) = verifyPass defaultParams
                                              (Pass old_password)
                                              old_encrypted_pass
             in if succeeded
                  then set_new
                  else return False
  where
    set_new = setEncryptedAdminPassword encrypted_new_pass >>
              return True

maybeAddSessionByPassword :: Session
                          -> B.ByteString
                          -> Update PersistentStorageState Bool
maybeAddSessionByPassword session@(Session sid _) password = do
    encrypted_pass <- use (admin . adminPassword)
    case encrypted_pass of
        AlwaysDenied -> return False
        NoAuthentication -> return True
        Password epass -> checkPass epass
  where
    checkPass epass = do
        let ( succeeded, maybe_new_pass ) =
                verifyPass defaultParams (Pass password) epass
        if succeeded
          then do admin . sessions %= M.insert sid session
                  whenJust maybe_new_pass $ \new_pass ->
                      admin . adminPassword .= Password new_pass
                  return True
          else return False

isValidSessionID :: SessionID -> UTCTime -> Query PersistentStorageState Bool
isValidSessionID sid now = do
    s <- view (admin . sessions)
    return $ case M.lookup sid s of
        Nothing -> False
        Just (Session _ expiry_time) -> now <= expiry_time

invalidateSessionID :: SessionID -> Update PersistentStorageState ()
invalidateSessionID sid = admin . sessions %= M.delete sid

