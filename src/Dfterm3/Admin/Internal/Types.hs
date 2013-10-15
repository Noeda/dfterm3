-- | Internal module to Dfterm3.Admin
--

{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, StandaloneDeriving, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dfterm3.Admin.Internal.Types
    ( AdminStatePersistent(..)
    , AccessRequired(..)
    , initialAdminStatePersistent
    , SessionID(..)
    , sessionIDToByteString
    , byteStringToSessionID
    , Session(..)
    , sessions
    , adminPassword )
    where

import Crypto.Scrypt
import Control.Lens
import Data.SafeCopy
import Data.Typeable ( Typeable )
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Time

newtype SessionID = SessionID B.ByteString
                    deriving ( Eq, Ord, Show, Read, Typeable )

sessionIDToByteString :: SessionID -> B.ByteString
sessionIDToByteString (SessionID bs) = bs

byteStringToSessionID :: B.ByteString -> SessionID
byteStringToSessionID = SessionID

data AccessRequired = AlwaysDenied | NoAuthentication | Password EncryptedPass
                      deriving ( Eq, Typeable )

data Session = Session SessionID UTCTime
               deriving ( Eq, Ord, Read, Show, Typeable )

data AdminStatePersistent = AdminStatePersistent
    { _sessions :: M.Map SessionID Session
    , _adminPassword :: AccessRequired }

initialAdminStatePersistent :: AdminStatePersistent
initialAdminStatePersistent =
    AdminStatePersistent { _sessions = M.empty
#ifdef WINDOWS
                         , _adminPassword = NoAuthentication }
#else
                         , _adminPassword = AlwaysDenied }
#endif

deriving instance Typeable EncryptedPass

makeLenses ''AdminStatePersistent
deriveSafeCopy 0 'base ''AccessRequired
deriveSafeCopy 0 'base ''AdminStatePersistent
deriveSafeCopy 0 'base ''Session
deriveSafeCopy 0 'base ''SessionID
deriveSafeCopy 0 'base ''EncryptedPass


