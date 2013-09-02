-- | Complementary to the `Dfterm3.User` module, this module just defines the
-- data types for the user system. Internal use only! If you are an evil module
-- that is not `Dfterm3.User` and are importing this, I will glare at you. And
-- maybe bite.
--
-- Anyway, this module exists because of heavy usage of template Haskell to
-- generate SafeCopy instances and lenses.

{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dfterm3.User.Types
    ( UserSystem(..)
    , UserSystemState(..)
    , Admin(..)
    , AddingResult(..)
    , validUntil
    , sessionID
    , first
    , dwarfFortressesWithPerms
    , registeredUsers
    , rawDf
    , dfEnabled
    , allowWatchingByDefault
    , allowPlayingByDefault
    , allowChattingByDefault
    , allowedWatchers
    , allowedPlayers
    , allowedChatters
    , disallowedWatchers
    , disallowedPlayers
    , disallowedChatters
    , DwarfFortressWithPerms(..)
    , adminSessions
    , adminPassword )
    where

import Dfterm3.DwarfFortress

import Data.Acid
import Data.Typeable ( Typeable )
import Control.Lens
import Data.SafeCopy
import Data.Time.Clock
import Crypto.Scrypt
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S

type RegisteredUserID = Integer

data DwarfFortressWithPerms =
    DwarfFortressWithPerms { _rawDf :: !DwarfFortress
                           , _dfEnabled :: Bool
                           , _allowWatchingByDefault :: Bool
                           , _allowPlayingByDefault :: Bool
                           , _allowChattingByDefault :: Bool
                           , _allowedWatchers :: S.Set RegisteredUserID
                           , _allowedPlayers  :: S.Set RegisteredUserID
                           , _allowedChatters :: S.Set RegisteredUserID
                           , _disallowedWatchers :: S.Set RegisteredUserID
                           , _disallowedPlayers :: S.Set RegisteredUserID
                           , _disallowedChatters :: S.Set RegisteredUserID }
    deriving ( Eq, Typeable )

data UserSystemState = UserSystemState { _first :: Bool
                                       , _dwarfFortressesWithPerms ::
                                           M.Map String DwarfFortressWithPerms
                                       , _adminSessions ::
                                           M.Map B.ByteString Admin
                                       , _adminPassword ::
                                           Maybe EncryptedPass
                                       , _registeredUsers ::
                                           M.Map RegisteredUserID
                                                 RegisteredUser }

data RegisteredUser =
    RegisteredUser
    {
      _userName :: T.Text
    -- These are authentication methods. "Just x" if that authentication method
    -- is enabled.
    , _password :: Maybe EncryptedPass
    , _cookieID :: Maybe B.ByteString   -- ^ Cookie based authentication
                                        --   The bytestring is an identifying
                                        --   key.

    -- Non-essential but /interesting/ information.
    , _registrationTime :: UTCTime      -- ^ When did this user register.
    , _numKeyPresses :: Integer         -- ^ Approximate number of keypresses
                                        --   this user has pressed.
    }
    deriving ( Eq, Typeable )

data Admin = Admin { _validUntil :: !UTCTime
                   , _sessionID :: !B.ByteString }
                   deriving ( Eq, Ord )

data AddingResult = New | Modified

deriving instance Typeable EncryptedPass
deriving instance Typeable Admin
deriving instance Typeable AddingResult

makeLenses ''UserSystemState
makeLenses ''DwarfFortressWithPerms
makeLenses ''Admin
deriveSafeCopy 0 'base ''EncryptedPass
deriveSafeCopy 0 'base ''UserSystemState
deriveSafeCopy 0 'base ''Admin
deriveSafeCopy 0 'base ''DwarfFortress
deriveSafeCopy 0 'base ''AddingResult
deriveSafeCopy 0 'base ''RegisteredUser
deriveSafeCopy 0 'base ''DwarfFortressWithPerms

newtype UserSystem = UserSystem (AcidState UserSystemState)

