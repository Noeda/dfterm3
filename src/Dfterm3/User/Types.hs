-- | Complementary to the `Dfterm3.User` module, this module just defines the
-- data types for the user system. Internal use only! If you are an evil module
-- that is not `Dfterm3.User` and are importing this, I will glare at you. And
-- maybe bite.
--
-- Anyway, this module exists because of heavy usage of template Haskell to
-- generate SafeCopy instances and lenses.

{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveDataTypeable #-}

module Dfterm3.User.Types
    ( UserSystem(..)
    , UserSystemState(..)
    , Admin(..)
    , AddingResult(..)
    , validUntil
    , sessionID
    , first
    , dwarfFortresses
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
import qualified Data.ByteString as B
import qualified Data.Map as M

data UserSystemState = UserSystemState { _first :: Bool
                                       , _dwarfFortresses ::
                                           M.Map String DwarfFortress
                                       , _adminSessions ::
                                           M.Map B.ByteString Admin
                                       , _adminPassword ::
                                           Maybe EncryptedPass }

data Admin = Admin { _validUntil :: !UTCTime
                   , _sessionID :: !B.ByteString }
                   deriving ( Eq, Ord )

data AddingResult = New | Replaced

deriving instance Typeable EncryptedPass
deriving instance Typeable Admin
deriving instance Typeable AddingResult

makeLenses ''UserSystemState
makeLenses ''Admin
deriveSafeCopy 0 'base ''EncryptedPass
deriveSafeCopy 0 'base ''UserSystemState
deriveSafeCopy 0 'base ''Admin
deriveSafeCopy 0 'base ''DwarfFortress
deriveSafeCopy 0 'base ''AddingResult

newtype UserSystem = UserSystem (AcidState UserSystemState)

