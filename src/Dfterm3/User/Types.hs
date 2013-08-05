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
    , first
    , adminPassword )
    where

import Data.Acid
import Data.Typeable ( Typeable )
import Control.Lens
import Data.SafeCopy
import Crypto.Scrypt

data UserSystemState = UserSystemState { _first :: Bool
                                       , _adminPassword ::
                                           Maybe EncryptedPass }

data Admin = Admin

deriving instance Typeable EncryptedPass

makeLenses ''UserSystemState
makeLenses ''Admin
deriveSafeCopy 0 'base ''EncryptedPass
deriveSafeCopy 0 'base ''UserSystemState

newtype UserSystem = UserSystem (AcidState UserSystemState)

