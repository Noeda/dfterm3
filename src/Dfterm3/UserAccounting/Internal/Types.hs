{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Dfterm3.UserAccounting.Internal.Types
    ( UserAccount(..)
    , UserAccountingPersistent(..)
    , password
    , admin
    , userAccounts
    , allowRegistrations
    , allowGuests
    , initialUserAccounting )
    where

import Data.Typeable
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Lens
import Data.SafeCopy

data UserAccount = UserAccount
    { _password :: Maybe B.ByteString
    , _admin    :: Bool }
    deriving ( Typeable )

data UserAccountingPersistent = UserAccountingPersistent
    { _userAccounts       :: M.Map T.Text UserAccount
    , _allowRegistrations :: Bool
    , _allowGuests        :: Bool }

makeLenses ''UserAccount
makeLenses ''UserAccountingPersistent
deriveSafeCopy 1 'base ''UserAccount
deriveSafeCopy 1 'base ''UserAccountingPersistent

initialUserAccounting :: UserAccountingPersistent
initialUserAccounting =
    UserAccountingPersistent
    { _userAccounts = M.empty
    , _allowRegistrations = True
    , _allowGuests = True }

