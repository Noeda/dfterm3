{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

module Dfterm3.Dfterm3State.Internal.Transactions
    ( TryPublishGame(..)
    , TryRemoveGame(..)
    , GetEncryptedAdminPassword(..)
    , SetEncryptedAdminPassword(..)
    , MaybeAddSessionByPassword(..)
    , IsValidSessionID(..)
    , ChangePassword(..) )
    where

import Dfterm3.GameSubscription.Internal.Transactions
import Dfterm3.Admin.Internal.Transactions
import Dfterm3.Dfterm3State.Internal.Types
import Data.Acid

makeAcidic ''PersistentStorageState [ 'tryPublishGame, 'tryRemoveGame
                                    , 'getEncryptedAdminPassword
                                    , 'setEncryptedAdminPassword
                                    , 'maybeAddSessionByPassword
                                    , 'isValidSessionID
                                    , 'changePassword ]

