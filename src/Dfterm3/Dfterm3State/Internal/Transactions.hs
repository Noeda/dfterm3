{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dfterm3.Dfterm3State.Internal.Transactions
    ( TryPublishGame(..)
    , TryRemoveGame(..)
    , GetPublishedGames(..)
    , AreGuestsEnabled(..)
    , AreRegistrationsEnabled(..)
    , IsValidLogin(..) )
    where

import Dfterm3.GameSubscription.Internal.Transactions
import Dfterm3.Dfterm3State.Internal.Types
import Dfterm3.UserAccounting.Internal.Transactions
import Data.Acid

makeAcidic ''PersistentStorageState [ 'tryPublishGame
                                    , 'tryRemoveGame
                                    , 'getPublishedGames
                                    , 'areGuestsEnabled
                                    , 'areRegistrationsEnabled
                                    , 'isValidLogin ]

