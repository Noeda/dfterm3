{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dfterm3.Dfterm3State.Internal.Transactions
    ( TryPublishGame(..)
    , TryRemoveGame(..)
    , GetPublishedGames(..) )
    where

import Dfterm3.GameSubscription.Internal.Transactions
import Dfterm3.Dfterm3State.Internal.Types
import Data.Acid

makeAcidic ''PersistentStorageState [ 'tryPublishGame
                                    , 'tryRemoveGame
                                    , 'getPublishedGames ]

