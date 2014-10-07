{-# LANGUAGE DeriveGeneric #-}

module Dfterm3.Game.TextGame
    ( TextGameInput(..)
    , TextGameChangesets(..)
    , Input(..)
    , KeyDirection(..) )
    where

import Dfterm3.Prelude
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Aeson as J
import Dfterm3.Terminal

data Input = Input !Int !Word32 !Bool !Bool !Bool
             deriving ( Eq, Ord, Read, Show, Typeable, Generic )

data KeyDirection = Up | Down | UpAndDown
                    deriving ( Eq, Ord, Read, Show, Typeable, Generic )

instance J.FromJSON KeyDirection
instance J.FromJSON Input
instance J.ToJSON KeyDirection
instance J.ToJSON Input

data TextGameInput = MkTextGameInput !KeyDirection !T.Text !Input
data TextGameChangesets =
    -- keep these non-strict
    MkTextGameChangesets Terminal TerminalChanges (Maybe T.Text)

