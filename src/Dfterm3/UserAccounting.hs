{-# LANGUAGE ViewPatterns #-}

module Dfterm3.UserAccounting
    ( areGuestsEnabled
    , areRegistrationsEnabled
    , isValidLogin )
    where

import Data.Acid
import Dfterm3.Dfterm3State.Internal.Types
import qualified Dfterm3.Dfterm3State.Internal.Transactions as Trans
import qualified Data.Text as T

areGuestsEnabled :: Storage -> IO Bool
areGuestsEnabled (readPersistentStorage -> st) =
    query st Trans.AreGuestsEnabled

areRegistrationsEnabled :: Storage -> IO Bool
areRegistrationsEnabled (readPersistentStorage -> st) =
    query st Trans.AreRegistrationsEnabled

isValidLogin :: T.Text -> T.Text -> Storage -> IO Bool
isValidLogin username password (readPersistentStorage -> st) = do
    update st $ Trans.IsValidLogin username password

