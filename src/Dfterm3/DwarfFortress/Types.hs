-- | This module exports the `DwarfFortress` type. It has its own module
-- because otherwise there would be cyclic references between some modules and
-- the `Dfterm3.DwarfFortress` module.

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module Dfterm3.DwarfFortress.Types
    ( DwarfFortress(..)
    , DwarfFortressCP437(..)
    , dfExecutable
    , dfArgs
    , dfWorkingDirectory
    , dfName
    , df
    , game )
    where

import Dfterm3.GamePool ( Game )
import Dfterm3.CP437Game
import Data.Typeable ( Typeable )
import Control.Lens
import qualified Data.Text as T

data DwarfFortress = DwarfFortress { _dfExecutable :: FilePath
                                   , _dfArgs :: [String]
                                   , _dfWorkingDirectory :: FilePath
                                   , _dfName :: T.Text }
                     deriving ( Eq, Ord, Show, Read, Typeable )
makeLenses ''DwarfFortress

-- | A wrapping around `CP437Game` so that DwarfFortress has unique type in a
-- `GamePool`.
data DwarfFortressCP437 = DwarfFortressCP437 { _game :: CP437Game
                                             , _df :: DwarfFortress }
                          deriving ( Typeable )
makeLenses ''DwarfFortressCP437

instance Game DwarfFortressCP437 () CP437Changes

