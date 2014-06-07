module Dfterm3.Prelude
    ( module Data.Semigroup
    , module Prelude
    , module Control.Category
    , module Data.Foldable
    , module Data.Traversable
    , module Control.Monad
    , module Control.Applicative
    , module Data.Maybe
    , module Data.IORef
    , module Data.Int
    , module Data.Word
    , module Data.Typeable
    , safeFromIntegral
    , showT
    , atomicModifyIORef_' )
    where

import Prelude hiding ( sequence, mapM, or, concat, notElem, elem
                      , foldl, foldr1, concatMap, any, sum, foldr
                      , and, all, mapM_, sequence_, product, maximum
                      , foldl1, minimum, (.), id )

import Data.Foldable
import Data.Traversable
import Data.Int
import Data.Word
import Data.Typeable
import Control.Monad hiding ( msum, forM, forM_, sequence, mapM, mapM_
                            , sequence_ )
import Control.Applicative
import Control.Category
import Data.Semigroup
import Data.Maybe
import Data.IORef
import qualified Data.Text as T

-- | Same as `show` but the result will be a `T.Text`, not `String`.
showT :: Show a => a -> T.Text
showT = T.pack . show
{-# INLINE showT #-}

-- | Same as `atomicModifyIORef'` but does not return a value.
atomicModifyIORef_' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_' ref fun = atomicModifyIORef' ref $ \old -> ( fun old, () )
{-# INLINE atomicModifyIORef_' #-}

-- | Similar to `fromIntegral` but raises a user error if the source
-- integer cannot be represented in the target type.
--
-- This cannot turn an integral into a non-integral type.
safeFromIntegral :: forall a b. (Num a, Integral a, Num b, Integral b) =>
                    a -> b
safeFromIntegral from
    | from /= (fromIntegral result :: a) = error "Invalid coercion."
    | otherwise = result
  where
    result = fromIntegral from :: b
{-# INLINE safeFromIntegral #-}

