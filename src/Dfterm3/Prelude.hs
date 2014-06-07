module Dfterm3.Prelude
    ( module Exports
    , safeFromIntegral
    , showT
    , atomicModifyIORef_' )
    where

import Prelude as Exports hiding
                      ( sequence, mapM, or, concat, notElem, elem
                      , foldl, foldr1, concatMap, any, sum, foldr
                      , and, all, mapM_, sequence_, product, maximum
                      , foldl1, minimum, (.), id )

import Data.Foldable as Exports
import Data.Traversable as Exports
import Data.Int as Exports
import Data.Word as Exports
import Data.Typeable as Exports
import Control.Monad as Exports hiding
                            ( msum, forM, forM_, sequence, mapM, mapM_
                            , sequence_ )
import Control.Applicative as Exports
import Control.Category as Exports
import Data.Semigroup as Exports
import Data.Maybe as Exports
import Data.IORef as Exports
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

