-- | Safe versions of some functions.

{-# LANGUAGE ScopedTypeVariables #-}

module Dfterm3.Safe
    ( safeFromIntegral )
    where

-- | Almost the same as `fromIntegral` but raises a user error if the source
-- integer cannot be represented in the target type.
--
-- This is safe in the sense that coercions that would silently fail and cause
-- havoc crash things down instead.
--
-- It is only almost the same because it can only convert integral types.
safeFromIntegral :: forall a b. (Num a, Integral a, Num b, Integral b) =>
                    a -> b
safeFromIntegral from
    | from /= (fromIntegral result :: a) = error "Invalid coercion."
    | otherwise = result
  where
    result = fromIntegral from :: b
{-# INLINE safeFromIntegral #-}

