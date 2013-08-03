-- | This is a dummy CP437 game that can be used to test the game pool system.
--
-- The game won't ever end.
--

{-# LANGUAGE OverloadedStrings #-}

module Dfterm3.Noise
    ( registerNoiseCP437Game )
    where

import Dfterm3.GamePool
import Dfterm3.CP437Game

import System.Random
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Array

registerNoiseCP437Game :: GamePool
                       -> IO (GameInstance CP437Game () CP437Changes)
registerNoiseCP437Game pool = do
    ( provider, inst ) <- registerGame pool

    void $ forkIO $ do
        rng_cp437 <- mkStdGen <$> randomIO
        rng_fcolors <- mkStdGen <$> randomIO
        rng_bcolors <- mkStdGen <$> randomIO
        let random_cp437 = randoms rng_cp437
            random_fcolor = randoms rng_fcolors
            random_bcolor = randoms rng_bcolors

        rec provider random_cp437 random_fcolor random_bcolor Nothing

    return inst
  where
    rec provider random_cp437 random_fcolor random_bcolor old_state = do
        threadDelay 100000
        let new_cells = zipWith3 CP437Cell
                                 (take (80*24) random_cp437)
                                 (take (80*24) random_fcolor)
                                 (take (80*24) random_bcolor)

        let new_state = (newCP437Game "Noise" $
                              listArray ((0, 0), (79, 23))
                                        new_cells)
        case old_state of
            Nothing -> updateGame new_state (emptyCP437Changes new_state) provider
            Just  x -> updateGame new_state (findCP437Changes x new_state) provider

        rec provider
            (drop (80*24) random_cp437)
            (drop (80*24) random_fcolor)
            (drop (80*24) random_bcolor)
            (Just new_state)


