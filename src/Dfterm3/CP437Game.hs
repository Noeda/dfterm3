{-# LANGUAGE ViewPatterns, DeriveDataTypeable, Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Dfterm3.CP437Game
    ( newCP437Game
    , CP437Code
    , CP437Game()
    , CP437Cell(..)
    , CP437Changes
    , emptyCP437Changes
    , findCP437Changes
    -- * Colors
    , ANSIColor(..)
    , intToColor
    , colorToInt
    -- * Lenses
    , cp437Name
    , cp437Array
    , cp437CellAt
    -- * Views
    , cp437Width
    , cp437Height
    , cp437Dimensions
    -- * Misc
    , tuckBounds )
    where

import Dfterm3.GamePool

import System.Random
import Control.Lens hiding ( indices )
import Data.Typeable
import Data.Array
import Data.Word ( Word8 )
import Data.Foldable
import qualified Data.Text as T

-- | CP437 characters conveniently fit in 8 bits.
type CP437Code = Word8
-- | A type synonym for a tuple that represents changes in CP437 represented
-- game. The first two integers are new width and height and the list is a list
-- of coordinates where cells have changed and their contents.
type CP437Changes = (Int, Int, [((Int, Int), CP437Cell)])

-- | The 16 colors that appear in ANSI terminals.
--
-- There are actually 17 colors here, of which `Default` is really some other
-- color.
--
-- `ANSIColor` is an instance of `Random` but you should not take intervals of
-- it as it makes no sense.
data ANSIColor = Black | Red | Green | Yellow | Blue | Magenta | Cyan |
                 White | Default | BrightBlack | BrightRed | BrightGreen |
                 BrightYellow | BrightBlue | BrightMagenta | BrightCyan |
                 BrightWhite
                 deriving ( Eq, Show, Ord, Read, Typeable )

-- | Turns an integer between 0 and 16 (inclusive) into a color.
--
-- What color exactly? Well, any color. But the same integer will always map to
-- the same color.
intToColor :: Int -> ANSIColor
intToColor 0 = Black
intToColor 1 = Blue
intToColor 2 = Green
intToColor 3 = Cyan
intToColor 4 = Red
intToColor 5 = Magenta
intToColor 6 = Yellow
intToColor 7 = White
intToColor 8 = BrightBlack
intToColor 9 = BrightBlue
intToColor 10 = BrightGreen
intToColor 11 = BrightCyan
intToColor 12 = BrightRed
intToColor 13 = BrightMagenta
intToColor 14 = BrightYellow
intToColor 15 = BrightWhite
intToColor 16 = Default
intToColor _ = error "intToColor: Invalid color integer."
{-# INLINE intToColor #-}

-- | The reverse operation of `intToColor`.
colorToInt :: ANSIColor -> Int
colorToInt Black = 0
colorToInt Red = 1
colorToInt Green = 2
colorToInt Yellow = 3
colorToInt Blue = 4
colorToInt Magenta = 5
colorToInt Cyan = 6
colorToInt White = 7
colorToInt BrightBlack = 8
colorToInt BrightRed = 9
colorToInt BrightGreen = 10
colorToInt BrightYellow = 11
colorToInt BrightBlue = 12
colorToInt BrightMagenta = 13
colorToInt BrightCyan = 14
colorToInt BrightWhite = 15
colorToInt Default = 16
{-# INLINE colorToInt #-}

-- | Represents what is inside one cell in a CP437 game.
data CP437Cell = CP437Cell {-# UNPACK #-} !CP437Code  -- ^ The characte code.
                           !ANSIColor  -- ^ Foreground.
                           !ANSIColor  -- ^ Background.
                 deriving ( Eq, Show, Ord, Read, Typeable )

-- | This data type is a game whose output can be wholly represented by a bunch
-- of CP437 encoded characters, with foregrounds and backgrounds.
data CP437Game = CP437Game { _cp437Name :: !T.Text
                           , _cp437Array :: !(Array (Int, Int) CP437Cell) }
                 deriving ( Eq, Show, Ord, Read, Typeable )

-- | Lens to one cell at somewhere.
--
-- Note that setting this recreates the entire array. Very inefficient.
cp437CellAt :: (Int, Int) -> Lens' CP437Game CP437Cell
cp437CellAt coords = lens (\game -> _cp437Array game ! coords)
                          (\game new_cell ->
                              game { _cp437Array =
                                  _cp437Array game // [(coords, new_cell)] })

-- | Lens to the name of a CP437 game.
cp437Name :: Lens' CP437Game T.Text
cp437Name = lens _cp437Name (\game new_name -> game { _cp437Name = new_name })
-- | Lens to the array in a CP437 game.
cp437Array :: Lens' CP437Game (Array (Int, Int) CP437Cell)
cp437Array = lens _cp437Array (\game new_arr -> game { _cp437Array = new_arr })

-- | Construct a `CP437Game`.
newCP437Game :: T.Text            -- ^ Name that identifies the game.
                                  --   Should refer to the game in general.
             -> Array (Int, Int) CP437Cell
                                  -- ^ Array of the cells.
             -> CP437Game
newCP437Game = CP437Game

-- | Returns the width and height of a `CP437Game` as a tuple.
cp437Dimensions :: CP437Game -> (Int, Int)
cp437Dimensions game =
    let ((left, top), (right, bottom)) = bounds $ _cp437Array game
     in (max (right-left+1) 0, max (bottom-top+1) 0)

-- | Returns the width of a `CP437Game`.
cp437Width :: CP437Game -> Int
cp437Width = fst . cp437Dimensions

-- | Returns the height of a `CP437Game`.
cp437Height :: CP437Game -> Int
cp437Height = snd . cp437Dimensions

instance Random ANSIColor where
    random gen =
        let ( val, next_gen ) = next gen
         in ( intToColor $ val `mod` 17, next_gen )
    randomR _ _ = error "randomR: \
                        \Interval randoms are not implemented for `ANSIColor`."

-- | Returns empty changes from an old state.
--
-- This is the same as calling `findCP437Changes` with the same state in both
-- arguments.
emptyCP437Changes :: CP437Game -> CP437Changes
emptyCP437Changes game = (cp437Width game, cp437Height game, [])

-- | Calculates what changes there are between two CP437 games.
findCP437Changes :: CP437Game       -- ^ The old game.
                 -> CP437Game       -- ^ The new game.
                 -> CP437Changes
findCP437Changes     (_cp437Array -> old_arr)
                 new@(_cp437Array -> new_arr) =
    -- If bounds have changed, update everything.
    if bounds old_arr /= bounds new_arr
      then (cp437Width new, cp437Height new, tuckBounds' changeEverything)
      else (cp437Width new, cp437Height new, tuckBounds' findChanges)
  where
    changeEverything = assocs new_arr
    ((left, top), (_, _)) = bounds new_arr

    findChanges =
        foldl' (\changes coords ->
                   if new_arr ! coords /=
                      old_arr ! coords
                      then (coords, new_arr ! coords):changes
                      else changes)
               []
               (indices old_arr)

    tuckBounds' = tuckBounds left top

-- Normalizes bounds so that origin is always at (0, 0)
tuckBounds :: Int      -- ^ Leftmost x-coordinate.
           -> Int      -- ^ Topmost y-coordinate.
           -> [((Int, Int), CP437Cell)] -> [((Int, Int), CP437Cell)]
tuckBounds left top = fmap (\((x, y), cell) -> ((x-left,y-top), cell))

instance Game CP437Game () CP437Changes

