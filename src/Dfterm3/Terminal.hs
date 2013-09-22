-- | Implements a terminal-like abstraction where cell contents are unicode
-- strings.

{-# LANGUAGE ViewPatterns, DeriveDataTypeable, Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dfterm3.Terminal
    ( newTerminal
    , CellTextContent
    , Terminal()
    , Cell(..)
    , Location
    , TerminalChanges
    , emptyChanges
    , findChanges
    -- * Colors
    , ANSIColor(..)
    , intToColor
    , colorToInt
    -- * Modifying terminal contents
    , writeCells
    -- * Lenses
    , gridArray
    , cursor
    , cellAt
    -- * Views
    , cursor'
    , cellAt'
    , width
    , height
    , dimensions
    -- * Misc
    , textWidth
    , tuckBounds )
    where

import System.Random
import Control.Lens hiding ( indices )
import Control.Monad.ST
import Data.Array.ST
import Data.Typeable
import Data.Array
import Data.Foldable
import qualified Data.Text as T

import System.IO.Unsafe ( unsafePerformIO )
import Data.Char ( ord )
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr

--foreign import ccall unsafe "unicode_width"
--    c_unicode_width :: CUInt -> IO CInt
foreign import ccall unsafe "unicode_string_width"
    c_unicode_string_width :: Ptr CUInt -> CSize -> IO CInt

-- | Returns the width of some text (in columns) as it would appear in a
-- terminal.
--
-- It is not possible to guarantee the text will actually be this wide but it
-- should be right most of the time, assuming correctly configured terminals.
textWidth :: T.Text -> Int
textWidth text = unsafePerformIO $
    allocaArray len $ \ptr -> do
        pokeArray (ptr :: Ptr CUInt)
                  (fmap (fromIntegral . ord) $ T.unpack text)
        fromIntegral `fmap` c_unicode_string_width ptr (fromIntegral len)
  where
    len = T.length text

-- | The type of some location in a terminal.
type Location = (Int, Int)

-- | Type of the text content in one cell.
type CellTextContent = T.Text
-- | A type synonym for a tuple that represents changes in the terminal.  game.
-- The first two integers are new width and height, the location is the new
-- cursor location and the list is a list of coordinates where cells have
-- changed and their contents.
type TerminalChanges = (Int, Int, Location, [(Location, Cell)])

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

-- | Represents what is inside one cell in a terminal.
--
-- The first color is the foregrond color and the latter is the background
-- color.
data Cell = Cell
    {-# UNPACK #-} !CellTextContent
    !ANSIColor
    !ANSIColor
    deriving ( Eq, Show, Ord, Read, Typeable )

-- | This data type is a game whose output can be wholly represented by a bunch
-- of Unicode characters, with foregrounds and backgrounds.
data Terminal = Terminal { _gridArray :: Array (Int, Int) Cell
                         , _cursor :: Location }
                 deriving ( Eq, Show, Ord, Read, Typeable )

-- | Getter to the cursor in a terminal.
cursor :: Getter Terminal (Int, Int)
cursor = to _cursor

-- | Returns where the cursor is right now.
cursor' :: Terminal -> Location
cursor' = _cursor

-- | Replaces the text content of a cell.
--
-- This may modify cells adjacent to the cell because some Unicode code points
-- are wider than others. If you insert a double-width code point, then the
-- next cell will have empty context. If you insert the code point right at the
-- end of a column, then the character will be written to the next line at
-- start, if there is a next line.
--
-- This creates a new array behind the scenes so it is more efficient to write
-- many location and cells at once rather than writing one at a time.
--
-- If the location is out of range, nothing is written.
writeCells :: [(Location, Cell)] -> Terminal -> Terminal
writeCells locations_and_cells terminal =
    terminal { _gridArray = runSTArray $ do
        arr <- thaw (_gridArray terminal)
        writeCells' locations_and_cells arr }

writeCells' :: [(Location, Cell)]
            -> STArray s Location Cell
            -> ST s (STArray s Location Cell)
writeCells' locations_and_cells arr = do
    bounds <- getBounds arr
    forM_ locations_and_cells $ writeCell arr bounds
    return arr
  where
    writeCell arr
              bounds@((left, top), (right, bottom))
              (coords@(x, y), cell@(Cell text foreground background))
        | x < left || y < top || x > right || y > bottom = return ()
        | char_width <= 1 = writeArray arr coords cell
        | otherwise =
            if x+char_width > right+1
              then writeCell arr bounds ((left, y+1), cell)
              else do writeArray arr coords cell
                      forM_ [x+1..x+char_width-1] $ \new_x ->
                          writeArray arr
                                     (new_x, y)
                                     emptyCell
      where
        char_width = textWidth text
        emptyCell = Cell T.empty foreground background

-- | Getter to one cell at somewhere.
--
-- Use `cellAt'` for the plain function version.
cellAt :: Location -> Getter Terminal Cell
cellAt coords = to (cellAt' coords)

-- | Returns a cell at somewhere.
cellAt' :: Location -> Terminal -> Cell
cellAt' coords term = _gridArray term ! coords

-- | Lens to the array in a terminal.
gridArray :: Lens' Terminal (Array Location Cell)
gridArray = lens _gridArray (\game new_arr -> game { _gridArray = new_arr })

-- | Construct a `Terminal`.
newTerminal :: Array Location Cell   -- ^ Array of the cells.
            -> Location              -- ^ Initial position of the cursor.
            -> Terminal
newTerminal = Terminal

-- | Returns the width and height of a `Terminal` as a tuple.
dimensions :: Terminal -> (Int, Int)
dimensions term =
    let ((left, top), (right, bottom)) = bounds $ _gridArray term
     in (max (right-left+1) 0, max (bottom-top+1) 0)

-- | Returns the width of a `Terminal`.
width :: Terminal -> Int
width = fst . dimensions

-- | Returns the height of a `Terminal`.
height :: Terminal -> Int
height = snd . dimensions

instance Random ANSIColor where
    random gen =
        let ( val, next_gen ) = next gen
         in ( intToColor $ val `mod` 17, next_gen )
    randomR _ _ = error "randomR: \
                        \Interval randoms are not implemented for `ANSIColor`."

-- | Returns empty changes from an old state.
--
-- This is the same as calling `findChanges` with the same state in both
-- arguments.
emptyChanges :: Terminal -> TerminalChanges
emptyChanges game = (width game, height game, cursor' game, [])

-- | Calculates what changes there are between two terminals.
findChanges :: Terminal       -- ^ The old game.
            -> Terminal       -- ^ The new game.
            -> TerminalChanges
findChanges (_gridArray -> old_arr)
            new@(_gridArray -> new_arr) =
    -- If bounds have changed, update everything.
    if bounds old_arr /= bounds new_arr
      then (width new, height new, cursor' new, tuckBounds' changeEverything)
      else (width new, height new, cursor' new, tuckBounds' findChanges)
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

-- | Normalizes bounds in terminal changesets so that origin is always at
-- (0, 0)
tuckBounds :: Int      -- ^ Leftmost x-coordinate.
           -> Int      -- ^ Topmost y-coordinate.
           -> [(Location, Cell)] -> [(Location, Cell)]
tuckBounds left top = fmap (\((x, y), cell) -> ((x-left,y-top), cell))

