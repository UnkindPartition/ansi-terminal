-- | The \'ANSI\' standards refer to the visual style of displaying characters
-- as their \'graphic rendition\'. The style includes the color of a character
-- or its background, the intensity (bold, normal or faint) of a character, or
-- whether the character is italic or underlined (single or double), blinking
-- (slowly or rapidly) or visible or not. The \'ANSI\' codes to establish the
-- graphic rendition for subsequent text are referred to as SELECT GRAPHIC
-- RENDITION (SGR).
--
-- This module exports types used to represent SGR aspects. See
-- also 'System.Console.ANSI.setSGR' and related functions.
module System.Console.ANSI.Types
  (
  -- * Types used to represent SGR aspects
    SGR (..)
  , ConsoleLayer (..)
  , Color (..)
  , ColorIntensity (..)
  , ConsoleIntensity (..)
  , Underlining (..)
  , BlinkSpeed (..)
  ) where

import Data.Ix (Ix)

import Data.Colour (Colour)

-- | ANSI's eight standard colors. They come in two intensities, which are
-- controlled by 'ColorIntensity'. Many terminals allow the colors of the
-- standard palette to be customised, so that, for example,
-- @setSGR [ SetColor Foreground Vivid Green ]@ may not result in bright green
-- characters.
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI's standard colors come in two intensities
data ColorIntensity = Dull
                    | Vivid
                    deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI colors can be set on two different layers
data ConsoleLayer = Foreground
                  | Background
                  deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI blink speeds: values other than 'NoBlink' are not widely supported
data BlinkSpeed = SlowBlink -- ^ Less than 150 blinks per minute
                | RapidBlink -- ^ More than 150 blinks per minute
                | NoBlink
                deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI text underlining
data Underlining
  = SingleUnderline
  -- | Not widely supported. Not supported natively on Windows 10
  | DoubleUnderline
  | NoUnderline
  deriving (Eq, Ord, Bounded ,Enum, Show, Read, Ix)

-- | ANSI general console intensity: usually treated as setting the font style
-- (e.g. 'BoldIntensity' causes text to be bold)
data ConsoleIntensity
  = BoldIntensity
  -- | Not widely supported: sometimes treated as concealing text. Not supported
  -- natively on Windows 10
  | FaintIntensity
  | NormalIntensity
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI Select Graphic Rendition (SGR) command
--
-- In respect of colors, there are two alternative commands:
--
-- (1) the \'ANSI\' standards allow for eight standard colors (with two
-- intensities). Windows and many other terminals (including xterm) allow the
-- user to redefine the standard colors (so, for example 'Vivid' 'Green' may not
-- correspond to bright green; and
--
-- (2) an extension of the standard that allows true colors (24 bit color depth)
-- in RGB space.
data SGR
  -- | Default rendition, cancels the effect of any preceding occurrence of SGR
  -- (implementation-defined)
  = Reset
  -- | Set the character intensity. Partially supported natively on Windows 10
  | SetConsoleIntensity !ConsoleIntensity
  -- | Set italicized. Not widely supported: sometimes treated as swapping
  -- foreground and background. Not supported natively on Windows 10
  | SetItalicized !Bool
  -- | Set or clear underlining. Partially supported natively on Windows 10
  | SetUnderlining !Underlining
  -- | Set or clear character blinking. Not supported natively on Windows 10
  | SetBlinkSpeed !BlinkSpeed
  -- | Set revealed or concealed. Not widely supported. Not supported natively
  -- on Windows 10
  | SetVisible !Bool
  -- | Set negative or positive image. Supported natively on Windows 10
  | SetSwapForegroundBackground !Bool
  -- | Set a color from the standard palette of 16 colors (8 colors by 2
  -- color intensities). Many terminals allow the palette colors to be
  -- customised
  | SetColor !ConsoleLayer !ColorIntensity !Color
  -- | Set a true color (24 bit color depth). Supported natively on Windows 10
  -- from the Creators Update (April 2017)
  --
  -- @since 0.7
  | SetRGBColor !ConsoleLayer !(Colour Float)
  deriving (Eq, Show, Read)
