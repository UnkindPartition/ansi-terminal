-- | Types used to represent SELECT GRAPHIC RENDITION (SGR) aspects.
module System.Console.ANSI.Types
  (
    SGR (..)
  , ConsoleLayer (..)
  -- | ANSI colors come in different flavors (3/4 bits, 8bits, 24 bits) :
  --   https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
  , Color (..)
  , Color8Code(..)
  , ColorIntensity (..)
  , ConsoleIntensity (..)
  , Underlining (..)
  , Xterm256Color (..)
  , BlinkSpeed (..)
  ) where

import Data.Ix (Ix)

import Data.Colour (Colour)
import Data.Colour.SRGB(RGB (..))

import Data.Word (Word8)

-- | ANSI 4-bit colors: come in various intensities, which are controlled by
-- 'ColorIntensity'
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI 4-bit colors come in two intensities
data ColorIntensity = Dull
                    | Vivid
                    deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI allows for a palette of up to 256 8-bit colors
newtype Color8Code = Color8Code Word8 deriving (Eq, Show, Read)

-- | Represents the xterm 256 color palette protocol.
--
--  The ranges of colors that can be represented by each constructor are specified in
--  https://en.wikipedia.org/wiki/ANSI_escape_code#Colors.
data Xterm256Color = SystemColor !ColorIntensity !Color
                   -- ^ corresponding ANSI ranges:
                   --
                   -- - [0x00-0x07]:  standard (system) colors (as in ESC [ 30–37 m)
                   -- - [0x08-0x0F]:  high intensity (system) colors (as in ESC [ 90–97 m)
                   | RGBColor !(RGB Word8)
                   -- ^ corresponding ANSI range:
                   --
                   -- - [0x10-0xE7]:  6 × 6 × 6 cube (216 colors):
                   --             16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5)
                    | GrayColor !Word8
                    -- ^ corresponding ANSI range:
                    --
                    -- - [0xE8-0xFF]:  grayscale from dark gray to near white in 24 steps
                   deriving (Eq, Show, Read)

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
data Underlining = SingleUnderline
                 | DoubleUnderline -- ^ Not widely supported
                 | NoUnderline
                 deriving (Eq, Ord, Bounded ,Enum, Show, Read, Ix)

-- | ANSI general console intensity: usually treated as setting the font style
-- (e.g. 'BoldIntensity' causes text to be bold)
data ConsoleIntensity = BoldIntensity
                      | FaintIntensity -- ^ Not widely supported: sometimes
                                       -- treated as concealing text
                      | NormalIntensity
                      deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI Select Graphic Rendition command
data SGR = Reset
         | SetConsoleIntensity ConsoleIntensity
         | SetItalicized Bool -- ^ Not widely supported: sometimes treated as
                              -- swapping foreground and background
         | SetUnderlining Underlining
         | SetBlinkSpeed BlinkSpeed
         | SetVisible Bool -- ^ Not widely supported
         | SetSwapForegroundBackground Bool
         | SetColor ConsoleLayer ColorIntensity Color
         | SetPaletteColor ConsoleLayer Color8Code -- ^ Supported from Windows 10
                                                   -- Creators Update
         | SetRGBColor ConsoleLayer (Colour Float) -- ^ Supported from Windows 10
                                                   -- Creators Update
         deriving (Eq, Show, Read)
