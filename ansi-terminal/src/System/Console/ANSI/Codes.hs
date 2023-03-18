{-# LANGUAGE Safe #-}

{-| This module exports functions that return 'String' values containing codes
in accordance with the \'ANSI\' standards for control character sequences
described in the documentation of module "System.Console.ANSI".
-}
module System.Console.ANSI.Codes
  (
    -- * Basic data types
    module System.Console.ANSI.Types

    -- * Cursor movement by character
    --
    -- | These functions yield @\"\"@ when the number is @0@ as, on some
    -- terminals, a @0@ parameter for the underlying \'ANSI\' code specifies a
    -- default parameter of @1@.
  , cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode

    -- * Cursor movement by line
    --
    -- | These functions yield the equivalent of @setCursorColumnCode 0@ when
    -- the number is @0@ as, on some terminals, a @0@ parameter for the
    -- underlying \'ANSI\' code specifies a default parameter of @1@.
  , cursorUpLineCode, cursorDownLineCode

    -- * Directly changing cursor position
  , setCursorColumnCode, setCursorPositionCode

    -- * Saving, restoring and reporting cursor position
  , saveCursorCode, restoreCursorCode, reportCursorPositionCode

    -- * Clearing parts of the screen
  , clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode
  , clearScreenCode, clearFromCursorToLineEndCode
  , clearFromCursorToLineBeginningCode, clearLineCode

    -- * Scrolling the screen
    --
    -- | These functions yield @\"\"@ when the number is @0@ as, on some
    -- terminals, a @0@ parameter for the underlying \'ANSI\' code specifies a
    -- default parameter of @1@.
  , scrollPageUpCode, scrollPageDownCode

    -- * Using screen buffers
  , useAlternateScreenBufferCode, useNormalScreenBufferCode

    -- * Reporting background or foreground colors
  , reportLayerColorCode

    -- * Select Graphic Rendition mode: colors and other whizzy stuff
  , setSGRCode

    -- * Cursor visibilty changes
  , hideCursorCode, showCursorCode

    -- * Hyperlinks
    -- | Some, but not all, terminals support hyperlinks - that is, clickable
    -- text that points to a URI.
  , hyperlinkCode, hyperlinkWithIdCode, hyperlinkWithParamsCode

    -- * Changing the title
  , setTitleCode

    -- * Utilities
  , colorToCode, csi, osc, sgrToCode
  ) where

import Data.Char (isPrint)
import Data.List (intercalate)

import Data.Colour.SRGB (toSRGB24, RGB (..))

import System.Console.ANSI.Types

-- | 'csi' @parameters controlFunction@, where @parameters@ is a list of 'Int',
-- returns the control sequence comprising the control function CONTROL
-- SEQUENCE INTRODUCER (CSI) followed by the parameter(s) (separated by \';\')
-- and ending with the @controlFunction@ character(s) that identifies the
-- control function.
csi ::
     [Int]  -- ^ List of parameters for the control sequence
  -> String -- ^ Character(s) that identify the control function
  -> String
csi args code = "\ESC[" ++ intercalate ";" (map show args) ++ code

-- | 'osc' @parameterS parametersT@, where @parameterS@ specifies the type of
-- operation to perform and @parametersT@ is the other parameter(s) (if any),
-- returns the control sequence comprising the control function OPERATING SYSTEM
-- COMMAND (OSC) followed by the parameters (separated by \';\') and ending with
-- the STRING TERMINATOR (ST) @\"\\ESC\\\\\"@.
--
-- @since 0.11.4
osc ::
     String -- ^ Ps parameter
  -> String -- ^ Pt parameter(s)
  -> String
osc pS pT = "\ESC]" ++ pS ++ ";" ++ pT ++ "\ESC\\"

-- | 'colorToCode' @color@ returns the 0-based index of the color (one of the
-- eight colors in the ANSI standard).
colorToCode :: Color -> Int
colorToCode color = case color of
  Black   -> 0
  Red     -> 1
  Green   -> 2
  Yellow  -> 3
  Blue    -> 4
  Magenta -> 5
  Cyan    -> 6
  White   -> 7

-- | 'sgrToCode' @sgr@ returns the parameter of the SELECT GRAPHIC RENDITION
-- (SGR) aspect identified by @sgr@.
sgrToCode ::
     SGR -- ^ The SGR aspect
  -> [Int]
sgrToCode sgr = case sgr of
  Reset -> [0]
  SetConsoleIntensity intensity -> case intensity of
    BoldIntensity   -> [1]
    FaintIntensity  -> [2]
    NormalIntensity -> [22]
  SetItalicized True  -> [3]
  SetItalicized False -> [23]
  SetUnderlining underlining -> case underlining of
    SingleUnderline -> [4]
    DoubleUnderline -> [21]
    NoUnderline     -> [24]
  SetBlinkSpeed blink_speed -> case blink_speed of
    SlowBlink   -> [5]
    RapidBlink  -> [6]
    NoBlink     -> [25]
  SetVisible False -> [8]
  SetVisible True  -> [28]
  SetSwapForegroundBackground True  -> [7]
  SetSwapForegroundBackground False -> [27]
  SetColor Foreground Dull color  -> [30 + colorToCode color]
  SetColor Foreground Vivid color -> [90 + colorToCode color]
  SetColor Background Dull color  -> [40 + colorToCode color]
  SetColor Background Vivid color -> [100 + colorToCode color]
  SetPaletteColor Foreground index -> [38, 5, fromIntegral index]
  SetPaletteColor Background index -> [48, 5, fromIntegral index]
  SetRGBColor Foreground color -> [38, 2] ++ toRGB color
  SetRGBColor Background color -> [48, 2] ++ toRGB color
  SetDefaultColor Foreground -> [39]
  SetDefaultColor Background -> [49]
 where
  toRGB color = let RGB r g b = toSRGB24 color
                in  map fromIntegral [r, g, b]

cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode ::
     Int -- ^ Number of lines or characters to move
  -> String
cursorUpCode n = if n == 0 then "" else csi [n] "A"
cursorDownCode n = if n == 0 then "" else csi [n] "B"
cursorForwardCode n = if n == 0 then "" else csi [n] "C"
cursorBackwardCode n = if n == 0 then "" else csi [n] "D"

cursorDownLineCode, cursorUpLineCode ::
     Int -- ^ Number of lines to move
  -> String
cursorDownLineCode n = if n == 0 then csi [1] "G" else csi [n] "E"
cursorUpLineCode n = if n == 0 then csi [1] "G" else csi [n] "F"

-- | Code to move the cursor to the specified column. The column numbering is
-- 0-based (that is, the left-most column is numbered 0).
setCursorColumnCode ::
     Int -- ^ 0-based column to move to
  -> String
setCursorColumnCode n = csi [n + 1] "G"

-- | Code to move the cursor to the specified position (row and column). The
-- position is 0-based (that is, the top-left corner is at row 0 column 0).
setCursorPositionCode ::
     Int -- ^ 0-based row to move to
  -> Int -- ^ 0-based column to move to
  -> String
setCursorPositionCode n m = csi [n + 1, m + 1] "H"

-- | @since 0.7.1
saveCursorCode, restoreCursorCode :: String
saveCursorCode = "\ESC7"
restoreCursorCode = "\ESC8"

-- | Code to emit the cursor position into the console input stream, immediately
-- after being recognised on the output stream, as:
-- @ESC [ \<cursor row> ; \<cursor column> R@
--
-- Note that the information that is emitted is 1-based (the top-left corner is
-- at row 1 column 1) but 'setCursorPositionCode' is 0-based.
--
-- In isolation of 'System.Console.ANSI.getReportedCursorPosition' or
-- 'System.Console.ANSI.getCursorPosition', this function may be of limited use
-- on Windows operating systems because of difficulties in obtaining the data
-- emitted into the console input stream.
--
-- @since 0.7.1
reportCursorPositionCode :: String
reportCursorPositionCode = csi [] "6n"

-- | Code to emit the layer color into the console input stream, immediately
-- after being recognised on the output stream, as:
-- @ESC ] \<Ps> ; rgb: \<red> ; \<green> ; \<blue> \<ST>@
-- where @\<Ps>@ is @10@ for 'Foreground' and @11@ for 'Background'; @\<red>@,
-- @\<green>@ and @\<blue>@ are the color channel values in hexadecimal (4, 8,
-- 12 and 16 bit values are possible, although 16 bit values are most common);
-- and @\<ST>@ is the STRING TERMINATOR (ST). ST depends on the terminal
-- software and may be the @BEL@ character or @ESC \\@ characters.
--
-- This function may be of limited, or no, use on Windows operating systems
-- because (1) the control character sequence is not supported on native
-- terminals (2) of difficulties in obtaining the data emitted into the
-- console input stream. See 'System.Console.ANSI.getReportedLayerColor'.
--
-- @since 0.11.4
reportLayerColorCode :: ConsoleLayer -> String
reportLayerColorCode Foreground = osc "10" "?"
reportLayerColorCode Background = osc "11" "?"

clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode,
  clearScreenCode :: String
clearFromCursorToLineEndCode, clearFromCursorToLineBeginningCode,
  clearLineCode :: String

clearFromCursorToScreenEndCode = csi [0] "J"
clearFromCursorToScreenBeginningCode = csi [1] "J"
clearScreenCode = csi [2] "J"
clearFromCursorToLineEndCode = csi [0] "K"
clearFromCursorToLineBeginningCode = csi [1] "K"
clearLineCode = csi [2] "K"

scrollPageUpCode, scrollPageDownCode ::
     Int -- ^ Number of lines to scroll by
  -> String
scrollPageUpCode n = if n == 0 then "" else csi [n] "S"
scrollPageDownCode n = if n == 0 then "" else csi [n] "T"

useAlternateScreenBufferCode, useNormalScreenBufferCode :: String
useAlternateScreenBufferCode = csi [] "?1049h"
useNormalScreenBufferCode = csi [] "?1049l"

setSGRCode ::
     [SGR]
     -- ^ Commands: these will typically be applied on top of the current
     -- console SGR mode. An empty list of commands is equivalent to the list
     -- @[Reset]@. Commands are applied left to right.
  -> String
setSGRCode sgrs = csi (concatMap sgrToCode sgrs) "m"

hideCursorCode, showCursorCode :: String
hideCursorCode = csi [] "?25l"
showCursorCode = csi [] "?25h"

-- | Code to introduce a hyperlink with (key, value) parameters. Some terminals
-- support an @id@ parameter key, so that hyperlinks with the same @id@ value
-- are treated as connected.
--
-- @since 0.11.3
hyperlinkWithParamsCode ::
     [(String, String)]
     -- ^ Parameters
  -> String
     -- ^ URI
  -> String
     -- ^ Link text
  -> String
hyperlinkWithParamsCode params uri link =
  osc "8" pT ++ link ++ osc "8" ";"
 where
  pT = params' ++ ";" ++ uri
  params' = intercalate ":" $ map (\(k, v) -> k ++ "=" ++ v) params

-- | Code to introduce a hyperlink.
--
-- @since 0.11.3
hyperlinkCode ::
     String
     -- ^ URI
  -> String
     -- ^ Link text
  -> String
hyperlinkCode = hyperlinkWithParamsCode []

-- | Code to introduce a hyperlink with an identifier for the link. Some
-- terminals support an identifier, so that hyperlinks with the same identifier
-- are treated as connected.
--
-- @since 0.11.3
hyperlinkWithIdCode ::
     String
     -- ^ Identifier for the link
  -> String
     -- ^ URI
  -> String
     -- ^ Link text
  -> String
hyperlinkWithIdCode linkId = hyperlinkWithParamsCode [("id", linkId)]

-- | Code to set the terminal window title and the icon name (that is, the text
-- for the window in the Start bar, or similar).

-- Thanks to Brandon S. Allbery and Curt Sampson for pointing me in the right
-- direction on xterm title setting on haskell-cafe. The "0" signifies that both
-- the title and "icon" text should be set. This is chosen for consistent
-- behaviour between Unixes and Windows.
setTitleCode ::
     String
     -- ^ New window title and icon name
  -> String
setTitleCode title = osc "0" (filter isPrint title)
