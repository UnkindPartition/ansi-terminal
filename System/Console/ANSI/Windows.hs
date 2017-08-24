{-# OPTIONS_HADDOCK hide #-}
module System.Console.ANSI.Windows (
#include "Exports-Include.hs"
    ) where

import System.Console.ANSI.Types
import qualified System.Console.ANSI.Unix as U
import System.Console.ANSI.Windows.Detect (ANSIEnabledStatus (..),
    ConsoleDefaultState (..), isANSIEnabled)
import qualified System.Console.ANSI.Windows.Emulator as E
import System.IO (Handle, hIsTerminalDevice, stdout)

#include "Common-Include.hs"
#include "Common-Include-Enabled.hs"

-- | A helper function which returns the native or emulated version, depending
-- on `isANSIEnabled`.
nativeOrEmulated :: a -> a -> a
nativeOrEmulated native emulated = case isANSIEnabled of
    ANSIEnabled      -> native
    NotANSIEnabled _ -> emulated

-- | A helper function which returns the native or emulated version, depending
-- on `isANSIEnabled`, where the emulator uses the default console state.
nativeOrEmulatedWithDefault :: a -> (ConsoleDefaultState -> a) -> a
nativeOrEmulatedWithDefault native emulated = case isANSIEnabled of
    ANSIEnabled        -> native
    NotANSIEnabled def -> emulated def


-- * Cursor movement by character
hCursorUp       = nativeOrEmulated U.hCursorUp       E.hCursorUp
hCursorDown     = nativeOrEmulated U.hCursorDown     E.hCursorDown
hCursorForward  = nativeOrEmulated U.hCursorForward  E.hCursorForward
hCursorBackward = nativeOrEmulated U.hCursorBackward E.hCursorBackward

cursorUpCode :: Int -> String
cursorUpCode = nativeOrEmulated U.cursorUpCode E.cursorUpCode

cursorDownCode :: Int -> String
cursorDownCode = nativeOrEmulated U.cursorDownCode E.cursorDownCode

cursorForwardCode :: Int -> String
cursorForwardCode = nativeOrEmulated U.cursorForwardCode E.cursorForwardCode

cursorBackwardCode :: Int -> String
cursorBackwardCode = nativeOrEmulated U.cursorBackwardCode E.cursorBackwardCode

-- * Cursor movement by line
hCursorUpLine   = nativeOrEmulated U.hCursorUpLine   E.hCursorUpLine
hCursorDownLine = nativeOrEmulated U.hCursorDownLine E.hCursorDownLine

cursorUpLineCode :: Int -> String
cursorUpLineCode = nativeOrEmulated U.cursorUpLineCode E.cursorUpLineCode

cursorDownLineCode :: Int -> String
cursorDownLineCode = nativeOrEmulated U.cursorDownLineCode E.cursorDownLineCode

-- * Directly changing cursor position
hSetCursorColumn = nativeOrEmulated U.hSetCursorColumn E.hSetCursorColumn

setCursorColumnCode :: Int -> String
setCursorColumnCode = nativeOrEmulated
    U.setCursorColumnCode E.setCursorColumnCode

hSetCursorPosition = nativeOrEmulated U.hSetCursorPosition E.hSetCursorPosition

setCursorPositionCode :: Int -> Int -> String
setCursorPositionCode = nativeOrEmulated
    U.setCursorPositionCode E.setCursorPositionCode

-- * Clearing parts of the screen
hClearFromCursorToScreenEnd = nativeOrEmulatedWithDefault
    U.hClearFromCursorToScreenEnd E.hClearFromCursorToScreenEnd
hClearFromCursorToScreenBeginning = nativeOrEmulatedWithDefault
    U.hClearFromCursorToScreenBeginning E.hClearFromCursorToScreenBeginning
hClearScreen = nativeOrEmulatedWithDefault U.hClearScreen E.hClearScreen

clearFromCursorToScreenEndCode :: String
clearFromCursorToScreenEndCode = nativeOrEmulated
    U.clearFromCursorToScreenEndCode E.clearFromCursorToScreenEndCode

clearFromCursorToScreenBeginningCode :: String
clearFromCursorToScreenBeginningCode = nativeOrEmulated
    U.clearFromCursorToScreenBeginningCode E.clearFromCursorToScreenBeginningCode

clearScreenCode :: String
clearScreenCode = nativeOrEmulated U.clearScreenCode E.clearScreenCode

hClearFromCursorToLineEnd = nativeOrEmulatedWithDefault
    U.hClearFromCursorToLineEnd E.hClearFromCursorToLineEnd
hClearFromCursorToLineBeginning = nativeOrEmulatedWithDefault
    U.hClearFromCursorToLineBeginning E.hClearFromCursorToLineBeginning
hClearLine = nativeOrEmulatedWithDefault U.hClearLine E.hClearLine

clearFromCursorToLineEndCode :: String
clearFromCursorToLineEndCode = nativeOrEmulated
    U.clearFromCursorToLineEndCode E.clearFromCursorToLineEndCode

clearFromCursorToLineBeginningCode :: String
clearFromCursorToLineBeginningCode = nativeOrEmulated
    U.clearFromCursorToLineBeginningCode E.clearFromCursorToLineBeginningCode

clearLineCode :: String
clearLineCode = nativeOrEmulated U.clearLineCode E.clearLineCode

-- * Scrolling the screen
hScrollPageUp   = nativeOrEmulatedWithDefault U.hScrollPageUp   E.hScrollPageUp
hScrollPageDown = nativeOrEmulatedWithDefault U.hScrollPageDown E.hScrollPageDown

scrollPageUpCode :: Int -> String
scrollPageUpCode = nativeOrEmulated U.scrollPageUpCode E.scrollPageUpCode

scrollPageDownCode :: Int -> String
scrollPageDownCode = nativeOrEmulated U.scrollPageDownCode E.scrollPageDownCode

-- * Select Graphic Rendition mode: colors and other whizzy stuff
--
-- The following SGR codes are NOT implemented by Windows 10 Threshold 2:
-- 2   SetConsoleIntensity FaintIntensity
-- 3   SetItalicized True
-- 5   SetBlinkSpeed SlowBlink
-- 6   SetBlinkSpeed RapidBlink
-- 8   SetVisible False
-- 21  SetUnderlining DoubleUnderline
-- 23  SetItalicized False
-- 25  SetBlinkSpeed NoBlink
-- 28  SetVisible True

hSetSGR = nativeOrEmulatedWithDefault U.hSetSGR E.hSetSGR

setSGRCode :: [SGR] -> String
setSGRCode = nativeOrEmulated U.setSGRCode E.setSGRCode

-- * Cursor visibilty changes
hHideCursor = nativeOrEmulated U.hHideCursor E.hHideCursor
hShowCursor = nativeOrEmulated U.hShowCursor E.hShowCursor

hideCursorCode :: String
hideCursorCode = nativeOrEmulated U.hideCursorCode E.hideCursorCode

showCursorCode :: String
showCursorCode = nativeOrEmulated U.showCursorCode E.showCursorCode

-- * Changing the title
hSetTitle = nativeOrEmulated U.hSetTitle E.hSetTitle

setTitleCode :: String -> String
setTitleCode = nativeOrEmulated U.setTitleCode E.setTitleCode
