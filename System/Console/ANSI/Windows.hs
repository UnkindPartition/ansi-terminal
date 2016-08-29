{-# OPTIONS_HADDOCK hide #-}
module System.Console.ANSI.Windows (
#include "Exports-Include.hs"
    ) where

import System.Console.ANSI.Types
import qualified System.Console.ANSI.Unix as U
import System.Console.ANSI.Windows.Detect (isANSIEnabled)
import qualified System.Console.ANSI.Windows.Emulator as E
import System.IO (Handle, hIsTerminalDevice, stdout)

#include "Common-Include.hs"

-- * Cursor movement by character
hCursorUp = if isANSIEnabled then U.hCursorUp else E.hCursorUp

hCursorDown = if isANSIEnabled then U.hCursorDown else E.hCursorDown

hCursorForward = if isANSIEnabled then U.hCursorForward else E.hCursorForward

hCursorBackward = if isANSIEnabled then U.hCursorBackward else E.hCursorBackward

cursorUpCode :: Int -> String
cursorUpCode = if isANSIEnabled then U.cursorUpCode else E.cursorUpCode

cursorDownCode :: Int -> String
cursorDownCode = if isANSIEnabled then U.cursorDownCode else E.cursorDownCode

cursorForwardCode :: Int -> String
cursorForwardCode = if isANSIEnabled
                    then U.cursorForwardCode
                    else E.cursorForwardCode

cursorBackwardCode :: Int -> String
cursorBackwardCode = if isANSIEnabled
                     then U.cursorBackwardCode
                     else E.cursorBackwardCode

-- * Cursor movement by line
hCursorUpLine = if isANSIEnabled then U.hCursorUpLine else E.hCursorUpLine

hCursorDownLine = if isANSIEnabled then U.hCursorDownLine else E.hCursorDownLine

cursorUpLineCode :: Int -> String
cursorUpLineCode = if isANSIEnabled
                   then U.cursorUpLineCode
                   else E.cursorUpLineCode

cursorDownLineCode :: Int -> String
cursorDownLineCode = if isANSIEnabled
                     then U.cursorDownLineCode
                     else E.cursorDownLineCode

-- * Directly changing cursor position
hSetCursorColumn = if isANSIEnabled
                   then U.hSetCursorColumn
                   else E.hSetCursorColumn

setCursorColumnCode :: Int -> String
setCursorColumnCode = if isANSIEnabled
                      then U.setCursorColumnCode
                      else E.setCursorColumnCode

hSetCursorPosition = if isANSIEnabled
                     then U.hSetCursorPosition
                     else E.hSetCursorPosition

setCursorPositionCode :: Int -> Int -> String
setCursorPositionCode = if isANSIEnabled
                        then U.setCursorPositionCode
                        else E.setCursorPositionCode

-- * Clearing parts of the screen
hClearFromCursorToScreenEnd = if isANSIEnabled
                              then U.hClearFromCursorToScreenEnd
                              else E.hClearFromCursorToScreenEnd

hClearFromCursorToScreenBeginning = if isANSIEnabled
                                    then U.hClearFromCursorToScreenBeginning
                                    else E.hClearFromCursorToScreenBeginning

hClearScreen = if isANSIEnabled then U.hClearScreen else E.hClearScreen

clearFromCursorToScreenEndCode :: String
clearFromCursorToScreenEndCode = if isANSIEnabled
                                 then U.clearFromCursorToScreenEndCode
                                 else E.clearFromCursorToScreenEndCode

clearFromCursorToScreenBeginningCode :: String
clearFromCursorToScreenBeginningCode =
    if isANSIEnabled
    then U.clearFromCursorToScreenBeginningCode
    else E.clearFromCursorToScreenBeginningCode

clearScreenCode :: String
clearScreenCode = if isANSIEnabled then U.clearScreenCode else E.clearScreenCode

hClearFromCursorToLineEnd = if isANSIEnabled
                            then U.hClearFromCursorToLineEnd
                            else E.hClearFromCursorToLineEnd

hClearFromCursorToLineBeginning = if isANSIEnabled
                                  then U.hClearFromCursorToLineBeginning
                                  else E.hClearFromCursorToLineBeginning

hClearLine = if isANSIEnabled then U.hClearLine else E.hClearLine

clearFromCursorToLineEndCode :: String
clearFromCursorToLineEndCode = if isANSIEnabled
                               then U.clearFromCursorToLineEndCode
                               else E.clearFromCursorToLineEndCode

clearFromCursorToLineBeginningCode :: String
clearFromCursorToLineBeginningCode = if isANSIEnabled
                                     then U.clearFromCursorToLineBeginningCode
                                     else E.clearFromCursorToLineBeginningCode

clearLineCode :: String
clearLineCode = if isANSIEnabled then U.clearLineCode else E.clearLineCode

-- * Scrolling the screen
hScrollPageUp = if isANSIEnabled then U.hScrollPageUp else E.hScrollPageUp
hScrollPageDown = if isANSIEnabled then U.hScrollPageDown else E.hScrollPageDown

scrollPageUpCode :: Int -> String
scrollPageUpCode = if isANSIEnabled
                   then U.scrollPageUpCode
                   else E.scrollPageUpCode

scrollPageDownCode :: Int -> String
scrollPageDownCode = if isANSIEnabled
                     then U.scrollPageDownCode
                     else E.scrollPageDownCode

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

hSetSGR = if isANSIEnabled then U.hSetSGR else E.hSetSGR

setSGRCode :: [SGR] -> String
setSGRCode = if isANSIEnabled then U.setSGRCode else E.setSGRCode

-- * Cursor visibilty changes
hHideCursor = if isANSIEnabled then U.hHideCursor else E.hHideCursor

hShowCursor = if isANSIEnabled then U.hShowCursor else E.hShowCursor

hideCursorCode :: String
hideCursorCode = if isANSIEnabled then U.hideCursorCode else E.hideCursorCode

showCursorCode :: String
showCursorCode = if isANSIEnabled then U.showCursorCode else E.showCursorCode

-- * Changing the title
hSetTitle = if isANSIEnabled then U.hSetTitle else E.hSetTitle

setTitleCode :: String -> String
setTitleCode = if isANSIEnabled then U.setTitleCode else E.setTitleCode
