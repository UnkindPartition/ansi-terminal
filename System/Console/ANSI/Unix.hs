{-# OPTIONS_HADDOCK hide #-}
module System.Console.ANSI.Unix (
#include "Exports-Include.hs"
    ) where

import System.Console.ANSI.Codes
import System.Console.ANSI.Types
import System.IO (Handle, hIsTerminalDevice, hPutStr, stdout)

#include "Common-Include.hs"
#include "Common-Include-Enabled.hs"

hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n

hCursorDownLine h n = hPutStr h $ cursorDownLineCode n
hCursorUpLine h n = hPutStr h $ cursorUpLineCode n

hSetCursorColumn h n = hPutStr h $ setCursorColumnCode n
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m

hClearFromCursorToScreenEnd h = hPutStr h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h = hPutStr h clearFromCursorToScreenBeginningCode
hClearScreen h = hPutStr h clearScreenCode

hClearFromCursorToLineEnd h = hPutStr h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = hPutStr h clearFromCursorToLineBeginningCode
hClearLine h = hPutStr h clearLineCode

hScrollPageUp h n = hPutStr h $ scrollPageUpCode n
hScrollPageDown h n = hPutStr h $ scrollPageDownCode n

hSetSGR h sgrs = hPutStr h $ setSGRCode sgrs

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode

hSetTitle h title = hPutStr h $ setTitleCode title
