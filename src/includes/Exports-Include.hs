-- This file contains code that is common to modules
-- System.Console.ANSI.Unix and System.Console.ANSI.Windows, namely the module
-- exports and the associated Haddock documentation.

    -- * Basic data types
    module System.Console.ANSI.Types

    -- * Cursor movement by character
    -- | These code sequences are part of ECMA-48 standard and are portable.
  , cursorUp
  , cursorDown
  , cursorForward
  , cursorBackward
    -- ** \'h...\' variants
  , hCursorUp
  , hCursorDown
  , hCursorForward
  , hCursorBackward
    -- ** \'...Code\' variants
  , cursorUpCode
  , cursorDownCode
  , cursorForwardCode
  , cursorBackwardCode

    -- * Cursor movement by line
    -- | The difference between movements \"by character\" and \"by line\" is
    -- that @*Line@ functions additionally move the cursor to the start of the
    -- line, while functions like @cursorUp@ and @cursorDown@ keep the column
    -- the same.
    --
    -- These code sequences are part of ECMA-48 standard, but some obsolete terminals,
    -- foremost <https://en.wikipedia.org/wiki/ANSI.SYS ANSI.SYS>, do not support them.
    -- Replacing them by more fine-grained calls, e. g., @cursorUp n >> setCursorColumn 0@
    -- instead of @cursorUpLine n@, does not help, because ANSI.SYS does not support
    -- @setCursorColumn@ as well.
  , cursorUpLine
  , cursorDownLine
    -- ** \'h...\' variants
  , hCursorUpLine
  , hCursorDownLine
    -- ** \'...Code\' variants
  , cursorUpLineCode
  , cursorDownLineCode

    -- * Directly changing cursor position
    -- | These code sequences are part of ECMA-48 standard, but some obsolete terminals,
    -- foremost <https://en.wikipedia.org/wiki/ANSI.SYS ANSI.SYS>, do not support
    -- @setCursorColumn@. However, @setCursorPosition@ is supported even by ANSI.SYS.
  , setCursorColumn
  , setCursorPosition
    -- ** \'h...\' variants
  , hSetCursorColumn
  , hSetCursorPosition
    -- ** \'...Code\' variants
  , setCursorColumnCode
  , setCursorPositionCode

    -- * Saving, restoring and reporting cursor position
    -- | These code sequences are not part of ECMA-48 standard; they are popular,
    -- but non-portable extensions. E. g., Terminal.app on MacOS
    -- <https://stackoverflow.com/questions/25879183 does not support them>.
    -- Somewhat surprisingly, ANSI.SYS does.
    --
    -- @terminfo@ package provides a portable way to determine correct sequences
    -- for cursor positions:
    --
    -- >>> import System.Console.Terminfo
    -- >>> t <- setupTermFromEnv
    -- >>> getCapability t (tiGetOutput1 "sc") :: Maybe String -- save cursor position
    -- Just "\ESC7"
    -- >>> getCapability t (tiGetOutput1 "rc") :: Maybe String -- restore cursor position
    -- Just "\ESC8"
    --
    -- On modern scrollable terminal emulators the semantics of cursor positions
    -- is a bit unintuitive:
    -- <https://unix.stackexchange.com/questions/565597 they are relative to the viewport, not to its content>.
    --
  , saveCursor
  , restoreCursor
  , reportCursorPosition
    -- ** \'h...\' variants
  , hSaveCursor
  , hRestoreCursor
  , hReportCursorPosition
    -- ** \'...Code\' variants
  , saveCursorCode
  , restoreCursorCode
  , reportCursorPositionCode

    -- * Clearing parts of the screen
    -- | Note that these functions only clear parts of the screen. They do not
    -- move the cursor. Some functions are based on the whole screen and others
    -- are based on the line in which the cursor is located.
    --
    -- These code sequences are part of ECMA-48 standard.
    -- <https://web.archive.org/web/20171103053038/http://www.drdos.net:80/documentation/usergeng/09ugch9.htm#807 Some versions of ANSI.SYS>
    -- support only @clearScreen@ and @clearFromCursorToLineEnd@, and some offer all variations.
  , clearFromCursorToScreenEnd
  , clearFromCursorToScreenBeginning
  , clearScreen
  , clearFromCursorToLineEnd
  , clearFromCursorToLineBeginning
  , clearLine
    -- ** \'h...\' variants
  , hClearFromCursorToScreenEnd
  , hClearFromCursorToScreenBeginning
  , hClearScreen
  , hClearFromCursorToLineEnd
  , hClearFromCursorToLineBeginning
  , hClearLine  
    -- ** \'...Code\' variants
  , clearFromCursorToScreenEndCode
  , clearFromCursorToScreenBeginningCode
  , clearScreenCode  
  , clearFromCursorToLineEndCode
  , clearFromCursorToLineBeginningCode
  , clearLineCode

    -- * Scrolling the screen
    -- | Scroll the displayed information up or down the terminal.
    -- These code sequences are part of ECMA-48 standard, but ANSI.SYS does not support them.
  , scrollPageUp
  , scrollPageDown
    -- ** \'h...\' variants
  , hScrollPageUp
  , hScrollPageDown
    -- ** \'...Code\' variants
  , scrollPageUpCode
  , scrollPageDownCode

    -- * Select Graphic Rendition mode: colors and other whizzy stuff.
    -- | These code sequences are part of ECMA-48 standard and are portable.
  , setSGR
  , hSetSGR
  , setSGRCode

    -- * Cursor visibilty changes
    -- | These code sequences are not part of ECMA-48 standard; they are popular,
    -- but non-portable extensions. E. g., ANSI.SYS does not support them.
  , hideCursor
  , showCursor
    -- ** \'h...\' variants
  , hHideCursor
  , hShowCursor
    -- ** \'...Code\' variants
  , hideCursorCode
  , showCursorCode

    -- * Hyperlinks
    -- | These code sequences are not part of ECMA-48 standard and not even an
    -- @xterm@ extension. Nevertheless
    -- <https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda many terminals>
    -- support them. On Windows, if emulation is required,
    -- hyperlinks are not emulated.
  , hyperlink
  , hyperlinkWithId
  , hyperlinkWithParams
    -- ** \'h...\' variants
  , hHyperlink
  , hHyperlinkWithId
  , hHyperlinkWithParams
    -- ** \'...Code\' variants
  , hyperlinkCode
  , hyperlinkWithIdCode
  , hyperlinkWithParamsCode

    -- * Changing the title
    -- | These code sequences are not part of ECMA-48 standard, but covered
    -- by @xterm@ extensions. They are not very portable, especially on Windows.
  , setTitle
  , hSetTitle
  , setTitleCode

    -- * Checking if handle supports ANSI (not portable: GHC only)
  , hSupportsANSI
  , hSupportsANSIColor
  , hSupportsANSIWithoutEmulation

    -- * Getting the cursor position
  , getCursorPosition
  , hGetCursorPosition
  , getReportedCursorPosition
  , cursorPosition

    -- * Getting the terminal size
  , getTerminalSize
  , hGetTerminalSize
  
