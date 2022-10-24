-- This file contains code that is common to modules
-- System.Console.ANSI.Unix and System.Console.ANSI.Windows, namely the module
-- exports and the associated Haddock documentation.

    -- * Basic data types
    module System.Console.ANSI.Types

    -- * Cursor movement by character
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
    -- Also keep in mind that @*Line@ functions are not as portable. See
    -- <https://github.com/UnkindPartition/ansi-terminal/issues/10> for the details.
  , cursorUpLine
  , cursorDownLine
    -- ** \'h...\' variants
  , hCursorUpLine
  , hCursorDownLine
    -- ** \'...Code\' variants
  , cursorUpLineCode
  , cursorDownLineCode

    -- * Directly changing cursor position
  , setCursorColumn
  , setCursorPosition
    -- ** \'h...\' variants
  , hSetCursorColumn
  , hSetCursorPosition
    -- ** \'...Code\' variants
  , setCursorColumnCode
  , setCursorPositionCode

    -- * Saving, restoring and reporting cursor position
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
  , scrollPageUp
  , scrollPageDown
    -- ** \'h...\' variants
  , hScrollPageUp
  , hScrollPageDown
    -- ** \'...Code\' variants
  , scrollPageUpCode
  , scrollPageDownCode

    -- * Select Graphic Rendition mode: colors and other whizzy stuff
  , setSGR
  , hSetSGR
  , setSGRCode

    -- * Cursor visibilty changes
  , hideCursor
  , showCursor
    -- ** \'h...\' variants
  , hHideCursor
  , hShowCursor
    -- ** \'...Code\' variants
  , hideCursorCode
  , showCursorCode

    -- * Hyperlinks
    -- | Some, but not all, terminals support hyperlinks - that is, clickable
    -- text that points to a URI. On Windows, if emulation is required,
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
  