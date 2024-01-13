{-# LANGUAGE Safe #-}

{-| == Introduction
Through this module, this library provides platform-independent support for
control character sequences following the \'ANSI\' standards (see further below)
for terminal software that supports those sequences, running on a Unix-like
operating system or on Windows (see further below).

The sequences of control characters (also referred to as \'escape\' sequences or
codes) provide a rich range of functionality for terminal control, which
includes:

 * Colored text output, with control over both foreground and background colors

 * Clearing parts of a line or the screen

 * Hiding or showing the cursor

 * Moving the cursor around

 * Reporting the position of the cursor

 * Scrolling the screen up or down

 * Switching between the Alternate and Normal Screen Buffers

 * Clickable hyperlinks to URIs

 * Changing the title of the terminal

A terminal that supports control character sequences acts on them when they
are flushed from the output buffer (with a newline character @\"\\n\"@ or, for
the standard output channel, @hFlush stdout@).

== \'ANSI\' standards
The \'ANSI\' standards refer to (1) standard ECMA-48 \`Control Functions for
Coded Character Sets\' (5th edition, 1991); (2) extensions in ITU-T
Recommendation (previously CCITT Recommendation) T.416 (03/93) \'Information
Technology – Open Document Architecture (ODA) and Interchange Format: Character
Content Architectures\` (also published as ISO/IEC International Standard
8613-6); and (3) further extensions used by \'XTerm\', a terminal emulator for
the X Window System. The escape codes are described in a
  [Wikipedia article](http://en.wikipedia.org/wiki/ANSI_escape_code) and those
codes supported on current versions of Windows are descibed in
  [Microsoft's documentation](https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences).

The whole of the \'ANSI\' standards are not supported by this library but most
(if not all) of the parts that are popular and well-supported by terminal
software are supported (see further below).

== Cursor positions
The functions moving the cursor to an absolute position are 0-based (the
top-left corner is considered to be at row 0 column 0) (see 'setCursorPosition')
and so is 'getCursorPosition'. The \'ANSI\' standards themselves are 1-based
(that is, the top-left corner is considered to be at row 1 column 1) and some
functions reporting the position of the cursor are too (see
'reportCursorPosition').

== Windows and control character sequences
The native terminal software on Windows has developed over time. Before
Windows 10 version 1511 (known as the \'November [2015] Update\' or
\'Threshold 2\') that software did not support control character sequences. From
2018, Microsoft introduced the Windows Pseudo Console (\'ConPTY\') API and then
Windows Terminal, with the objective of replacing most of the Windows Console
API with the use of control character sequences and retiring the historical
user-interface role of Windows Console Host (\'ConHost\').

Windows Terminal is supported on Windows 10 version 19041.0 or higher and
provided with Windows 11. It can be downloaded from the Microsoft Store. Windows
Terminal can be set as the default terminal application on Windows 10 (from
the 22H2 update) and is the default application on Windows 11 (from the 22H2
update).

Despite the above developments, some Windows users may continue to use ConHost.
ConHost does not enable the processing of \'ANSI\' control characters in output
by default. See 'hNowSupportsANSI' for a function that can try to enable such
processing.

Terminal software other than the native software exists for Windows. One example
is the \'mintty\' terminal emulator for \'Cygwin\', \'MSYS\' or \'MSYS2\', and
dervied projects, and for \'WSL\' (Windows Subsystem for Linux).

GHC's management of input and output (IO) on Windows has also developed over
time. If they are supported by the terminal software, some control character
sequences cause data to be emitted into the console input stream. For GHC's
historical and default IO manager, the function 'hGetBufNonBlocking' in module
"System.IO" does not work on Windows. This has been attributed to the lack of
non-blocking primatives in the operating system (see
  [GHC bug report #806](https://ghc.haskell.org/trac/ghc/ticket/806). GHC's
native IO manager on Windows (\'WinIO\'), introduced as a preview in
  [GHC 9.0.1](https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html#highlights),
has not yet provided a solution. On Windows, this library uses emulation based
on the Windows Console API to try to read data emitted into the console input
stream. Functions that use that emulation are not supported on consoles, such
as mintty, that are not based on that API.

== Function variants provided
Every function exported by this module comes in three variants, namely:

 * A variant that has an @IO ()@ type and doesn't take a @Handle@ (for example,
   @clearScreen :: IO ()@). This variant just outputs the \`ANSI\` command
   directly to the standard output channel ('stdout') and any terminal
   corresponding to it. Commands issued like this should work as you expect on
   both Unix-like operating systems and Windows (unless exceptions on Windows
   are stated).

 * An \'@h@...\' variant that has an @IO ()@ type but takes a @Handle@ (for
   example, @hClearScreen :: Handle -> IO ()@). This variant outputs the
   \`ANSI\` command to the supplied handle and any terminal corresponding to it.
   Commands issued like this should also work as you expect on both Unix-like
   operating systems and Windows (unless exceptions on Windows are stated).

 * A \'...@Code@\' variant that has a @String@ type (for example,
   @clearScreenCode :: String@). This variant outputs the sequence of control
   characters as a 'String', which can be added to any other bit of text before
   being output. If a high degree of backwards compatability is rewuired, the
   use of these codes is discouraged because they will not work on legacy
   versions of Windows where the terminal in use is not ANSI-enabled (see
   further above). On Windows, where emulation has been necessary, these
   variants will always output the empty string. That is done so that it is
   possible to use them portably; for example, coloring console output on the
   understanding that you will see colors only if you are running on a Unix-like
   operating system or a version of Windows where emulation has not been
   necessary. If the control characters are always required, see module
   "System.Console.ANSI.Codes".

== Examples of use

A simple example is below:

> module Main where
>
> import System.Console.ANSI
> import System.IO (stdout)
>
> -- Set colors and write some text in those colors.
> main :: IO ()
> main = do
>   stdoutSupportsANSI <- hNowSupportsANSI stdout
>   if stdoutSupportsANSI
>     then do
>       setSGR [SetColor Foreground Vivid Red]
>       setSGR [SetColor Background Vivid Blue]
>       putStrLn "Red-On-Blue"
>       setSGR [Reset]  -- Reset to default colour scheme
>       putStrLn "Default colors."
>     else
>       putStrLn "Standard output does not support 'ANSI' escape codes."

Another example is below:

> module Main where
>
> import System.IO (hFlush, stdout)
> import System.Console.ANSI
>
> main :: IO ()
> main = do
>   stdoutSupportsANSI <- hNowSupportsANSI stdout
>   if stdoutSupportsANSI
>     then do
>       setSGR [SetColor Foreground Dull Blue]
>       putStr "Enter your name: "
>       setSGR [SetColor Foreground Dull Yellow]
>       hFlush stdout  -- flush the output buffer before getLine
>       name <- getLine
>       setSGR [SetColor Foreground Dull Blue]
>       putStrLn $ "Hello, " ++ name ++ "!"
>       setSGR [Reset]  -- reset to default colour scheme
>     else
>       putStrLn "Standard output does not support 'ANSI' escape codes."

For many more examples, see the project's extensive
<https://github.com/UnkindPartition/ansi-terminal/blob/master/app/Example.hs Example.hs> file.
-}

module System.Console.ANSI
  (
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
    -- | These code sequences are not part of ECMA-48 standard; they are popular,
    -- but non-portable extensions. E. g., Terminal.app on MacOS
    -- <https://stackoverflow.com/questions/25879183 does not support them>.
    -- A more portable way would be to query @terminfo@ database
    -- for @rc@ and @sc@ capabilities.
    --
    -- Cursor positions
    -- <https://unix.stackexchange.com/questions/565597 are relative to the viewport, not to its content>.
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

    -- * Using screen buffers
    -- | These code sequences are not part of ECMA-48 standard; they are popular,
    -- but non-portable extensions, corresponding to @smcup@ and @rmcup@ capabilities
    -- in @terminfo@ database.
    -- On Windows, if emulation is required, switching between alternate and
    -- normal screen buffers is not emulated.
  , useAlternateScreenBuffer
  , useNormalScreenBuffer
    -- ** \'h...\' variants
  , hUseAlternateScreenBuffer
  , hUseNormalScreenBuffer
    -- ** \'...Code\' variants
  , useAlternateScreenBufferCode
  , useNormalScreenBufferCode

    -- * Reporting the background or foreground colors
  , reportLayerColor
  , hReportLayerColor
  , reportLayerColorCode

    -- * Select Graphic Rendition mode: colors and other whizzy stuff
  , setSGR
  , hSetSGR
  , setSGRCode

    -- * Cursor visibilty changes
    -- | Strictly speaking, these code sequences are not part of ECMA-48 standard;
    -- they are popular, but non-portable extensions. However, in practice they seem
    -- to work pretty much everywhere.
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
  , setTitle
  , hSetTitle
  , setTitleCode

    -- * Checking if handle supports ANSI (not portable: GHC only)
  , hSupportsANSI
  , hNowSupportsANSI
  , hSupportsANSIColor

    -- * Getting the cursor position
  , getCursorPosition
  , hGetCursorPosition
  , getReportedCursorPosition
  , cursorPosition

    -- * Getting the terminal size
  , getTerminalSize
  , hGetTerminalSize

    -- * Getting the background or foreground colors
  , getLayerColor
  , hGetLayerColor
  , getReportedLayerColor
  , layerColor

    -- * Deprecated
  , hSupportsANSIWithoutEmulation
  ) where

import Control.Exception.Base ( bracket )
import Control.Monad ( when, void )
import Data.Char ( digitToInt, isDigit, isHexDigit )
import Data.Colour.SRGB ( RGB (..) )
import Data.Word ( Word16 )
import System.Environment ( getEnvironment )
import System.IO
         ( BufferMode (..), Handle, hFlush, hGetBuffering, hGetEcho, hPutStr
         , hReady, hSetBuffering, hSetEcho, stdin, stdout
         )
import Text.ParserCombinators.ReadP
         ( ReadP, (<++), char, many1, readP_to_S, satisfy, string )

import System.Console.ANSI.Codes
import qualified System.Console.ANSI.Internal as Internal
import System.Console.ANSI.Types

hCursorUp, hCursorDown, hCursorForward, hCursorBackward ::
     Handle
  -> Int -- Number of lines or characters to move
  -> IO ()
hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n

cursorUp, cursorDown, cursorForward, cursorBackward ::
     Int -- ^ Number of lines or characters to move
  -> IO ()
cursorUp = hCursorUp stdout
cursorDown = hCursorDown stdout
cursorForward = hCursorForward stdout
cursorBackward = hCursorBackward stdout

hCursorDownLine, hCursorUpLine ::
     Handle
  -> Int -- Number of lines to move
  -> IO ()
hCursorDownLine h n = hPutStr h $ cursorDownLineCode n
hCursorUpLine h n = hPutStr h $ cursorUpLineCode n

cursorDownLine, cursorUpLine ::
     Int -- ^ Number of lines to move
  -> IO ()
cursorDownLine = hCursorDownLine stdout
cursorUpLine = hCursorUpLine stdout

hSetCursorColumn ::
     Handle
  -> Int -- 0-based column to move to
  -> IO ()
hSetCursorColumn h n = hPutStr h $ setCursorColumnCode n

-- | Move the cursor to the specified column. The column numbering is 0-based
-- (that is, the left-most column is numbered 0).
setCursorColumn ::
     Int -- ^ 0-based column to move to
  -> IO ()
setCursorColumn = hSetCursorColumn stdout

hSetCursorPosition ::
     Handle
  -> Int -- 0-based row to move to
  -> Int -- 0-based column to move to
  -> IO ()
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m

-- | Move the cursor to the specified position (row and column). The position is
-- 0-based (that is, the top-left corner is at row 0 column 0).
setCursorPosition ::
     Int -- ^ 0-based row to move to
  -> Int -- ^ 0-based column to move to
  -> IO ()
setCursorPosition = hSetCursorPosition stdout

hSaveCursor, hRestoreCursor, hReportCursorPosition :: Handle -> IO ()
hSaveCursor h = hPutStr h saveCursorCode
hRestoreCursor h = hPutStr h restoreCursorCode
hReportCursorPosition h = hPutStr h reportCursorPositionCode

-- | Save the cursor position in memory. The only way to access the saved value
-- is with the 'restoreCursor' command.
--
-- @since 0.7.1
saveCursor :: IO ()
saveCursor = hSaveCursor stdout

-- | Restore the cursor position from memory. There will be no value saved in
-- memory until the first use of the 'saveCursor' command.
--
-- @since 0.7.1
restoreCursor :: IO ()
restoreCursor = hRestoreCursor stdout

-- | Looking for a way to get the cursors position? See
-- 'getCursorPosition'.
--
-- Emit the cursor position into the console input stream, immediately after
-- being recognised on the output stream, as:
-- @ESC [ \<cursor row> ; \<cursor column> R@
--
-- Note that the information that is emitted is 1-based (the top-left corner is
-- at row 1 column 1) but 'setCursorColumn' and 'setCursorPosition' are
-- 0-based.
--
-- In isolation of 'getReportedCursorPosition' or 'getCursorPosition', this
-- function may be of limited use on Windows operating systems because of
-- difficulties in obtaining the data emitted into the console input stream.
--
-- @since 0.7.1
reportCursorPosition :: IO ()
reportCursorPosition = hReportCursorPosition stdout

hHideCursor, hShowCursor :: Handle -> IO ()
hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode

hideCursor, showCursor :: IO ()
hideCursor = hHideCursor stdout
showCursor = hShowCursor stdout

hUseAlternateScreenBuffer :: Handle -> IO ()
hUseAlternateScreenBuffer h = hPutStr h useAlternateScreenBufferCode

hUseNormalScreenBuffer :: Handle -> IO ()
hUseNormalScreenBuffer h = hPutStr h useNormalScreenBufferCode

-- | Use the Alternate Screen Buffer. If currently using the Normal Screen
-- Buffer, it will save the cursor position and switch to the Alternate Screen
-- Buffer. It will always clear the Alternate Screen Buffer. The Alternate
-- Screen Buffer has no scroll back facility.
--
-- It is an application's responsibility to ensure that it switches back to the
-- Normal Screen Buffer if an exception is raised while the Alternate Screen
-- Buffer is being used. For example, by using 'Control.Exception.bracket_':
--
-- > bracket_ useAlternateScreenBuffer useNormalScreenBuffer action
--
-- @since 0.11.4
useAlternateScreenBuffer :: IO ()
useAlternateScreenBuffer = hUseAlternateScreenBuffer stdout

-- | Use the Normal Screen Buffer. If currently using the Alternate Screen
-- Buffer, it will clear the Alternate Screen Buffer, and switch to the Normal
-- Screen Buffer. It will always restore the saved cursor position.
--
-- @since 0.11.4
useNormalScreenBuffer :: IO ()
useNormalScreenBuffer = hUseNormalScreenBuffer stdout

-- Introduce a hyperlink with (key, value) parameters. Some terminals support
-- an @id@ parameter key, so that hyperlinks with the same @id@ value are
-- treated as connected.
--
-- @since 0.11.3
hHyperlinkWithParams::
     Handle
  -> [(String, String)]  -- Parameters
  -> String              -- URI
  -> String              -- Link text
  -> IO ()
hHyperlinkWithParams h params uri link =
  hPutStr h $ hyperlinkWithParamsCode params uri link

-- | Introduce a hyperlink with (key, value) parameters. Some terminals support
-- an @id@ parameter key, so that hyperlinks with the same @id@ value are
-- treated as connected.
--
-- @since 0.11.3
hyperlinkWithParams ::
     [(String, String)]  -- ^ Parameters
  -> String              -- ^ URI
  -> String              -- ^ Link text
  -> IO ()
hyperlinkWithParams = hHyperlinkWithParams stdout

-- Introduce a hyperlink.
--
-- @since 0.11.3
hHyperlink ::
     Handle
  -> String  -- URI
  -> String  -- Link text
  -> IO ()
hHyperlink h = hHyperlinkWithParams h []

-- | Introduce a hyperlink.
--
-- @since 0.11.3
hyperlink ::
     String  -- ^ URI
  -> String  -- ^ Link text
  -> IO ()
hyperlink = hHyperlink stdout

-- Introduce a hyperlink with an identifier for the link. Some terminals
-- support an identifier, so that hyperlinks with the same identifier are
-- treated as connected.
--
-- @since 0.11.3
hHyperlinkWithId ::
     Handle
  -> String  -- Identifier for the link
  -> String  -- URI
  -> String  -- Link text
  -> IO ()
hHyperlinkWithId h linkId = hHyperlinkWithParams h [("id", linkId)]

-- | Introduce a hyperlink with an identifier for the link. Some terminals
-- support an identifier, so that hyperlinks with the same identifier are
-- treated as connected.
--
-- @since 0.11.3
hyperlinkWithId ::
     String  -- ^ Identifier for the link
  -> String  -- ^ URI
  -> String  -- ^ Link text
  -> IO ()
hyperlinkWithId = hHyperlinkWithId stdout

-- Set the terminal window title and icon name (that is, the text for the
-- window in the Start bar, or similar).
hSetTitle ::
     Handle
  -> String -- New window title and icon name
  -> IO ()
hSetTitle h title = hPutStr h $ setTitleCode title

-- | Set the terminal window title and icon name (that is, the text for the
-- window in the Start bar, or similar).
setTitle :: String -- ^ New window title and icon name
         -> IO ()
setTitle = hSetTitle stdout

-- | Use heuristics to determine whether the functions defined in this package
-- will work with a given handle.
--
-- If the handle is not writable (that is, it cannot manage output - see
-- 'hIsWritable'), then @pure False@ is returned.
--
-- For Unix-like operating systems, the current implementation checks
-- that: (1) the handle is a terminal; and (2) a @TERM@ environment variable is
-- not set to @dumb@ (which is what the GNU Emacs text editor sets for its
-- integrated terminal).
--
-- For Windows, the current implementation checks: first that (1) the handle is
-- a terminal, (2) a @TERM@ environment variable is not set to @dumb@, and (3)
-- the processing of \'ANSI\' control characters in output is enabled; and
-- second, as an alternative, whether the handle is connected to a \'mintty\'
-- terminal. (That is because the function 'hIsTerminalDevice' is used to check
-- if the handle is a terminal. However, where a non-native Windows terminal
-- (such as \'mintty\') is implemented using redirection, that function will not
-- identify a handle to the terminal as a terminal.) If it is not already
-- enabled, this function does *not* enable the processing of \'ANSI\' control
-- characters in output (see 'hNowSupportsANSI').
--
-- @since 0.6.2
hSupportsANSI :: Handle -> IO Bool
hSupportsANSI = Internal.hSupportsANSI

-- | With one exception, equivalent to 'hSupportsANSI'. The exception is that,
-- on Windows only, if a @TERM@ environment variable is not set to @dumb@ and
-- the processing of \'ANSI\' control characters in output is not enabled, this
-- function first tries to enable such processing.
--
-- @Since 1.0.1
hNowSupportsANSI :: Handle -> IO Bool
hNowSupportsANSI = Internal.hNowSupportsANSI

-- | Some terminals (e.g. Emacs) are not fully ANSI compliant but can support
-- ANSI colors. This can be used in such cases, if colors are all that is
-- needed.
--
-- @since 0.9
hSupportsANSIColor :: Handle -> IO Bool
hSupportsANSIColor h = (||) <$> hSupportsANSI h <*> isEmacsTerm
  where
    isEmacsTerm = (\env -> insideEmacs env && isDumb env) <$> getEnvironment
    insideEmacs = any (\(k, _) -> k == "INSIDE_EMACS")
    isDumb env = Just "dumb" == lookup "TERM" env

-- | Use heuristics to determine whether a given handle will support \'ANSI\'
-- control characters in output. The function is consistent with
-- 'hNowSupportsANSI'.
--
-- This function is deprecated as, from version 1.0, the package no longer
-- supports legacy versions of Windows that required emulation.
--
-- @since 0.8.1
{-# DEPRECATED hSupportsANSIWithoutEmulation "See Haddock documentation and hNowSupportsANSI." #-}
hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)
hSupportsANSIWithoutEmulation h = Just <$> hNowSupportsANSI h

-- | Parses the characters emitted by 'reportCursorPosition' into the console
-- input stream. Returns the cursor row and column as a tuple.
--
-- For example, if the characters emitted by 'reportCursorPosition' are in
-- 'String' @input@ then the parser could be applied like this:
--
-- > let result = readP_to_S cursorPosition input
-- > case result of
-- >     [] -> putStrLn $ "Error: could not parse " ++ show input
-- >     [((row, column), _)] -> putStrLn $ "The cursor was at row " ++ show row
-- >         ++ " and column" ++ show column ++ "."
-- >     (_:_) -> putStrLn $ "Error: parse not unique"
--
-- @since 0.7.1
cursorPosition :: ReadP (Int, Int)
cursorPosition = do
  void $ char '\ESC'
  void $ char '['
  row <- decimal -- A non-negative whole decimal number
  void $ char ';'
  col <- decimal -- A non-negative whole decimal number
  void $ char 'R'
  pure (read row, read col)
 where
  digit = satisfy isDigit
  decimal = many1 digit

-- | Attempts to get the reported cursor position data from the console input
-- stream. The function is intended to be called immediately after
-- 'reportCursorPosition' (or related functions) have caused characters to be
-- emitted into the stream.
--
-- For example, on a Unix-like operating system:
--
-- > -- set no buffering (if 'no buffering' is not already set, the contents of
-- > -- the buffer will be discarded, so this needs to be done before the cursor
-- > -- positon is emitted)
-- > hSetBuffering stdin NoBuffering
-- > -- ensure that echoing is off
-- > input <- bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
-- >   hSetEcho stdin False
-- >   reportCursorPosition
-- >   hFlush stdout -- ensure the report cursor position code is sent to the
-- >                 -- operating system
-- >   getReportedCursorPosition
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Windows' Console API. (Command Prompt
-- and PowerShell are based on the Console API.)
--
-- @since 0.7.1
getReportedCursorPosition :: IO String
getReportedCursorPosition = Internal.getReportedCursorPosition

-- | Attempts to get the reported cursor position, combining the functions
-- 'reportCursorPosition', 'getReportedCursorPosition' and 'cursorPosition'. Any
-- position @(row, column)@ is translated to be 0-based (that is, the top-left
-- corner is at @(0, 0)@), consistent with `setCursorColumn` and
-- `setCursorPosition`. (Note that the information emitted into the console
-- input stream by 'reportCursorPosition' is 1-based.) Returns 'Nothing' if any
-- data emitted by 'reportCursorPosition', obtained by
-- 'getReportedCursorPosition', cannot be parsed by 'cursorPosition'. Uses
-- 'stdout'. If 'stdout' will be redirected, see 'hGetCursorPosition' for a more
-- general function.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Windows' Console API. (Command Prompt
-- and PowerShell are based on the Console API.)
--
-- @since 0.10.3
getCursorPosition :: IO (Maybe (Int, Int))
getCursorPosition = hGetCursorPosition stdout

-- | Attempts to get the reported cursor position, combining the functions
-- 'hReportCursorPosition' (with the specified handle),
-- 'getReportedCursorPosition' and 'cursorPosition'. Any position
-- @(row, column)@ is translated to be 0-based (that is, the top-left corner is
-- at @(0, 0)@), consistent with 'hSetCursorColumn' and 'hSetCursorPosition'.
-- (Note that the information emitted into the console input stream by
-- 'hReportCursorPosition' is 1-based.) Returns 'Nothing' if any data emitted by
-- 'hReportCursorPosition', obtained by 'getReportedCursorPosition', cannot be
-- parsed by 'cursorPosition'.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Windows' Console API. (Command Prompt
-- and PowerShell are based on the Console API.)
--
-- @since 0.10.1
hGetCursorPosition :: Handle -> IO (Maybe (Int, Int))
hGetCursorPosition h = fmap to0base <$> getCursorPosition'
 where
  to0base (row, col) = (row - 1, col - 1)
  getCursorPosition' = do
    input <- bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
      -- set no buffering (if 'no buffering' is not already set, the contents of
      -- the buffer will be discarded, so this needs to be done before the
      -- cursor positon is emitted)
      hSetBuffering stdin NoBuffering
      -- ensure that echoing is off
      bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
        hSetEcho stdin False
        clearStdin
        hReportCursorPosition h
        hFlush h -- ensure the report cursor position code is sent to the
                 -- operating system
        getReportedCursorPosition
    case readP_to_S cursorPosition input of
      [] -> pure Nothing
      [((row, col),_)] -> pure $ Just (row, col)
      (_:_) -> pure Nothing
  clearStdin = do
    isReady <- hReady stdin
    when isReady $ do
      _ <-getChar
      clearStdin

-- | Looking for a way to get layer colors? See 'getLayerColor'.
--
-- Emit the layerColor into the console input stream, immediately after
-- being recognised on the output stream, as:
-- @ESC ] \<Ps> ; rgb: \<red> ; \<green> ; \<blue> \<ST>@
-- where @\<Ps>@ is @10@ for 'Foreground' and @11@ for 'Background'; @\<red>@,
-- @\<green>@ and @\<blue>@ are the color channel values in hexadecimal (4, 8,
-- 12 and 16 bit values are possible, although 16 bit values are most common);
-- and @\<ST>@ is the STRING TERMINATOR (ST). ST depends on the terminal
-- software and may be the @BEL@ character or @ESC \\@ characters.
--
-- This function may be of limited, or no, use on Windows operating systems
-- because (1) the function is not supported on native terminals and is
-- emulated, but the emulation does not work on Windows Terminal and (2) of
-- difficulties in obtaining the data emitted into the console input stream.
--
-- @since 0.11.4
reportLayerColor :: ConsoleLayer -> IO ()
reportLayerColor = hReportLayerColor stdout

-- @since 0.11.4
hReportLayerColor :: Handle -> ConsoleLayer -> IO ()
hReportLayerColor h layer = hPutStr h $ reportLayerColorCode layer

-- | Attempts to get the reported layer color data from the console input
-- stream. The function is intended to be called immediately after
-- 'reportLayerColor' (or related functions) have caused characters to be
-- emitted into the stream.
--
-- For example, on a Unix-like operating system:
--
-- > -- set no buffering (if 'no buffering' is not already set, the contents of
-- > -- the buffer will be discarded, so this needs to be done before the cursor
-- > -- positon is emitted)
-- > hSetBuffering stdin NoBuffering
-- > -- ensure that echoing is off
-- > input <- bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
-- >   hSetEcho stdin False
-- >   reportLayerColor Foreground
-- >   hFlush stdout -- ensure the report cursor position code is sent to the
-- >                 -- operating system
-- >   getReportedLayerColor Foreground
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Windows' Console API. (Command Prompt
-- and PowerShell are based on the Console API.)
--
-- @since 0.11.4
getReportedLayerColor :: ConsoleLayer -> IO String
getReportedLayerColor = Internal.getReportedLayerColor

-- | Attempts to get the reported layer color, combining the functions
-- 'reportLayerColor', 'getReportedLayerColor' and 'layerColor'. Any RGB color
-- is scaled to be 16 bits per channel, the most common format reported by
-- terminal software. Returns 'Nothing' if any data emitted by
-- 'reportLayerColor', obtained by 'getReportedLayerColor', cannot be parsed by
-- 'layerColor'. Uses 'stdout'. If 'stdout' will be redirected, see
-- 'hGetLayerColor' for a more general function.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Windows' Console API. (Command Prompt
-- and PowerShell are based on the Console API.) This function also relies on
-- emulation that does not work on Windows Terminal.
--
-- @since 0.11.4
getLayerColor :: ConsoleLayer -> IO (Maybe(RGB Word16))
getLayerColor = hGetLayerColor stdout

-- | Attempts to get the reported layer color, combining the functions
-- 'hReportLayerColor', 'getReportedLayerColor' and 'layerColor'. Any RGB color
-- is scaled to be 16 bits per channel, the most common format reported by
-- terminal software. Returns 'Nothing' if any data emitted by
-- 'hReportLayerColor', obtained by 'getReportedLayerColor', cannot be parsed by
-- 'layerColor'.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Windows' Console API. (Command Prompt
-- and PowerShell are based on the Console API.) This function also relies on
-- emulation that does not work on Windows Terminal.
--
-- @since 0.11.4
hGetLayerColor :: Handle -> ConsoleLayer -> IO (Maybe (RGB Word16))
hGetLayerColor h layer = do
  input <- bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
    -- set no buffering (if 'no buffering' is not already set, the contents of
    -- the buffer will be discarded, so this needs to be done before the
    -- cursor positon is emitted)
    hSetBuffering stdin NoBuffering
    -- ensure that echoing is off
    bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
      hSetEcho stdin False
      clearStdin
      hReportLayerColor h layer
      hFlush h -- ensure the report cursor position code is sent to the
               -- operating system
      getReportedLayerColor layer
  case readP_to_S (layerColor layer) input of
      [] -> pure Nothing
      [(col, _)] -> pure $ Just col
      (_:_) -> pure Nothing
 where
  clearStdin = do
    isReady <- hReady stdin
    when isReady $ do
      _ <-getChar
      clearStdin

-- | Parses the characters emitted by 'reportLayerColor' into the console input
-- stream.
--
-- For example, if the characters emitted by 'reportLayerColor' are in 'String'
-- @input@ then the parser could be applied like this:
--
-- > let result = readP_to_S (layerColor layer) input
-- > case result of
-- >     [] -> putStrLn $ "Error: could not parse " ++ show input
-- >     [(col, _)] -> putStrLn $ "The color was " ++ show col ++ "."
-- >     (_:_) -> putStrLn $ "Error: parse not unique"
--
-- @since 0.11.4
layerColor :: ConsoleLayer -> ReadP (RGB Word16)
layerColor layer = do
  void $ string "\ESC]"
  void $ string $ case layer of
    Foreground -> "10"
    Background -> "11"
  void $ string ";rgb:"
  redHex <- hexadecimal -- A non-negative whole hexadecimal number
  void $ char '/'
  greenHex <- hexadecimal -- A non-negative whole hexadecimal number
  void $ char '/'
  blueHex <- hexadecimal -- A non-negative whole hexadecimal number
  void $ string "\BEL" <++ string "\ESC\\"
  let lenRed = length redHex
      lenGreen = length greenHex
      lenBlue = length blueHex
  if lenRed == lenGreen && lenGreen == lenBlue
    then
      if lenRed == 0 || lenRed > 4
        then fail "Color format not recognised"
        else
          let m = 16 ^ (4 - lenRed)
              r = fromIntegral $ m * hexToInt redHex
              g = fromIntegral $ m * hexToInt greenHex
              b = fromIntegral $ m * hexToInt blueHex
          in  pure $ RGB r g b
    else fail "Color format not recognised"
 where
  hexDigit = satisfy isHexDigit
  hexadecimal = many1 hexDigit
  hexToInt hex = foldl (\d a -> d * 16 + a) 0 (map digitToInt hex)

-- | Attempts to get the current terminal size (height in rows, width in
-- columns).
--
-- There is no \'ANSI\' control character sequence that reports the terminal
-- size. So, it attempts to set the cursor position beyond the bottom right
-- corner of the terminal and then use 'getCursorPosition' to query the console
-- input stream. It works only on terminals that support each step and if data
-- can be emitted to 'stdin'. (Use 'System.IO.hIsTerminalDevice' to test if
-- 'stdin' is connected to a terminal.) Uses 'stdout'. If 'stdout' will be
-- redirected, see 'System.IO.hGetTerminalSize' for a more general function.
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on Windows' Console API. (Command Prompt and
-- PowerShell are based on the Console API.)
--
-- For a different approach, one that does not use control character sequences
-- and works when 'stdin' is redirected, see the
-- <https://hackage.haskell.org/package/terminal-size terminal-size> package.
--
-- @since 0.9
getTerminalSize :: IO (Maybe (Int, Int))
getTerminalSize = hGetTerminalSize stdout

-- | Attempts to get the current terminal size (height in rows, width in
-- columns), by writing control character sequences to the specified handle
-- (which will typically be 'stdout' or 'stderr').
--
-- There is no \'ANSI\' control character sequence that reports the terminal
-- size. So, it attempts to set the cursor position beyond the bottom right
-- corner of the terminal and then use 'hGetCursorPosition' to query the console
-- input stream. It works only on terminals that support each step and if data
-- can be emitted to 'stdin'. (Use 'System.IO.hIsTerminalDevice' to test if
-- 'stdin' is connected to a terminal.)
--
-- On Windows operating systems, the function is not supported on consoles, such
-- as mintty, that are not based on the Windows' Console API. (Command Prompt
-- and PowerShell are based on the Console API.)
--
-- For a different approach, one that does not use control character sequences
-- and works when 'stdin' is redirected, see the
-- <https://hackage.haskell.org/package/terminal-size terminal-size> package.
--
-- @since 0.10.1
hGetTerminalSize :: Handle -> IO (Maybe (Int, Int))
hGetTerminalSize h = do
  hSaveCursor h
  hSetCursorPosition h 9999 9999  -- Attempt to set the cursor position beyond
                                  -- the bottom right corner of the terminal.
  mPos <- hGetCursorPosition h
  hRestoreCursor h
  hFlush h -- ensure the restore cursor position code is sent to the
           -- operating system
  pure $ fmap (\(r, c) -> (r + 1, c + 1)) mPos

-- Set the Select Graphic Rendition mode
hSetSGR ::
     Handle
  -> [SGR] -- Commands: these will typically be applied on top of the
           -- current console SGR mode. An empty list of commands is
           -- equivalent to the list @[Reset]@. Commands are applied left to
           -- right.
  -> IO ()
hSetSGR h sgrs = hPutStr h $ setSGRCode sgrs

-- | Set the Select Graphic Rendition mode
setSGR ::
     [SGR] -- ^ Commands: these will typically be applied on top of the
           -- current console SGR mode. An empty list of commands is
           -- equivalent to the list @[Reset]@. Commands are applied left to
           -- right.
  -> IO ()
setSGR = hSetSGR stdout

hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen ::
     Handle
  -> IO ()
hClearFromCursorToScreenEnd h = hPutStr h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h
    = hPutStr h clearFromCursorToScreenBeginningCode
hClearScreen h = hPutStr h clearScreenCode

clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen :: IO ()
clearFromCursorToScreenEnd = hClearFromCursorToScreenEnd stdout
clearFromCursorToScreenBeginning = hClearFromCursorToScreenBeginning stdout
clearScreen = hClearScreen stdout

hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine ::
     Handle
  -> IO ()
hClearFromCursorToLineEnd h = hPutStr h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = hPutStr h clearFromCursorToLineBeginningCode
hClearLine h = hPutStr h clearLineCode

clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine :: IO ()
clearFromCursorToLineEnd = hClearFromCursorToLineEnd stdout
clearFromCursorToLineBeginning = hClearFromCursorToLineBeginning stdout
clearLine = hClearLine stdout

hScrollPageUp, hScrollPageDown ::
     Handle
  -> Int -- Number of lines to scroll by
  -> IO ()
hScrollPageUp h n = hPutStr h $ scrollPageUpCode n
hScrollPageDown h n = hPutStr h $ scrollPageDownCode n

scrollPageUp, scrollPageDown ::
     Int -- ^ Number of lines to scroll by
  -> IO ()
scrollPageUp = hScrollPageUp stdout
scrollPageDown = hScrollPageDown stdout
