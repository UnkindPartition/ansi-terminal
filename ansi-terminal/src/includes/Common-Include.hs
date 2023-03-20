-- This file contains code that is common to modules System.Console.ANSI.Unix,
-- System.Console.ANSI.Windows and System.Console.ANSI.Windows.Emulator, such as
-- type signatures and the definition of functions specific to stdout in terms
-- of the corresponding more general functions, including the related Haddock
-- documentation.

import Control.Monad (void)
import Data.Char (digitToInt, isDigit, isHexDigit)
import Data.Word (Word16)
import System.Environment (getEnvironment)
import System.IO (hFlush, stdout)
import Text.ParserCombinators.ReadP (ReadP, (<++), char, many1, satisfy, string)

import Data.Colour.SRGB (RGB (..))

import System.Console.ANSI.Types

hCursorUp, hCursorDown, hCursorForward, hCursorBackward
  :: Handle
  -> Int -- Number of lines or characters to move
  -> IO ()
cursorUp, cursorDown, cursorForward, cursorBackward
  :: Int -- ^ Number of lines or characters to move
  -> IO ()
cursorUp = hCursorUp stdout
cursorDown = hCursorDown stdout
cursorForward = hCursorForward stdout
cursorBackward = hCursorBackward stdout

hCursorDownLine, hCursorUpLine :: Handle
                               -> Int -- Number of lines to move
                               -> IO ()
cursorDownLine, cursorUpLine :: Int -- ^ Number of lines to move
                             -> IO ()
cursorDownLine = hCursorDownLine stdout
cursorUpLine = hCursorUpLine stdout

hSetCursorColumn :: Handle
                 -> Int -- 0-based column to move to
                 -> IO ()

-- | Move the cursor to the specified column. The column numbering is 0-based
-- (that is, the left-most column is numbered 0).
setCursorColumn :: Int -- ^ 0-based column to move to
                -> IO ()
setCursorColumn = hSetCursorColumn stdout

hSetCursorPosition :: Handle
                   -> Int -- 0-based row to move to
                   -> Int -- 0-based column to move to
                   -> IO ()

-- | Move the cursor to the specified position (row and column). The position is
-- 0-based (that is, the top-left corner is at row 0 column 0).
setCursorPosition :: Int -- ^ 0-based row to move to
                  -> Int -- ^ 0-based column to move to
                  -> IO ()
setCursorPosition = hSetCursorPosition stdout

hSaveCursor, hRestoreCursor, hReportCursorPosition :: Handle -> IO ()

-- | Save the cursor position in memory. The only way to access the saved value
-- is with the 'restoreCursor' command.
--
-- @since 0.7.1
saveCursor :: IO ()

-- | Restore the cursor position from memory. There will be no value saved in
-- memory until the first use of the 'saveCursor' command.
--
-- @since 0.7.1
restoreCursor :: IO ()

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

saveCursor = hSaveCursor stdout
restoreCursor = hRestoreCursor stdout
reportCursorPosition = hReportCursorPosition stdout

hHideCursor, hShowCursor :: Handle
                         -> IO ()
hideCursor, showCursor :: IO ()
hideCursor = hHideCursor stdout
showCursor = hShowCursor stdout

hUseAlternateScreenBuffer
  :: Handle
  -> IO ()

hUseNormalScreenBuffer
  :: Handle
  -> IO ()

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
useAlternateScreenBuffer
  :: IO ()
useAlternateScreenBuffer = hUseAlternateScreenBuffer stdout

-- | Use the Normal Screen Buffer. If currently using the Alternate Screen
-- Buffer, it will clear the Alternate Screen Buffer, and switch to the Normal
-- Screen Buffer. It will always restore the saved cursor position.
--
-- @since 0.11.4
useNormalScreenBuffer
  :: IO ()
useNormalScreenBuffer = hUseNormalScreenBuffer stdout

-- Introduce a hyperlink with (key, value) parameters. Some terminals support
-- an @id@ parameter key, so that hyperlinks with the same @id@ value are
-- treated as connected.
--
-- @since 0.11.3
hHyperlinkWithParams
 :: Handle
 -> [(String, String)]  -- Parameters
 -> String              -- URI
 -> String              -- Link text
 -> IO ()

-- | Introduce a hyperlink with (key, value) parameters. Some terminals support
-- an @id@ parameter key, so that hyperlinks with the same @id@ value are
-- treated as connected.
--
-- @since 0.11.3
hyperlinkWithParams
 :: [(String, String)]  -- ^ Parameters
 -> String              -- ^ URI
 -> String              -- ^ Link text
 -> IO ()
hyperlinkWithParams = hHyperlinkWithParams stdout

-- Introduce a hyperlink.
--
-- @since 0.11.3
hHyperlink
 :: Handle
 -> String  -- URI
 -> String  -- Link text
 -> IO ()
hHyperlink h = hHyperlinkWithParams h []

-- | Introduce a hyperlink.
--
-- @since 0.11.3
hyperlink
 :: String  -- ^ URI
 -> String  -- ^ Link text
 -> IO ()
hyperlink = hHyperlink stdout

-- Introduce a hyperlink with an identifier for the link. Some terminals
-- support an identifier, so that hyperlinks with the same identifier are
-- treated as connected.
--
-- @since 0.11.3
hHyperlinkWithId
 :: Handle
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
hyperlinkWithId
 :: String  -- ^ Identifier for the link
 -> String  -- ^ URI
 -> String  -- ^ Link text
 -> IO ()
hyperlinkWithId = hHyperlinkWithId stdout

-- Set the terminal window title and icon name (that is, the text for the
-- window in the Start bar, or similar).
hSetTitle :: Handle
          -> String -- New window title and icon name
          -> IO ()
-- | Set the terminal window title and icon name (that is, the text for the
-- window in the Start bar, or similar).
setTitle :: String -- ^ New window title and icon name
         -> IO ()
setTitle = hSetTitle stdout

-- | Use heuristics to determine whether the functions defined in this
-- package will work with a given handle. This function assumes that the handle
-- is writable (that is, it manages output - see 'hIsWritable').
--
-- For Unix-like operating systems, the current implementation checks
-- that: (1) the handle is a terminal; and (2) a @TERM@
-- environment variable is not set to @dumb@ (which is what the GNU Emacs text
-- editor sets for its integrated terminal).
--
-- For Windows, the current implementation performs the same checks as for
-- Unix-like operating systems and, as an alternative, checks whether the
-- handle is connected to a \'mintty\' terminal. (That is because the function
-- 'hIsTerminalDevice' is used to check if the handle is a
-- terminal. However, where a non-native Windows terminal (such as \'mintty\')
-- is implemented using redirection, that function will not identify a
-- handle to the terminal as a terminal.) On Windows 10, if the handle is
-- identified as connected to a native terminal, this function does /not/ enable
-- the processing of \'ANSI\' control characters in output (see
-- 'hSupportsANSIWithoutEmulation').
--
-- @since 0.6.2
hSupportsANSI :: Handle -> IO Bool

-- | Some terminals (e.g. Emacs) are not fully ANSI compliant but can support
-- ANSI colors. This can be used in such cases, if colors are all that is
-- needed.
--
-- @since 0.9
hSupportsANSIColor :: Handle -> IO Bool
hSupportsANSIColor h = (||) <$> hSupportsANSI h <*> isEmacsTerm
  where
    isEmacsTerm = (\env -> (insideEmacs env) && (isDumb env)) <$> getEnvironment
    insideEmacs env = any (\(k, _) -> k == "INSIDE_EMACS") env
    isDumb env = Just "dumb" == lookup "TERM" env

-- | Use heuristics to determine whether a given handle will support \'ANSI\'
-- control characters in output. (On Windows versions before Windows 10, that
-- means \'support without emulation\'.)
--
-- If the handle is not writable (that is, it cannot manage output - see
-- 'hIsWritable'), then @pure (Just False)@ is returned.
--
-- On Unix-like operating systems, with one exception, the function is
-- consistent with 'hSupportsANSI'. The exception is if the handle is not
-- writable.
--
-- On Windows, what is returned will depend on what the handle is connected to
-- and the version of the operating system. If the handle is identified as
-- connected to a \'mintty\' terminal, @pure (Just True)@ is
-- returned. If it is identified as connected to a native terminal, then, on
-- Windows 10, the processing of \'ANSI\' control characters will be enabled and
-- @pure (Just True)@ returned; and, on versions of Windows before Windows 10,
-- @pure (Just False)@ is returned. Otherwise, if a @TERM@ environment
-- variable is set to @dumb@, @pure (Just False)@ is returned. In all other
-- cases of a writable handle, @pure Nothing@ is returned; this indicates that
-- the heuristics cannot assist - the handle may be connected to a file or
-- to another type of terminal.
--
-- @since 0.8.1
hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)

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
