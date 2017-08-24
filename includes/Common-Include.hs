import System.Environment
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

hCursorUp, hCursorDown, hCursorForward, hCursorBackward :: Handle
                                                        -> Int -- ^ Number of lines or characters to move
                                                        -> IO ()
cursorUp, cursorDown, cursorForward, cursorBackward :: Int -- ^ Number of lines or characters to move
                                                    -> IO ()
cursorUp = hCursorUp stdout
cursorDown = hCursorDown stdout
cursorForward = hCursorForward stdout
cursorBackward = hCursorBackward stdout

hCursorDownLine, hCursorUpLine :: Handle
                               -> Int -- ^ Number of lines to move
                               -> IO ()
cursorDownLine, cursorUpLine :: Int -- ^ Number of lines to move
                             -> IO ()
cursorDownLine = hCursorDownLine stdout
cursorUpLine = hCursorUpLine stdout

hSetCursorColumn :: Handle
                 -> Int -- ^ 0-based column to move to
                 -> IO ()
setCursorColumn :: Int -- ^ 0-based column to move to
                -> IO ()
setCursorColumn = hSetCursorColumn stdout

hSetCursorPosition :: Handle
                   -> Int -- ^ 0-based row to move to
                   -> Int -- ^ 0-based column to move to
                   -> IO ()
setCursorPosition :: Int -- ^ 0-based row to move to
                  -> Int -- ^ 0-based column to move to
                  -> IO ()
setCursorPosition = hSetCursorPosition stdout

hHideCursor, hShowCursor :: Handle
                         -> IO ()
hideCursor, showCursor :: IO ()
hideCursor = hHideCursor stdout
showCursor = hShowCursor stdout

-- | Set the terminal window title
hSetTitle :: Handle
          -> String -- ^ New title
          -> IO ()
-- | Set the terminal window title
setTitle :: String -- ^ New title
         -> IO ()
setTitle = hSetTitle stdout

-- | Use heuristics to determine whether the functions defined in this
-- package will work with a given handle.
--
-- The current implementation checks that the handle is a terminal, and
-- that the @TERM@ environment variable doesn't say @dumb@ (which is what
-- Emacs sets for its own terminal).
hSupportsANSI :: Handle -> IO Bool
-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> (not <$> isDumb)
  where
    -- cannot use lookupEnv since it only appeared in GHC 7.6
    isDumb = maybe False (== "dumb") . lookup "TERM" <$> getEnvironment
