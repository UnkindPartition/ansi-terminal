#include "Common-Safe-Haskell.hs"
{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Unix
  (
-- This file contains code that is common to modules
-- System.Console.ANSI.Unix and System.Console.ANSI.Windows, namely the module
-- exports and the associated Haddock documentation.
#include "Exports-Include.hs"
  ) where

import Control.Exception.Base (bracket)
import Control.Monad (when)
import Data.List (uncons)
import Data.Maybe (fromMaybe, mapMaybe)
import System.IO (BufferMode (..), Handle, hGetBuffering, hGetEcho,
  hIsTerminalDevice, hIsWritable, hPutStr, hReady, hSetBuffering, hSetEcho,
  stdin)
import System.Timeout (timeout)
import Text.ParserCombinators.ReadP (readP_to_S)

import System.Console.ANSI.Codes

-- This file contains code that is common to modules System.Console.ANSI.Unix,
-- System.Console.ANSI.Windows and System.Console.ANSI.Windows.Emulator, such as
-- type signatures and the definition of functions specific to stdout in terms
-- of the corresponding more general functions, including the related Haddock
-- documentation.
#include "Common-Include.hs"
-- This file contains code that is common save that different code is required
-- in the case of the module System.Console.ANSI.Windows.Emulator (see the file
-- Common-Include-Emulator.hs in respect of the latter).
#include "Common-Include-Enabled.hs"

hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n

hCursorDownLine h n = hPutStr h $ cursorDownLineCode n
hCursorUpLine h n = hPutStr h $ cursorUpLineCode n

hSetCursorColumn h n = hPutStr h $ setCursorColumnCode n
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m

hSaveCursor h = hPutStr h saveCursorCode
hRestoreCursor h = hPutStr h restoreCursorCode
hReportCursorPosition h = hPutStr h reportCursorPositionCode

hClearFromCursorToScreenEnd h = hPutStr h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h
    = hPutStr h clearFromCursorToScreenBeginningCode
hClearScreen h = hPutStr h clearScreenCode

hClearFromCursorToLineEnd h = hPutStr h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = hPutStr h clearFromCursorToLineBeginningCode
hClearLine h = hPutStr h clearLineCode

hScrollPageUp h n = hPutStr h $ scrollPageUpCode n
hScrollPageDown h n = hPutStr h $ scrollPageDownCode n

hUseAlternateScreenBuffer h = hPutStr h useAlternateScreenBufferCode
hUseNormalScreenBuffer h = hPutStr h useNormalScreenBufferCode

hReportLayerColor h layer = hPutStr h $ reportLayerColorCode layer

hSetSGR h sgrs = hPutStr h $ setSGRCode sgrs

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode

hHyperlinkWithParams h params uri link =
  hPutStr h $ hyperlinkWithParamsCode params uri link

hSetTitle h title = hPutStr h $ setTitleCode title

-- hSupportsANSI :: Handle -> IO Bool
-- (See Common-Include.hs for Haddock documentation)
--
-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> isNotDumb
 where
  -- cannot use lookupEnv since it only appeared in GHC 7.6
  isNotDumb = (/= Just "dumb") . lookup "TERM" <$> getEnvironment

-- hSupportsANSIWithoutEmulation :: Handle -> IO (Maybe Bool)
-- (See Common-Include.hs for Haddock documentation)
hSupportsANSIWithoutEmulation h =
  Just <$> ((&&) <$> hIsWritable h <*> hSupportsANSI h)

-- getReportedCursorPosition :: IO String
-- (See Common-Include.hs for Haddock documentation)
getReportedCursorPosition = getReport "\ESC[" ["R"]

-- getReportedLayerColor :: ConsoleLayer -> IO String
-- (See Common-Include.hs for Haddock documentation)
getReportedLayerColor layer =
  getReport ("\ESC]" ++ pS ++ ";rgb:") ["\BEL", "\ESC\\"]
 where
   pS = case layer of
          Foreground -> "10"
          Background -> "11"

getReport :: String -> [String] -> IO String
getReport _ [] = error "getReport requires a list of terminating sequences."
getReport startChars endChars = do
  -- If, unexpectedly, no data is available on the console input stream then
  -- the timeout will prevent the getChar blocking. For consistency with the
  -- Windows equivalent, returns "" if the expected information is unavailable.
  fromMaybe "" <$> timeout 500000 (getStart startChars "") -- 500 milliseconds
 where
  endChars' = mapMaybe uncons endChars

  -- The list is built in reverse order, in order to avoid O(n^2) complexity.
  -- So, getReport yields the reversed built list.

  getStart :: String -> String -> IO String
  getStart "" r = getRest r
  getStart (h:hs) r = do
    c <- getChar
    if c == h
      then getStart hs (c:r) -- Try to get the rest of the start characters
      else pure $ reverse (c:r) -- If the first character(s) are not the
                                  -- expected start then give up. This provides
                                  -- a modicom of protection against unexpected
                                  -- data in the input stream.
  getRest :: String -> IO String
  getRest r = do
    c <- getChar
    case lookup c endChars' of
      Nothing -> getRest (c:r) -- Continue building the list, until the first of
                               -- the end characters is obtained.
      Just es -> getEnd es (c:r) -- Try to get the rest of the end characters.

  getEnd :: String -> String -> IO String
  getEnd "" r = pure $ reverse r
  getEnd (e:es) r = do
    c <- getChar
    if c /= e
      then getRest (c:r) -- Continue building the list, with the original end
                         -- characters.
      else getEnd es (c:r) -- Continue building the list, checking against the
                           -- remaining end characters.

-- hGetCursorPosition :: Handle -> IO (Maybe (Int, Int))
-- (See Common-Include.hs for Haddock documentation)
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

-- hGetLayerColor :: Handle -> IO (Maybe (Colour Word16))
-- (See Common-Include.hs for Haddock documentation)
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
