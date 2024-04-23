{-# LANGUAGE Trustworthy #-}

module System.Console.ANSI.Internal
  ( getReportedCursorPosition
  , getReportedLayerColor
  , hSupportsANSI
  , hNowSupportsANSI
  , hSupportsANSIColor
  ) where

import Data.List ( uncons )
import Data.Maybe ( fromMaybe, isJust, mapMaybe )
import System.Environment ( lookupEnv )
import System.IO ( Handle, hIsTerminalDevice, hIsWritable )
import System.IO.Unsafe ( unsafePerformIO )
import System.Timeout ( timeout )

import System.Console.ANSI.Types ( ConsoleLayer (..) )

getReportedCursorPosition :: IO String
getReportedCursorPosition = getReport "\ESC[" ["R"]

getReportedLayerColor :: ConsoleLayer -> IO String
getReportedLayerColor Underlining =
  error "getReportedLayerColor does not support underlining."
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

hSupportsANSI :: Handle -> IO Bool
-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
hSupportsANSI h = (&&) <$> hIsWritable h <*> hSupportsANSI'
 where
  hSupportsANSI' = (&& isNotDumb) <$> hIsTerminalDevice h

hNowSupportsANSI :: Handle -> IO Bool
hNowSupportsANSI = hSupportsANSI

hSupportsANSIColor :: Handle -> IO Bool
hSupportsANSIColor h = (|| isEmacsTerm) <$> hSupportsANSI h
 where
  isEmacsTerm = insideEmacs && isDumb
  isDumb = not isNotDumb

-- | This function assumes that once it is first established whether or not the
-- TERM environment variable exists with contents dumb, that will not change.
-- This approach is taken because the use of C function setenv() in one thread
-- can cause other threads calling C function getenv() to crash. On Unix-like
-- operating systems, System.Environment.lookupEnv is implemented using C
-- function getenv(). 
isNotDumb :: Bool
isNotDumb = unsafePerformIO (lookupEnv "TERM") /= Just "dumb"

{-# NOINLINE isNotDumb #-}

-- | This function assumes that once it is first established whether or not the
-- INSIDE_EMACS environment variable exists, that will not change. This approach
-- is taken because the use of C function setenv() in one thread can cause other
-- threads calling C function getenv() to crash. On Unix-like operating systems,
-- System.Environment.lookupEnv is implemented using C function getenv(). 
insideEmacs :: Bool
insideEmacs = isJust $ unsafePerformIO (lookupEnv "INSIDE_EMACS")

{-# NOINLINE insideEmacs #-}
