{-# LANGUAGE Safe #-}

module System.Console.ANSI.Internal
  ( getReportedCursorPosition
  , getReportedLayerColor
  , hSupportsANSI
  ) where

import Control.Exception ( IOException, catch )
import Data.Maybe ( mapMaybe )
import System.Environment ( lookupEnv )
import System.IO ( Handle, hIsTerminalDevice, hIsWritable, stdin )
import System.Win32.MinTTY (isMinTTYHandle)

import System.Console.ANSI.Types ( ConsoleLayer )
import System.Console.ANSI.Windows.Foreign
         ( INPUT_RECORD (..), INPUT_RECORD_EVENT (..), KEY_EVENT_RECORD (..)
         , cWcharsToChars, getNumberOfConsoleInputEvents, readConsoleInput
         , unicodeAsciiChar, withHandleToHANDLE
         )

getReportedCursorPosition :: IO String
getReportedCursorPosition = getReported

getReportedLayerColor :: ConsoleLayer -> IO String
getReportedLayerColor _ = getReported

getReported :: IO String
getReported = catch getReported' getReportedExceptionHandler
 where
  getReported' = withHandleToHANDLE stdin action
   where
    action hdl = do
      n <- getNumberOfConsoleInputEvents hdl
      if n == 0
        then pure ""
        else do
          es <- readConsoleInput hdl n
          pure $ stringFromInputEvents es
    stringFromInputEvents = cWcharsToChars . wCharsFromInputEvents
    wCharsFromInputEvents = mapMaybe wCharFromInputEvent
    wCharFromInputEvent e = if isKeyEvent && isKeyDown
      then Just (unicodeAsciiChar $ keyEventChar keyEventRecord)
      else Nothing
     where
      eventType = inputEventType e
      eventRecord = inputEvent e
      isKeyEvent = eventType == 1
      keyEventRecord = case eventRecord of
        InputKeyEvent keyEventRecord' -> keyEventRecord'
        _ -> error "Unexpected input event, given input event type."
      isKeyDown = keyEventKeyDown keyEventRecord

getReportedExceptionHandler :: IOException -> IO a
getReportedExceptionHandler e = error msg
 where
  msg = "Error: " ++ show e ++ "\nThis error may be avoided by using a " ++
        "console based on the Windows' Console API, such as Command Prompt " ++
        "or PowerShell."

hSupportsANSI :: Handle -> IO Bool
hSupportsANSI h = (&&) <$> hIsWritable h <*> hSupportsANSI'
 where
  hSupportsANSI' = (||) <$> isTDNotDumb <*> isMinTTY
  -- Borrowed from an HSpec patch by Simon Hengel
  -- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
  isTDNotDumb = (&&) <$> hIsTerminalDevice h <*> isNotDumb
  isNotDumb = (/= Just "dumb") <$> lookupEnv "TERM"
  isMinTTY = withHandleToHANDLE h isMinTTYHandle
