{-# LANGUAGE Safe #-}

module System.Console.ANSI.Internal
  ( getReportedCursorPosition
  , getReportedLayerColor
  , hNowSupportsANSI
  , hSupportsANSI
  ) where

import Control.Exception ( IOException, SomeException, catch, try )
import Data.Bits ( (.&.), (.|.) )
import Data.Maybe ( mapMaybe )
import System.Environment ( lookupEnv )
import System.IO ( Handle, hIsTerminalDevice, hIsWritable, stdin )
import System.Console.ANSI.Types ( ConsoleLayer )

-- Provided by the ansi-terminal package
import System.Console.ANSI.Windows.Foreign
         ( INPUT_RECORD (..), INPUT_RECORD_EVENT (..), KEY_EVENT_RECORD (..)
         , cWcharsToChars, eNABLE_VIRTUAL_TERMINAL_PROCESSING
         , getConsoleMode, getNumberOfConsoleInputEvents, iNVALID_HANDLE_VALUE
         , nullHANDLE, readConsoleInput, setConsoleMode, unicodeAsciiChar
         )
import System.Console.ANSI.Windows.Win32.MinTTY ( isMinTTYHandle )
import System.Console.ANSI.Windows.Win32.Types
         ( DWORD, HANDLE, withHandleToHANDLE )

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
hSupportsANSI = hSupportsANSI' False

hNowSupportsANSI :: Handle -> IO Bool
hNowSupportsANSI = hSupportsANSI' True

hSupportsANSI' :: Bool -> Handle -> IO Bool
hSupportsANSI' tryToEnable handle = do
  isWritable <- hIsWritable handle
  if isWritable
    then withHandleToHANDLE handle $ withHANDLE
      (pure False) -- Invalid handle or no handle
      ( \h -> do
          tryMode <- try (getConsoleMode h) :: IO (Either SomeException DWORD)
          case tryMode of
            Left _ -> isMinTTYHandle h -- No ConHost mode
            Right mode -> do
              let isVTEnabled = mode .&. eNABLE_VIRTUAL_TERMINAL_PROCESSING /= 0
                  isNotDumb = (/= Just "dumb") <$> lookupEnv "TERM"
              isTDNotDumb <- (&&) <$> hIsTerminalDevice handle <*> isNotDumb
              if isTDNotDumb && not isVTEnabled && tryToEnable
                then do
                  let mode' = mode .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING
                  trySetMode <- try (setConsoleMode h mode')
                    :: IO (Either SomeException ())
                  case trySetMode of
                    Left _ -> pure False -- Can't enable VT processing
                    Right () -> pure True -- VT processing enabled
                else pure $ isTDNotDumb && isVTEnabled
      )
    else pure False

-- | This function applies another to the Windows handle, if the handle is
-- valid. If it is invalid, the specified default action is returned.
withHANDLE :: IO a -> (HANDLE -> IO a) -> HANDLE -> IO a
withHANDLE invalid action h =
  if h == iNVALID_HANDLE_VALUE || h == nullHANDLE
    then invalid  -- Invalid handle or no handle
    else action h
