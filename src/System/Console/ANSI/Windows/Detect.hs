{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Windows.Detect
  (
    ANSISupport (..)
  , ConsoleDefaultState (..)
  , aNSISupport
  ) where

import Control.Exception (SomeException(..), throwIO, try)
import Data.Bits ((.&.), (.|.))
import System.IO (stdout)
import System.IO.Unsafe (unsafePerformIO)

import System.Console.ANSI.Windows.Foreign (ConsoleException(..),
  CONSOLE_SCREEN_BUFFER_INFO (..), DWORD, HANDLE, WORD,
  bACKGROUND_INTENSE_WHITE, eNABLE_VIRTUAL_TERMINAL_PROCESSING,
  fOREGROUND_INTENSE_WHITE, getConsoleMode, getConsoleScreenBufferInfo,
  iNVALID_HANDLE_VALUE, nullHANDLE, setConsoleMode, withHandleToHANDLE)

-- | The default state of the console.
data ConsoleDefaultState = ConsoleDefaultState
  { defaultForegroundAttributes :: WORD -- ^ Foreground attributes
  , defaultBackgroundAttributes :: WORD -- ^ Background attributes
  } deriving (Eq, Show)

-- | How the console is assumed to support ANSI control codes.
data ANSISupport
  = Native                       -- ^ Assume ANSI-enabled
  | Emulated ConsoleDefaultState -- ^ Not ANSI-enabled (including the state of
                                 -- the console when that status was determined)
  deriving (Eq, Show)

-- | This function assumes that once it is first established whether or not the
-- Windows console requires emulation, that will not change. If the console
-- requires emulation, the state of the console is considered to be its default
-- state.
{-# NOINLINE aNSISupport #-}
aNSISupport :: ANSISupport
aNSISupport = unsafePerformIO $ withHandleToHANDLE stdout aNSISupport'

-- | This function first checks if the Windows handle is valid and throws an
-- exception if it is not. It then tries to get a ConHost console mode for
-- that handle. If it can not, it assumes that the handle is ANSI-enabled. If
-- virtual termimal (VT) processing is already enabled, the handle does not
-- require emulation. Otherwise, it trys to enable processing. If it can, the
-- handle is ANSI-enabled. If it can not, emulation will be attempted and the
-- state of the console is considered to be its default state.
aNSISupport' :: HANDLE -> IO ANSISupport
aNSISupport' h =
  if h == iNVALID_HANDLE_VALUE || h == nullHANDLE
    then throwIO $ ConsoleException 6  -- Invalid handle or no handle
    else do
      tryMode <- try (getConsoleMode h) :: IO (Either SomeException DWORD)
      case tryMode of
        Left _     -> return Native  -- No ConHost mode
        Right mode -> if mode .&. eNABLE_VIRTUAL_TERMINAL_PROCESSING /= 0
          then return Native  -- VT processing already enabled
          else do
            let mode' = mode .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING
            trySetMode <- try (setConsoleMode h mode') :: IO (Either SomeException ())
            case trySetMode of
              Left _   -> emulated       -- Can't enable VT processing
              Right () -> return Native  -- VT processing enabled
 where
  emulated = do
    info <- getConsoleScreenBufferInfo h
    let attributes = csbi_attributes info
        fgAttributes = attributes .&. fOREGROUND_INTENSE_WHITE
        bgAttributes = attributes .&. bACKGROUND_INTENSE_WHITE
        consoleDefaultState = ConsoleDefaultState
          { defaultForegroundAttributes = fgAttributes
          , defaultBackgroundAttributes = bgAttributes }
    return $ Emulated consoleDefaultState
