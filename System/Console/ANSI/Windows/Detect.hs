{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Windows.Detect
  (
    ANSIEnabledStatus (..)
  , ConsoleDefaultState (..)
  , isANSIEnabled
  ) where

import Control.Exception (SomeException(..), onException, throwIO, try)
import Data.Bits ((.&.), (.|.))
-- 'lookupEnv' is not available until base-4.6.0.0 (GHC 7.6.1)
import System.Environment.Compat (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import System.Console.ANSI.Windows.Foreign (bACKGROUND_INTENSE_WHITE,
  ConsoleException(..), CONSOLE_SCREEN_BUFFER_INFO (..), DWORD,
  eNABLE_VIRTUAL_TERMINAL_PROCESSING, fOREGROUND_INTENSE_WHITE,
  getConsoleMode, getConsoleScreenBufferInfo, getStdHandle, HANDLE,
  iNVALID_HANDLE_VALUE, nullHANDLE, setConsoleMode, sTD_OUTPUT_HANDLE, WORD)

-- | The default state of the console
data ConsoleDefaultState = ConsoleDefaultState
  { defaultForegroundAttributes :: WORD -- ^ Foreground attributes
  , defaultBackgroundAttributes :: WORD -- ^ Background attributes
  } deriving (Eq, Show)

-- | The status of the console as regards it being ANSI-enabled or not
-- ANSI-enabled.
data ANSIEnabledStatus
  = ANSIEnabled -- ^ ANSI-enabled
  | NotANSIEnabled ConsoleDefaultState -- ^ Not ANSI-enabled (including the
                                       -- state of the console when that status
                                       -- was determined)
  deriving (Eq, Show)

-- | This function assumes that once it is first established whether or not the
-- Windows console is ANSI-enabled, that will not change. If the console is not
-- ANSI-enabled, the state of the console is considered to be its default state.
{-# NOINLINE isANSIEnabled #-}
isANSIEnabled :: ANSIEnabledStatus
isANSIEnabled = unsafePerformIO $ do
  e <- safeIsANSIEnabled
  if e
    then return ANSIEnabled
    else do
      hOut <- getValidStdHandle sTD_OUTPUT_HANDLE
      info <- getConsoleScreenBufferInfo hOut
      let attributes = csbi_attributes info
          fgAttributes = attributes .&. fOREGROUND_INTENSE_WHITE
          bgAttributes = attributes .&. bACKGROUND_INTENSE_WHITE
          consoleDefaultState = ConsoleDefaultState
            { defaultForegroundAttributes = fgAttributes
            , defaultBackgroundAttributes = bgAttributes }
      return $ NotANSIEnabled consoleDefaultState

-- This function takes the following approach. If the environment variable
-- APPVEYOR exists and is set to 'True', it assumes the code is running in the
-- Appveyor build environment and that the build console is ANSI-enabled.
-- Otherwise, if the environment variable TERM exists and is not set to 'dumb'
-- or 'msys' (see below), it assumes the console is ANSI-enabled. Otherwise, it
-- tries to get a valid standard output handle. If that does not fail, it
-- tried to get a ConHost console mode. If that fails, it assumes that the
-- handle is ANSI-enabled. Otherwise, it tries to enable virtual terminal
-- processing. If that fails, it assumes the console is not ANSI-enabled.
--
-- In Git Shell, if Command Prompt or PowerShell are used, the environment
-- variable TERM is set to 'msys'. If 'Git Bash' (mintty) is used, TERM is set
-- to 'xterm' (by default).
safeIsANSIEnabled :: IO Bool
safeIsANSIEnabled = do
  appveyor <- lookupEnv "APPVEYOR"
  case appveyor of
    Just "True" -> return True
    _           -> do
      term <- lookupEnv "TERM"
      case term of
        Just "dumb" -> return False
        Just "msys" -> doesEnableANSIOutSucceed
        Just _      -> return True
        Nothing     -> doesEnableANSIOutSucceed

-- This function returns whether or not an attempt to enable virtual terminal
-- processing succeeded, or was deemed to succeed, in the IO monad.
doesEnableANSIOutSucceed :: IO Bool
doesEnableANSIOutSucceed = do
  result <- try enableANSIOut :: IO (Either SomeException ())
  case result of
    Left _ -> return False
    Right () -> return True

-- This function tries to get a valid handle on the standard output and throws
-- an exception if it cannot. It then tries to get a ConHost console mode for
-- that handle. If it can not, it assumes that the standard output handle is
-- ANSI-enabled. If it can, it tries to enable virtual terminal processing and
-- throws an exception if it can not.
enableANSIOut :: IO ()
enableANSIOut = do
  hOut <- getValidStdHandle sTD_OUTPUT_HANDLE
  result <- conHostConsoleMode hOut
  case result of
    Nothing -> return ()  -- assume hOut is ANSI-enabled
    Just mOut -> do
      let mOut' = mOut .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING
      setConsoleMode hOut mOut'

-- This function tries to get a valid standard handle and throws an exception if
-- it cannot.
getValidStdHandle :: DWORD -> IO HANDLE
getValidStdHandle nStdHandle = do
  h <- getStdHandle nStdHandle
  if h == iNVALID_HANDLE_VALUE || h == nullHANDLE
    then throwIO $ ConsoleException 6 -- Invalid handle or no handle
    else return h

-- The function tries to get a ConHost console mode from a handle that is
-- assumed to be a valid standard handle (see getValidStdHandle)
conHostConsoleMode :: HANDLE -> IO (Maybe DWORD)
conHostConsoleMode h = do
  result <- try (getConsoleMode h) :: IO (Either SomeException DWORD)
  case result of
    Left _ -> return Nothing
    Right mode -> return (Just mode)
