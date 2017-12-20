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
    else onException (do
      hOut <- getValidStdHandle sTD_OUTPUT_HANDLE
      info <- getConsoleScreenBufferInfo hOut
      let attributes = csbi_attributes info
          fgAttributes = attributes .&. fOREGROUND_INTENSE_WHITE
          bgAttributes = attributes .&. bACKGROUND_INTENSE_WHITE
          consoleDefaultState = ConsoleDefaultState
            { defaultForegroundAttributes = fgAttributes
            , defaultBackgroundAttributes = bgAttributes }
      return $ NotANSIEnabled consoleDefaultState)
      (putStrLn $ "A fatal error has occurred. An attempt has been made to " ++
        "send console virtual terminal sequences (ANSI codes) to an output " ++
        "that has not been recognised as an ANSI-capable terminal and also " ++
        "cannot be emulated as an ANSI-enabled terminal (emulation needs a " ++
        "ConHost-based terminal, such as Command Prompt or PowerShell).\n\n" ++
        "If that is unexpected, please post an issue at the home page of " ++
        "the ansi-terminal package. See " ++
        "https://hackage.haskell.org/package/ansi-terminal for the home " ++
        "page location.\n")

-- This function takes the following approach. If the environment variable
-- APPVEYOR exists and is set to 'True', it assumes the code is running in the
-- Appveyor build environment and that the build console is ANSI-enabled.
-- Otherwise, if the environment variable TERM exists and is not set to 'dumb'
-- or 'msys' (see below), it assumes the console is ANSI-enabled. Otherwise, it
-- tries to enable virtual terminal processing. If that fails, it assumes the
-- console is not ANSI-enabled.
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
-- processing succeeded, in the IO monad.
doesEnableANSIOutSucceed :: IO Bool
doesEnableANSIOutSucceed = do
  result <- try enableANSIOut :: IO (Either SomeException ())
  case result of
    Left _ -> return False
    Right () -> return True

-- This function tries to enable virtual terminal processing on the standard
-- output and throws an exception if it cannot.
enableANSIOut :: IO ()
enableANSIOut = do
  hOut <- getValidStdHandle sTD_OUTPUT_HANDLE
  mOut <- getConsoleMode hOut
  let mOut' = mOut .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING
  setConsoleMode hOut mOut'

-- This function tries to get a valid standard handle and throws an exception if
-- it cannot.
getValidStdHandle :: DWORD -> IO HANDLE
getValidStdHandle nStdHandle = do
  h <- getStdHandle nStdHandle
  if h == iNVALID_HANDLE_VALUE || h == nullHANDLE
    then throwIO $ ConsoleException 6 -- Invalid Handle
    else return h
