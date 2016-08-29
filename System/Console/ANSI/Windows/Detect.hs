{-# OPTIONS_HADDOCK hide #-}

module System.Console.ANSI.Windows.Detect
(
    isANSIEnabled
) where

import Control.Exception (SomeException(..), throwIO, try)
import Data.Bits ((.|.))
import System.Console.ANSI.Windows.Foreign (ConsoleException(..), DWORD,
    eNABLE_VIRTUAL_TERMINAL_PROCESSING, getConsoleMode, getStdHandle, HANDLE,
    iNVALID_HANDLE_VALUE, nullHANDLE, setConsoleMode, sTD_OUTPUT_HANDLE)
-- 'lookupEnv' is not available until base-4.6.0.0 (GHC 7.6.1)
import System.Environment.Compat (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- This function assumes that once it is first established whether or not the
-- Windows console is ANSI-enabled, that will not change.
{-# NOINLINE isANSIEnabled #-}
isANSIEnabled :: Bool
isANSIEnabled = unsafePerformIO safeIsANSIEnabled

-- This function takes the following approach. If the environment variable TERM
-- exists and is not set to 'dumb' or 'msys' (see below), it assumes the console
-- is ANSI-enabled. Otherwise, it tries to enable virtual terminal processing.
-- If that fails, it assumes the console is not ANSI-enabled.
--
-- In Git Shell, if Command Prompt or PowerShell are used, the environment
-- variable TERM is set to 'msys'. If 'Git Bash' (mintty) is used, TERM is set
-- to 'xterm' (by default).
safeIsANSIEnabled :: IO Bool
safeIsANSIEnabled = do
    result <- lookupEnv "TERM"
    case result of
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
