{-# LANGUAGE Safe #-}

{-| This module is based on the corresponding code in the Win32 package, in
order to avoid a dependency on that package. Some of that code had its origins
in earlier versions of this package.
-}
module System.Console.ANSI.Windows.Win32.Types
  ( Addr
  , BOOL
  , DWORD
  , ErrCode
  , FileType
  , HANDLE
  , HMODULE
  , LPCSTR
  , LPCTSTR
  , LPDWORD
  , LPTSTR
  , SHORT
  , TCHAR
  , UINT
  , UINT_PTR
  , ULONG
  , USHORT
  , WCHAR
  , WORD
  , failIfFalse_
  , failIfNeg
  , failIfNull
  , withHandleToHANDLE
  ) where

import Control.Exception ( throwIO )
import Control.Monad ( when )
import Data.Char ( isSpace )
import Data.Word ( Word16, Word32 )
import Foreign.C.Error ( Errno (..), errnoToIOError )
import Foreign.C.String ( peekCWString )
import Foreign.C.Types ( CChar, CInt (..), CShort (..), CWchar )
import Foreign.Ptr ( Ptr, nullPtr )
import Numeric ( showHex )
import System.IO.Error ( ioeSetErrorString )
import System.Win32.Types (withHandleToHANDLE)

type Addr = Ptr ()
type BOOL = Bool
type DWORD = Word32
type ErrCode = DWORD
type FileType = DWORD
type HANDLE = Ptr ()
type HMODULE = Ptr ()
type LPCSTR = LPSTR
type LPCTSTR = LPTSTR
type LPDWORD = Ptr DWORD
type LPSTR = Ptr CChar
type LPTSTR = Ptr TCHAR
type LPWSTR = Ptr CWchar
type SHORT = CShort
type TCHAR = CWchar
type UINT = Word32
type UINT_PTR = Word
type ULONG = Word32
type USHORT = Word16
type WCHAR = CWchar
type WORD = Word16

failIfNeg :: (Num a, Ord a) => String -> IO a -> IO a
failIfNeg = failIf (< 0)

failIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
failIfNull = failIf (== nullPtr)

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
  v <- act
  if p v then errorWin wh else return v

failIfFalse_ :: String -> IO Bool -> IO ()
failIfFalse_ = failIf_ not

failIf_ :: (a -> Bool) -> String -> IO a -> IO ()
failIf_ p wh act = do
  v <- act
  when (p v) $ errorWin wh

errorWin :: String -> IO a
errorWin fn_name = do
  err_code <- getLastError
  failWith fn_name err_code

failWith :: String -> ErrCode -> IO a
failWith fn_name err_code = do
  c_msg <- getErrorMessage err_code
  msg <- if c_msg == nullPtr
    then return $ "Error 0x" ++ Numeric.showHex err_code ""
    else do
      msg <- peekTString c_msg
      -- We ignore failure of freeing c_msg, given we're already failing
      _ <- localFree c_msg
      return msg
  -- turn GetLastError() into errno, which errnoToIOError knows how to convert
  -- to an IOException we can throw.
  errno <- c_maperrno_func err_code
  let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
      ioerror = errnoToIOError fn_name errno Nothing Nothing
                  `ioeSetErrorString` msg'
  throwIO ioerror

peekTString :: LPCTSTR -> IO String
peekTString = peekCWString

foreign import ccall unsafe "maperrno_func" -- in base/cbits/Win32Utils.c
   c_maperrno_func :: ErrCode -> IO Errno

foreign import ccall unsafe "errors.h _ansi_terminal_getErrorMessage"
  getErrorMessage :: DWORD -> IO LPWSTR

foreign import ccall unsafe "windows.h GetLastError"
  getLastError :: IO ErrCode

foreign import ccall unsafe "windows.h LocalFree"
  localFree :: Ptr a -> IO (Ptr a)
