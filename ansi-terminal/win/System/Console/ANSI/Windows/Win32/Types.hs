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

import Control.Concurrent.MVar ( readMVar )
import Control.Exception ( bracket, throwIO )
import Control.Monad ( when )
import Data.Char ( isSpace )
import Data.Typeable ( cast )
import Data.Word ( Word16, Word32 )
import Foreign.C.Error ( Errno (..), errnoToIOError )
import Foreign.C.String ( peekCWString )
import Foreign.C.Types ( CChar, CInt (..), CShort (..), CWchar )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.StablePtr ( StablePtr, freeStablePtr, newStablePtr )
#if MIN_VERSION_base(4,20,0)
import GHC.Internal.IO.Handle.Types ( Handle (..), Handle__ (..) )
import GHC.Internal.IO.FD ( FD(..) ) -- A wrapper around an Int32
#else
import GHC.IO.Handle.Types ( Handle (..), Handle__ (..) )
import GHC.IO.FD ( FD(..) ) -- A wrapper around an Int32
#endif
import Numeric ( showHex )
import System.IO.Error ( ioeSetErrorString )

#if defined(__IO_MANAGER_WINIO__)
#if MIN_VERSION_base(4,20,0)
import GHC.Internal.IO.Exception
         ( IOErrorType (InappropriateType), IOException (IOError), ioException )
import GHC.Internal.IO.SubSystem ( (<!>) )
import GHC.Internal.IO.Windows.Handle ( ConsoleHandle, Io, NativeHandle, toHANDLE )
#else
import GHC.IO.Exception
         ( IOErrorType (InappropriateType), IOException (IOError), ioException )
import GHC.IO.SubSystem ( (<!>) )
import GHC.IO.Windows.Handle ( ConsoleHandle, Io, NativeHandle, toHANDLE )
#endif
#endif

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

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr

#if defined(__IO_MANAGER_WINIO__)

withHandleToHANDLE :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLE = withHandleToHANDLEPosix <!> withHandleToHANDLENative

withHandleToHANDLENative :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLENative haskell_handle action =
  withStablePtr haskell_handle $ const $ do
    let write_handle_mvar = case haskell_handle of
            FileHandle _ handle_mvar     -> handle_mvar
            DuplexHandle _ _ handle_mvar -> handle_mvar
    windows_handle <- readMVar write_handle_mvar >>= handle_ToHANDLE
    action windows_handle
 where
  handle_ToHANDLE :: Handle__ -> IO HANDLE
  handle_ToHANDLE (Handle__{haDevice = dev}) =
    case ( cast dev :: Maybe (Io NativeHandle)
         , cast dev :: Maybe (Io ConsoleHandle)) of
      (Just hwnd, Nothing) -> pure $ toHANDLE hwnd
      (Nothing, Just hwnd) -> pure $ toHANDLE hwnd
      _                    -> throwErr "not a known HANDLE"

  throwErr msg = ioException $ IOError (Just haskell_handle)
    InappropriateType "withHandleToHANDLENative" msg Nothing Nothing

#else

withHandleToHANDLE :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLE = withHandleToHANDLEPosix

#endif

withHandleToHANDLEPosix :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLEPosix haskell_handle action =
  -- Create a stable pointer to the Handle. This prevents the garbage collector
  -- getting to it while we are doing horrible manipulations with it, and hence
  -- stops it being finalized (and closed).
  withStablePtr haskell_handle $ const $ do
    -- Grab the write handle variable from the Handle
    let write_handle_mvar = case haskell_handle of
          FileHandle _ handle_mvar     -> handle_mvar
          DuplexHandle _ _ handle_mvar -> handle_mvar
          -- This is "write" MVar, we could also take the "read" one

    -- Get the FD from the algebraic data type
    Just fd <- (\(Handle__ { haDevice = dev }) -> fmap fdFD (cast dev))
             <$> readMVar write_handle_mvar

    -- Finally, turn that (C-land) FD into a HANDLE using msvcrt
    windows_handle <- c_get_osfhandle fd
    -- Do what the user originally wanted
    action windows_handle

-- This essential function comes from the C runtime system. It is certainly
-- provided by msvcrt, and also seems to be provided by the mingw C library -
-- hurrah!
foreign import ccall unsafe "_get_osfhandle"
  c_get_osfhandle :: CInt -> IO HANDLE

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
