{-# LANGUAGE ScopedTypeVariables #-}

{-| This module is based on the corresponding code in the mintty package and the
Win32 package, in order to avoid a dependency on those packages.
-}
module System.Win32.MinTTY
  ( isMinTTYHandle
  ) where

import Control.Exception ( catch, throwIO )
import Data.Char ( isSpace )
import Data.Int ( Int32 )
import Data.List ( isInfixOf )
import Data.Word ( Word16, Word32, Word8 )
import Foreign.C.Error ( Errno (..), errnoToIOError )
import Foreign.C.String
         ( peekCWString, peekCWStringLen, withCAString, withCWString
         , withCWStringLen
         )
import Foreign.C.Types ( CChar, CInt (..), CWchar )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Array ( advancePtr, copyArray )
import Foreign.Marshal.Utils ( maybeWith )
import Foreign.Ptr ( FunPtr, Ptr, castPtr, castPtrToFunPtr, nullPtr, plusPtr )
import Foreign.Storable ( Storable (..) )
import Numeric ( showHex )
import System.IO.Error ( ioeSetErrorString )

-- The headers that are shipped with GHC's copy of MinGW-w64 assume Windows XP.
-- Since we need some structs that are only available with Vista or later,
-- we must manually set WINVER/_WIN32_WINNT accordingly.
#undef WINVER
#define WINVER 0x0600
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
##include "windows_cconv.h"
#include <windows.h>
#include "winternl_compat.h"

type Addr = Ptr ()
type BOOL = Bool
type DWORD = Word32
type ErrCode = DWORD
type FileType = DWORD
type HANDLE = Ptr ()
type HMODULE = Ptr ()
type LPCSTR = LPSTR
type LPCTSTR = LPTSTR
type LPSTR = Ptr CChar
type LPTSTR = Ptr TCHAR
type LPWSTR = Ptr CWchar
type TCHAR = CWchar
type ULONG = Word32
type USHORT = Word16
type F_NtQueryObject =
     HANDLE
  -> CInt
  -> Ptr OBJECT_NAME_INFORMATION
  -> ULONG
  -> Ptr ULONG
  -> IO NTSTATUS
type F_GetFileInformationByHandleEx =
  HANDLE -> CInt -> Ptr FILE_NAME_INFO -> DWORD -> IO BOOL
type NTSTATUS = #type NTSTATUS

data FILE_NAME_INFO = FILE_NAME_INFO
  { fniFileNameLength :: DWORD
  , fniFileName :: String
  }

instance Storable FILE_NAME_INFO where
  sizeOf _ = #size FILE_NAME_INFO
  alignment _ = #alignment FILE_NAME_INFO
  poke buf fni = withTStringLen (fniFileName fni) $ \(str, len) -> do
    let len' = (min mAX_PATH len) * sizeOfTCHAR
        start = advancePtr (castPtr buf) (#offset FILE_NAME_INFO, FileName)
        end = advancePtr start len'
    (#poke FILE_NAME_INFO, FileNameLength) buf len'
    copyArray start (castPtr str :: Ptr Word8) len'
    poke (castPtr end) (0 :: TCHAR)
  peek buf = do
    vfniFileNameLength <- (#peek FILE_NAME_INFO, FileNameLength) buf
    let len = fromIntegral vfniFileNameLength `div` sizeOfTCHAR
    vfniFileName <- peekTStringLen (plusPtr buf (#offset FILE_NAME_INFO, FileName), len)
    return $ FILE_NAME_INFO
      { fniFileNameLength = vfniFileNameLength
      , fniFileName = vfniFileName
      }

newtype OBJECT_NAME_INFORMATION = OBJECT_NAME_INFORMATION
  { oniName :: UNICODE_STRING }

instance Storable OBJECT_NAME_INFORMATION where
  sizeOf _ = #size OBJECT_NAME_INFORMATION
  alignment _ = #alignment OBJECT_NAME_INFORMATION
  poke buf oni = (#poke OBJECT_NAME_INFORMATION, Name) buf (oniName oni)
  peek buf =
    fmap OBJECT_NAME_INFORMATION $ (#peek OBJECT_NAME_INFORMATION, Name) buf

data UNICODE_STRING = UNICODE_STRING
  { usLength :: USHORT
  , usMaximumLength :: USHORT
  , usBuffer :: String
  }

instance Storable UNICODE_STRING where
  sizeOf _ = #size UNICODE_STRING
  alignment _ = #alignment UNICODE_STRING
  poke buf us = withTStringLen (usBuffer us) $ \(str, len) -> do
    let len' = (min mAX_PATH len) * sizeOfTCHAR
        start = advancePtr (castPtr buf) (#size UNICODE_STRING)
        end = advancePtr start len'
    (#poke UNICODE_STRING, Length) buf len'
    (#poke UNICODE_STRING, MaximumLength) buf (len' + sizeOfTCHAR)
    (#poke UNICODE_STRING, Buffer) buf start
    copyArray start (castPtr str :: Ptr Word8) len'
    poke (castPtr end) (0 :: TCHAR)
  peek buf = do
    vusLength <- (#peek UNICODE_STRING, Length) buf
    vusMaximumLength <- (#peek UNICODE_STRING, MaximumLength) buf
    vusBufferPtr <- (#peek UNICODE_STRING, Buffer) buf
    let len = fromIntegral vusLength `div` sizeOfTCHAR
    vusBuffer <- peekTStringLen (vusBufferPtr, len)
    return $ UNICODE_STRING
      { usLength = vusLength
      , usMaximumLength = vusMaximumLength
      , usBuffer = vusBuffer
      }

-- | Returns 'True' is the given handle is attached to a MinTTY console
-- (e.g., Cygwin or MSYS). Returns 'False' otherwise.
isMinTTYHandle :: HANDLE -> IO Bool
isMinTTYHandle h = do
  fileType <- getFileType h
  if fileType /= fILE_TYPE_PIPE
    then return False
    else isMinTTYVista h `catch` \(_ :: IOError) -> isMinTTYCompat h
    -- GetFileNameByHandleEx is only available on Vista and later (hence
    -- the name isMinTTYVista). If we're on an older version of Windows,
    -- getProcAddress will throw an IOException when it fails to find
    -- GetFileNameByHandleEx, and thus we will default to using
    -- NtQueryObject (isMinTTYCompat).

isMinTTYVista :: HANDLE -> IO Bool
isMinTTYVista h = do
    fn <- getFileNameByHandle h
    return $ cygwinMSYSCheck fn
  `catch` \(_ :: IOError) ->
    return False

cygwinMSYSCheck :: String -> Bool
cygwinMSYSCheck fn =
     ("cygwin-" `isInfixOf` fn || "msys-" `isInfixOf` fn)
  && "-pty" `isInfixOf` fn
-- Note that GetFileInformationByHandleEx might return a filepath like:
--
--    \msys-dd50a72ab4668b33-pty1-to-master
--
-- But NtQueryObject might return something like:
--
--    \Device\NamedPipe\msys-dd50a72ab4668b33-pty1-to-master
--
-- This means we can't rely on "\cygwin-" or "\msys-" being at the very start
-- of the filepath. As a result, we use `isPrefixOf` to check for "cygwin" and
-- "msys".
--
-- It's unclear if "-master" will always appear in the filepath name. Recent
-- versions of MinTTY have been known to give filepaths like this (#186):
--
--    \msys-dd50a72ab4668b33-pty0-to-master-nat
--
-- Just in case MinTTY ever changes this convention, we don't bother checking
-- for the presence of "-master" in the filepath name at all.

isMinTTYCompat :: HANDLE -> IO Bool
isMinTTYCompat h = do
    fn <- ntQueryObjectNameInformation h
    return $ cygwinMSYSCheck fn
  `catch` \(_ :: IOError) ->
    return False

fILE_TYPE_PIPE :: FileType
fILE_TYPE_PIPE = 3

ntQueryObjectNameInformation :: HANDLE -> IO String
ntQueryObjectNameInformation h = do
  let sizeOfONI = sizeOf (undefined :: OBJECT_NAME_INFORMATION)
      bufSize   = sizeOfONI + mAX_PATH * sizeOfTCHAR
  allocaBytes bufSize $ \buf ->
    alloca $ \p_len -> do
      hwnd <- getModuleHandle (Just "ntdll.exe")
      addr <- getProcAddress hwnd "NtQueryObject"
      let c_NtQueryObject = mk_NtQueryObject (castPtrToFunPtr addr)
      _ <- failIfNeg "NtQueryObject" $ c_NtQueryObject
             h objectNameInformation buf (fromIntegral bufSize) p_len
      oni <- peek buf
      return $ usBuffer $ oniName oni

sizeOfTCHAR :: Int
sizeOfTCHAR = sizeOf (undefined :: TCHAR)

getFileNameByHandle :: HANDLE -> IO String
getFileNameByHandle h = do
  let sizeOfDWORD = sizeOf (undefined :: DWORD)
      -- note: implicitly assuming that DWORD has stronger alignment than wchar_t
      bufSize     = sizeOfDWORD + mAX_PATH * sizeOfTCHAR
  allocaBytes bufSize $ \buf -> do
    getFileInformationByHandleEx h fileNameInfo buf (fromIntegral bufSize)
    fni <- peek buf
    return $ fniFileName fni

getFileInformationByHandleEx ::
     HANDLE
  -> CInt
  -> Ptr FILE_NAME_INFO
  -> DWORD
  -> IO ()
getFileInformationByHandleEx h cls buf bufSize = do
  lib <- getModuleHandle (Just "kernel32.dll")
  ptr <- getProcAddress lib "GetFileInformationByHandleEx"
  let c_GetFileInformationByHandleEx =
        mk_GetFileInformationByHandleEx (castPtrToFunPtr ptr)
  failIfFalse_ "getFileInformationByHandleEx"
    (c_GetFileInformationByHandleEx h cls buf bufSize)

getModuleHandle :: Maybe String -> IO HMODULE
getModuleHandle mb_name =
  maybeWith withTString mb_name $ \ c_name ->
  failIfNull "GetModuleHandle" $ c_GetModuleHandle c_name

getProcAddress :: HMODULE -> String -> IO Addr
getProcAddress hmod procname =
  withCAString procname $ \ c_procname ->
  failIfNull "GetProcAddress" $ c_GetProcAddress hmod c_procname

failIfNeg :: (Num a, Ord a) => String -> IO a -> IO a
failIfNeg = failIf (< 0)

failIfFalse_ :: String -> IO Bool -> IO ()
failIfFalse_ = failIf_ not

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
  v <- act
  if p v then errorWin wh else return v

failIf_ :: (a -> Bool) -> String -> IO a -> IO ()
failIf_ p wh act = do
  v <- act
  if p v then errorWin wh else return ()

failIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
failIfNull = failIf (== nullPtr)

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

peekTStringLen :: (LPCTSTR, Int) -> IO String
peekTStringLen = peekCWStringLen

withTString :: String -> (LPTSTR -> IO a) -> IO a
withTString = withCWString

withTStringLen :: String -> ((LPTSTR, Int) -> IO a) -> IO a
withTStringLen = withCWStringLen

fileNameInfo :: CInt
fileNameInfo = #const FileNameInfo

mAX_PATH :: Num a => a
mAX_PATH = #const MAX_PATH

objectNameInformation :: CInt
objectNameInformation = #const ObjectNameInformation

foreign import ccall "dynamic"
  mk_GetFileInformationByHandleEx ::
    FunPtr F_GetFileInformationByHandleEx -> F_GetFileInformationByHandleEx

foreign import ccall unsafe "maperrno_func" -- in base/cbits/Win32Utils.c
   c_maperrno_func :: ErrCode -> IO Errno

foreign import ccall unsafe "errors.h"
  getErrorMessage :: DWORD -> IO LPWSTR

foreign import ccall unsafe "windows.h GetLastError"
  getLastError :: IO ErrCode

foreign import ccall unsafe "windows.h LocalFree"
  localFree :: Ptr a -> IO (Ptr a)

foreign import ccall unsafe "windows.h GetFileType"
  getFileType :: HANDLE -> IO FileType

foreign import ccall unsafe "windows.h GetProcAddress"
  c_GetProcAddress :: HMODULE -> LPCSTR -> IO Addr

foreign import ccall "dynamic"
  mk_NtQueryObject :: FunPtr F_NtQueryObject -> F_NtQueryObject

foreign import ccall unsafe "windows.h GetModuleHandleW"
  c_GetModuleHandle :: LPCTSTR -> IO HMODULE
