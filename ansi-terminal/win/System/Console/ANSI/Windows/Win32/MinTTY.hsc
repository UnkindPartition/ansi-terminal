{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| This module is based on the corresponding code in the mintty package and the
Win32 package, in order to avoid a dependency on those packages.
-}
module System.Console.ANSI.Windows.Win32.MinTTY
  ( isMinTTYHandle
  ) where

import Control.Exception ( catch )
import Data.Int ( Int32 )
import Data.List ( isInfixOf )
import Data.Word ( Word8 )
import Foreign.C.String
         ( peekCWStringLen, withCAString, withCWString, withCWStringLen )
import Foreign.C.Types ( CInt (..) )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Array ( advancePtr, copyArray )
import Foreign.Marshal.Utils ( maybeWith )
import Foreign.Ptr ( FunPtr, Ptr, castPtr, castPtrToFunPtr, plusPtr )
import Foreign.Storable ( Storable (..) )

-- Provided by the ansi-terminal package
import System.Console.ANSI.Windows.Win32.Types
         ( Addr, BOOL, DWORD, FileType, HANDLE, HMODULE, LPCSTR, LPCTSTR, LPTSTR
         , TCHAR, ULONG, USHORT, failIfFalse_, failIfNeg, failIfNull
         )

-- The headers that are shipped with GHC's copy of MinGW-w64 assume Windows XP.
-- Since we need some structs that are only available with Vista or later,
-- we must manually set WINVER/_WIN32_WINNT accordingly.
#undef WINVER
#define WINVER 0x0600
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#include <windows.h>
#include "winternl_compat.h"

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

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
    vfniFileName <-
      peekTStringLen (plusPtr buf (#offset FILE_NAME_INFO, FileName), len)
    pure $ FILE_NAME_INFO
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
    pure $ UNICODE_STRING
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
    then pure False
    else isMinTTYVista h `catch` \(_ :: IOError) -> isMinTTYCompat h
    -- GetFileNameByHandleEx is only available on Vista and later (hence
    -- the name isMinTTYVista). If we're on an older version of Windows,
    -- getProcAddress will throw an IOException when it fails to find
    -- GetFileNameByHandleEx, and thus we will default to using
    -- NtQueryObject (isMinTTYCompat).

isMinTTYVista :: HANDLE -> IO Bool
isMinTTYVista h = do
    fn <- getFileNameByHandle h
    pure $ cygwinMSYSCheck fn
  `catch` \(_ :: IOError) -> pure False

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
    pure $ cygwinMSYSCheck fn
  `catch` \(_ :: IOError) -> pure False

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
      pure $ usBuffer $ oniName oni

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
    pure $ fniFileName fni

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

foreign import ccall unsafe "windows.h GetFileType"
  getFileType :: HANDLE -> IO FileType

foreign import ccall unsafe "windows.h GetProcAddress"
  c_GetProcAddress :: HMODULE -> LPCSTR -> IO Addr

foreign import ccall "dynamic"
  mk_NtQueryObject :: FunPtr F_NtQueryObject -> F_NtQueryObject

foreign import ccall unsafe "windows.h GetModuleHandleW"
  c_GetModuleHandle :: LPCTSTR -> IO HMODULE
