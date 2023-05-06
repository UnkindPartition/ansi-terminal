{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE Safe               #-}

module System.Console.ANSI.Windows.Foreign
  (
    INPUT_RECORD (..)
  , INPUT_RECORD_EVENT (..)
  , KEY_EVENT_RECORD (..)
  , getNumberOfConsoleInputEvents
  , readConsoleInput
  , cWcharsToChars
  , unicodeAsciiChar
  ) where

import Control.Exception ( Exception )
import Data.Char ( chr )
import Data.Typeable ( Typeable )
import Data.Word ( Word32 )
import Foreign.C.Types ( CWchar (..) )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, peekArray, pokeArray )
import Foreign.Ptr ( Ptr, castPtr, plusPtr )
import Foreign.Storable ( Storable (..) )
import System.Console.ANSI.Windows.Win32.Types
         ( BOOL, DWORD, ErrCode, HANDLE, LPDWORD, SHORT, UINT, ULONG, WCHAR
         , WORD, failIfFalse_
         )

peekAndOffset :: Storable a => Ptr a -> IO (a, Ptr b)
peekAndOffset ptr = do
  item <- peek ptr
  pure (item, ptr `plusPtr` sizeOf item)

pokeAndOffset :: Storable a => Ptr a -> a -> IO (Ptr b)
pokeAndOffset ptr item = do
  poke ptr item
  pure (ptr `plusPtr` sizeOf item)

data COORD = COORD
  { coord_x :: SHORT
  , coord_y :: SHORT
  } deriving (Read, Eq)

instance Show COORD where
  show (COORD x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Storable COORD where
  sizeOf ~(COORD x y) = sizeOf x + sizeOf y
  alignment ~(COORD x _) = alignment x
  peek ptr = do
    let ptr' = castPtr ptr :: Ptr SHORT
    x <- peekElemOff ptr' 0
    y <- peekElemOff ptr' 1
    pure (COORD x y)
  poke ptr (COORD x y) = do
    let ptr' = castPtr ptr :: Ptr SHORT
    pokeElemOff ptr' 0 x
    pokeElemOff ptr' 1 y

data SMALL_RECT = SMALL_RECT COORD COORD

instance Show SMALL_RECT where
  show (SMALL_RECT tl br) = show tl ++ "-" ++ show br

instance Storable SMALL_RECT where
  sizeOf ~(SMALL_RECT tl br) = sizeOf tl + sizeOf br
  alignment ~(SMALL_RECT tl _) = alignment tl
  peek ptr = do
    let ptr' = castPtr ptr :: Ptr COORD
    tl <- peekElemOff ptr' 0
    br <- peekElemOff ptr' 1
    pure (SMALL_RECT tl br)
  poke ptr (SMALL_RECT tl br) = do
    let ptr' = castPtr ptr :: Ptr COORD
    pokeElemOff ptr' 0 tl
    pokeElemOff ptr' 1 br

data CONSOLE_CURSOR_INFO = CONSOLE_CURSOR_INFO
  { cci_cursor_size    :: DWORD
  , cci_cursor_visible :: BOOL
  } deriving (Show)

instance Storable CONSOLE_CURSOR_INFO where
  sizeOf ~(CONSOLE_CURSOR_INFO size visible) = sizeOf size + sizeOf visible
  alignment ~(CONSOLE_CURSOR_INFO size _) = alignment size
  peek ptr = do
    (size, ptr') <- peekAndOffset (castPtr ptr)
    visible <- peek ptr'
    pure (CONSOLE_CURSOR_INFO size visible)
  poke ptr (CONSOLE_CURSOR_INFO size visible) = do
    ptr' <- pokeAndOffset (castPtr ptr) size
    poke ptr' visible

data CONSOLE_SCREEN_BUFFER_INFO = CONSOLE_SCREEN_BUFFER_INFO
  { csbi_size                :: COORD
  , csbi_cursor_position     :: COORD
  , csbi_attributes          :: WORD
  , csbi_window              :: SMALL_RECT
  , csbi_maximum_window_size :: COORD
  } deriving (Show)

instance Storable CONSOLE_SCREEN_BUFFER_INFO where
  sizeOf ~(CONSOLE_SCREEN_BUFFER_INFO
    size cursor_position attributes window maximum_window_size)
    = sizeOf size + sizeOf cursor_position + sizeOf attributes + sizeOf window
        + sizeOf maximum_window_size
  alignment ~(CONSOLE_SCREEN_BUFFER_INFO size _ _ _ _) = alignment size
  peek ptr = do
    (size, ptr1) <- peekAndOffset (castPtr ptr)
    (cursor_position, ptr2) <- peekAndOffset ptr1
    (attributes, ptr3) <- peekAndOffset ptr2
    (window, ptr4) <- peekAndOffset ptr3
    maximum_window_size <- peek ptr4
    pure (CONSOLE_SCREEN_BUFFER_INFO
      size cursor_position attributes window maximum_window_size)
  poke ptr (CONSOLE_SCREEN_BUFFER_INFO
    size cursor_position attributes window maximum_window_size)
    = do
      ptr1 <- pokeAndOffset (castPtr ptr) size
      ptr2 <- pokeAndOffset ptr1 cursor_position
      ptr3 <- pokeAndOffset ptr2 attributes
      ptr4 <- pokeAndOffset ptr3 window
      poke ptr4 maximum_window_size

data CONSOLE_SCREEN_BUFFER_INFOEX = CONSOLE_SCREEN_BUFFER_INFOEX
  { csbix_size                 :: COORD
  , csbix_cursor_position      :: COORD
  , csbix_attributes           :: WORD
  , csbix_window               :: SMALL_RECT
  , csbix_maximum_window_size  :: COORD
  , csbix_popup_attributes     :: WORD
  , csbix_fullscreen_supported :: BOOL
  , csbix_color_table          :: [COLORREF]
  } deriving (Show)

-- When specifying an explicit RGB color, the COLORREF value has the following
-- hexadecimal form:
-- 0x00bbggrr
-- The low-order byte contains a value for the relative intensity of red; the
-- second byte contains a value for green; and the third byte contains a value
-- for blue. The high-order byte must be zero. The maximum value for a single
-- byte is 0xFF.
type COLORREF = Word32

instance Storable CONSOLE_SCREEN_BUFFER_INFOEX where
  sizeOf ~(CONSOLE_SCREEN_BUFFER_INFOEX
    size cursor_position attributes window maximum_window_size popup_attributes
      fullscreen_supported _)
    = sizeOf sizeCsbix + sizeOf size + sizeOf cursor_position + sizeOf attributes + sizeOf window
        + sizeOf maximum_window_size + sizeOf popup_attributes
        + sizeOf fullscreen_supported + 16 * sizeOf (undefined :: COLORREF)
  alignment ~(CONSOLE_SCREEN_BUFFER_INFOEX _ _ _ _ _ _ _ _) = alignment sizeCsbix
  peek ptr = do
    let ptr0 = castPtr ptr `plusPtr` sizeOf sizeCsbix
    (size, ptr1) <- peekAndOffset ptr0
    (cursor_position, ptr2) <- peekAndOffset ptr1
    (attributes, ptr3) <- peekAndOffset ptr2
    (window, ptr4) <- peekAndOffset ptr3
    (maximum_window_size, ptr5) <- peekAndOffset ptr4
    (popup_attributes, ptr6) <- peekAndOffset ptr5
    (fullscreen_supported, ptr7) <- peekAndOffset ptr6
    color_table <- peekArray 16 ptr7
    pure (CONSOLE_SCREEN_BUFFER_INFOEX
      size cursor_position attributes window maximum_window_size
      popup_attributes fullscreen_supported color_table)
  poke ptr (CONSOLE_SCREEN_BUFFER_INFOEX
    size cursor_position attributes window maximum_window_size popup_attributes
    fullscreen_supported color_table)
    = do
      ptr0 <- pokeAndOffset (castPtr ptr) sizeCsbix
      ptr1 <- pokeAndOffset ptr0 size
      ptr2 <- pokeAndOffset ptr1 cursor_position
      ptr3 <- pokeAndOffset ptr2 attributes
      ptr4 <- pokeAndOffset ptr3 window
      ptr5 <- pokeAndOffset ptr4 maximum_window_size
      ptr6 <- pokeAndOffset ptr5 popup_attributes
      ptr7 <- pokeAndOffset ptr6 fullscreen_supported
      pokeArray ptr7 color_table'
   where
    color_table' = take 16 $ color_table ++ repeat 0

sizeCsbix :: ULONG
sizeCsbix = fromIntegral $
  sizeOf (undefined :: CONSOLE_SCREEN_BUFFER_INFOEX)

data CHAR_INFO = CHAR_INFO
  { ci_char       :: WCHAR
  , ci_attributes :: WORD
  } deriving (Show)

instance Storable CHAR_INFO where
  sizeOf ~(CHAR_INFO char attributes) = sizeOf char + sizeOf attributes
  alignment ~(CHAR_INFO char _) = alignment char
  peek ptr = do
    (char, ptr') <- peekAndOffset (castPtr ptr)
    attributes <- peek ptr'
    pure (CHAR_INFO char attributes)
  poke ptr (CHAR_INFO char attributes) = do
    ptr' <- pokeAndOffset (castPtr ptr) char
    poke ptr' attributes

kEY_EVENT, mOUSE_EVENT, wINDOW_BUFFER_SIZE_EVENT, mENU_EVENT,
  fOCUS_EVENT :: WORD
kEY_EVENT                =  1
mOUSE_EVENT              =  2
wINDOW_BUFFER_SIZE_EVENT =  4
mENU_EVENT               =  8
fOCUS_EVENT              = 16

foreign import ccall unsafe "windows.h GetNumberOfConsoleInputEvents"
  cGetNumberOfConsoleInputEvents :: HANDLE -> Ptr DWORD -> IO BOOL
foreign import ccall unsafe "windows.h ReadConsoleInputW"
  cReadConsoleInput :: HANDLE
                    -> Ptr INPUT_RECORD
                    -> DWORD
                    -> LPDWORD
                    -> IO BOOL

data ConsoleException = ConsoleException !ErrCode deriving (Eq, Typeable)

instance Show ConsoleException where
  show (ConsoleException 6) =
    "A fatal error has occurred.\n\n" ++
    "An attempt has been made to send console virtual terminal sequences\n" ++
    "(ANSI codes) to an output that has not been recognised as an\n" ++
    "ANSI-capable terminal and also cannot be emulated as an ANSI-enabled\n" ++
    "terminal (emulation needs a ConHost-based terminal, such as Command\n" ++
    "Prompt or PowerShell). That may occur, for example, if output has\n" ++
    "been redirected to a file.\n\n" ++
    "If that is unexpected, please post an issue at:\n" ++
    "https://github.com/UnkindPartition/ansi-terminal/issues\n"
  show (ConsoleException errCode) = "ConsoleException " ++ show errCode

instance Exception ConsoleException

returnWith_ :: Storable a => (Ptr a -> IO b) -> IO a
returnWith_ act = alloca $ \ptr -> act ptr >> peek ptr

{-
typedef union _UNICODE_ASCII_CHAR {
    WCHAR UnicodeChar;
    CHAR  AsciiChar;
} UNICODE_ASCII_CHAR;
-}
newtype UNICODE_ASCII_CHAR = UnicodeAsciiChar
  { unicodeAsciiChar :: WCHAR
  } deriving (Show, Read, Eq)

instance Storable UNICODE_ASCII_CHAR where
  sizeOf _    = 2
  alignment _ = 2
  peek ptr = UnicodeAsciiChar <$> (`peekByteOff` 0) ptr
  poke ptr val = case val of
    UnicodeAsciiChar c -> (`pokeByteOff` 0) ptr c

{-
typedef struct _KEY_EVENT_RECORD {
	BOOL bKeyDown;
	WORD wRepeatCount;
	WORD wVirtualKeyCode;
	WORD wVirtualScanCode;
	union {
		WCHAR UnicodeChar;
		CHAR AsciiChar;
	} uChar;
	DWORD dwControlKeyState;
}
#ifdef __GNUC__
/* gcc's alignment is not what win32 expects */
 PACKED
#endif
KEY_EVENT_RECORD;
-}
data KEY_EVENT_RECORD = KEY_EVENT_RECORD
  { keyEventKeyDown         :: BOOL
  , keyEventRepeatCount     :: WORD
  , keyEventVirtualKeyCode  :: WORD
  , keyEventVirtualScanCode :: WORD
  , keyEventChar            :: UNICODE_ASCII_CHAR
  , keyEventControlKeystate :: DWORD
  } deriving (Show, Read, Eq)

instance Storable KEY_EVENT_RECORD where
  sizeOf _    = 16
  alignment _ =  4
  peek ptr = KEY_EVENT_RECORD <$> (`peekByteOff`  0) ptr
                              <*> (`peekByteOff`  4) ptr
                              <*> (`peekByteOff`  6) ptr
                              <*> (`peekByteOff`  8) ptr
                              <*> (`peekByteOff` 10) ptr
                              <*> (`peekByteOff` 12) ptr
  poke ptr val = do
    (`pokeByteOff`  0) ptr $ keyEventKeyDown val
    (`pokeByteOff`  4) ptr $ keyEventRepeatCount val
    (`pokeByteOff`  6) ptr $ keyEventVirtualKeyCode val
    (`pokeByteOff`  8) ptr $ keyEventVirtualScanCode val
    (`pokeByteOff` 10) ptr $ keyEventChar val
    (`pokeByteOff` 12) ptr $ keyEventControlKeystate val

{-
typedef struct _MOUSE_EVENT_RECORD {
	COORD dwMousePosition;
	DWORD dwButtonState;
	DWORD dwControlKeyState;
	DWORD dwEventFlags;
} MOUSE_EVENT_RECORD;
-}
data MOUSE_EVENT_RECORD = MOUSE_EVENT_RECORD
  { mousePosition        :: COORD
  , mouseButtonState     :: DWORD
  , mouseControlKeyState :: DWORD
  , mouseEventFlags      :: DWORD
  } deriving (Show, Read, Eq)

instance Storable MOUSE_EVENT_RECORD where
  sizeOf _    = 16
  alignment _ =  4
  peek ptr = MOUSE_EVENT_RECORD <$> (`peekByteOff`  0) ptr
                                <*> (`peekByteOff`  4) ptr
                                <*> (`peekByteOff`  8) ptr
                                <*> (`peekByteOff` 12) ptr
  poke ptr val = do
    (`pokeByteOff`  0) ptr $ mousePosition val
    (`pokeByteOff`  4) ptr $ mouseButtonState val
    (`pokeByteOff`  8) ptr $ mouseControlKeyState val
    (`pokeByteOff` 12) ptr $ mouseEventFlags val

{-
typedef struct _WINDOW_BUFFER_SIZE_RECORD {
    COORD dwSize;
} WINDOW_BUFFER_SIZE_RECORD;
-}
data WINDOW_BUFFER_SIZE_RECORD = WINDOW_BUFFER_SIZE_RECORD
  { bufSizeNew :: COORD
  } deriving (Show, Read, Eq)

instance Storable WINDOW_BUFFER_SIZE_RECORD where
  sizeOf _    = 4
  alignment _ = 4
  peek ptr = WINDOW_BUFFER_SIZE_RECORD <$> (`peekByteOff` 0) ptr
  poke ptr val = (`pokeByteOff` 0) ptr $ bufSizeNew val

{-
typedef struct _MENU_EVENT_RECORD {
    UINT dwCommandId;
} MENU_EVENT_RECORD,*PMENU_EVENT_RECORD;
-}
data MENU_EVENT_RECORD = MENU_EVENT_RECORD
  { menuCommandId :: UINT
  } deriving (Show, Read, Eq)

instance Storable MENU_EVENT_RECORD where
  sizeOf _    = 4
  alignment _ = 4
  peek ptr = MENU_EVENT_RECORD <$> (`peekByteOff` 0) ptr
  poke ptr val = (`pokeByteOff` 0) ptr $ menuCommandId val

{-
typedef struct _FOCUS_EVENT_RECORD { BOOL bSetFocus; } FOCUS_EVENT_RECORD;
-}
data FOCUS_EVENT_RECORD = FOCUS_EVENT_RECORD
  { focusSetFocus :: BOOL
  } deriving (Show, Read, Eq)

instance Storable FOCUS_EVENT_RECORD where
  sizeOf _    = 4
  alignment _ = 4
  peek ptr = FOCUS_EVENT_RECORD <$> (`peekByteOff` 0) ptr
  poke ptr val = (`pokeByteOff` 0) ptr $ focusSetFocus val

data INPUT_RECORD_EVENT
  = InputKeyEvent KEY_EVENT_RECORD
  | InputMouseEvent MOUSE_EVENT_RECORD
  | InputWindowBufferSizeEvent WINDOW_BUFFER_SIZE_RECORD
  | InputMenuEvent MENU_EVENT_RECORD
  | InputFocusEvent FOCUS_EVENT_RECORD
  deriving (Show, Read, Eq)

{-
typedef struct _INPUT_RECORD {
	WORD EventType;
	union {
		KEY_EVENT_RECORD KeyEvent;
		MOUSE_EVENT_RECORD MouseEvent;
		WINDOW_BUFFER_SIZE_RECORD WindowBufferSizeEvent;
		MENU_EVENT_RECORD MenuEvent;
		FOCUS_EVENT_RECORD FocusEvent;
	} Event;
} INPUT_RECORD,*PINPUT_RECORD;
-}
data INPUT_RECORD = INPUT_RECORD
  { inputEventType :: WORD
  , inputEvent     :: INPUT_RECORD_EVENT
  } deriving (Show, Read, Eq)

instance Storable INPUT_RECORD where
  sizeOf _    = 20
  alignment _ =  4
  peek ptr = do
    evType <- (`peekByteOff` 0) ptr
    event <- case evType of
      _ | evType == kEY_EVENT
          -> InputKeyEvent              <$> (`peekByteOff` 4) ptr
      _ | evType == mOUSE_EVENT
          -> InputMouseEvent            <$> (`peekByteOff` 4) ptr
      _ | evType == wINDOW_BUFFER_SIZE_EVENT
          -> InputWindowBufferSizeEvent <$> (`peekByteOff` 4) ptr
      _ | evType == mENU_EVENT
          -> InputMenuEvent             <$> (`peekByteOff` 4) ptr
      _ | evType == fOCUS_EVENT
          -> InputFocusEvent            <$> (`peekByteOff` 4) ptr
      _ -> error $ "peek (INPUT_RECORD): Unknown event type " ++
             show evType
    pure $ INPUT_RECORD evType event
  poke ptr val = do
    (`pokeByteOff` 0) ptr $ inputEventType val
    case inputEvent val of
      InputKeyEvent              ev -> (`pokeByteOff` 4) ptr ev
      InputMouseEvent            ev -> (`pokeByteOff` 4) ptr ev
      InputWindowBufferSizeEvent ev -> (`pokeByteOff` 4) ptr ev
      InputMenuEvent             ev -> (`pokeByteOff` 4) ptr ev
      InputFocusEvent            ev -> (`pokeByteOff` 4) ptr ev

-- The following is based on module System.Win32.Console.Extra from package
-- Win32-console.

getNumberOfConsoleInputEvents :: HANDLE -> IO DWORD
getNumberOfConsoleInputEvents hdl =
  returnWith_ $ \ptrN ->
    failIfFalse_ "GetNumberOfConsoleInputEvents" $
      cGetNumberOfConsoleInputEvents hdl ptrN

-- The following is based on module System.Win32.Console.Extra from package
-- Win32-console, cut down for the WCHAR version of readConsoleInput.

readConsoleInput :: HANDLE -> DWORD -> IO [INPUT_RECORD]
readConsoleInput hdl len
  = readConsoleInputWith hdl len $ \(ptr, n) -> peekArray (fromEnum n) ptr

readConsoleInputWith :: HANDLE
                     -> DWORD
                     -> OutputHandler (Ptr INPUT_RECORD, DWORD)
readConsoleInputWith hdl len handler =
  allocaArray (fromEnum len) $ \ptrBuf ->
    alloca $ \ptrN -> do
      failIfFalse_ "ReadConsoleInputW" $
        cReadConsoleInput hdl ptrBuf len ptrN
      n <- peek ptrN
      handler (ptrBuf, n)

type OutputHandler o = forall a. (o -> IO a) -> IO a

-- Replicated from module Foreign.C.String in package base because that module
-- does not export the function.
cWcharsToChars :: [CWchar] -> [Char]
cWcharsToChars = map chr . fromUTF16 . map fromIntegral
 where
  fromUTF16 (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c:wcs) = c : fromUTF16 wcs
  fromUTF16 [] = []
