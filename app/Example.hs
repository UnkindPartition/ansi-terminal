module Main
  (
    main
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import System.IO (hFlush, stdout)

import System.Console.ANSI

examples :: [IO ()]
examples = [ cursorMovementExample
           , lineChangeExample
           , setCursorPositionExample
           , saveRestoreCursorExample
           , clearExample
           , scrollExample
           , sgrColorExample
           , sgrOtherExample
           , cursorVisibilityExample
           , titleExample
           , getCursorPositionExample
           ]

main :: IO ()
main = mapM_ (\example -> resetScreen >> example) examples

-- Annex D to Standard ECMA-48 (5th Ed, 1991) identifies that the representation
-- of an erased state is implementation-dependent. There may or may not be a
-- distinction between a character position in the erased state and one imaging
-- SPACE. Consequently, to reset the screen, the default graphic rendition must
-- be selected (setSGR [Reset]) before all character positions are put into the
-- erased state (clearScreen).
resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

pause :: IO ()
pause = do
  hFlush stdout
  -- 1 second pause
  threadDelay 1000000

cursorMovementExample :: IO ()
cursorMovementExample = do
  putStrLn "Line One"
  putStr "Line Two"
  pause
  -- Line One
  -- Line Two

  cursorUp 1
  putStr " - Extras"
  pause
  -- Line One - Extras
  -- Line Two

  cursorBackward 2
  putStr "zz"
  pause
  -- Line One - Extrzz
  -- Line Two

  cursorForward 2
  putStr "- And More"
  pause
  -- Line One - Extrzz  - And More
  -- Line Two

  cursorDown 1
  putStr "Disconnected"
  pause
  -- Line One - Extrzz  - And More
  -- Line Two                     Disconnected

lineChangeExample :: IO ()
lineChangeExample = do
  putStrLn "Line One"
  putStr "Line Two"
  pause
  -- Line One
  -- Line Two

  cursorUpLine 1
  putStr "New Line One"
  pause
  -- New Line One
  -- Line Two

  cursorDownLine 1
  putStr "New Line Two"
  pause
  -- New Line One
  -- New Line Two

setCursorPositionExample :: IO ()
setCursorPositionExample = do
  putStrLn "Line One"
  putStrLn "Line Two"
  pause
  -- Line One
  -- Line Two

  setCursorPosition 0 5
  putStr "Foo"
  pause
  -- Line Foo
  -- Line Two

  setCursorPosition 1 5
  putStr "Bar"
  pause
  -- Line Foo
  -- Line Bar

  setCursorColumn 1
  putStr "oaf"
  pause
  -- Line Foo
  -- Loaf Bar

saveRestoreCursorExample :: IO ()
saveRestoreCursorExample = do
  putStr "Start sentence ..."
  pause
  -- Start sentence ...

  saveCursor
  setCursorPosition 2 3
  putStr "SPLASH!"
  pause
  -- Start sentence ...
  --
  --    SPLASH!

  restoreCursor
  putStr " end sentence, uninterrupted."
  pause
  -- Start sentence ... end sentence, uninterrupted
  --
  --    SPLASH!

clearExample :: IO ()
clearExample = do
  putStrLn "Line One"
  putStrLn "Line Two"
  pause
  -- Line One
  -- Line Two

  setCursorPosition 0 4
  clearFromCursorToScreenEnd
  pause
  -- Line

  resetScreen
  putStrLn "Line One"
  putStrLn "Line Two"
  pause
  -- Line One
  -- Line Two

  setCursorPosition 1 4
  clearFromCursorToScreenBeginning
  pause
  --
  --     Two

  resetScreen
  putStrLn "Line One"
  putStrLn "Line Two"
  pause
  -- Line One
  -- Line Two

  setCursorPosition 0 4
  clearFromCursorToLineEnd
  pause
  -- Line
  -- Line Two

  setCursorPosition 1 4
  clearFromCursorToLineBeginning
  pause
  -- Line
  --      Two

  clearLine
  pause
  -- Line

  clearScreen
  pause
  --

scrollExample :: IO ()
scrollExample = do
  putStrLn "Line One"
  putStrLn "Line Two"
  putStrLn "Line Three"
  pause
  -- Line One
  -- Line Two
  -- Line Three

  scrollPageDown 2
  pause
  --
  --
  -- Line One
  -- Line Two
  -- Line Three

  scrollPageUp 3
  pause
  -- Line Two
  -- Line Three

sgrColorExample :: IO ()
sgrColorExample = do
  let colors = enumFromTo minBound maxBound :: [Color]
  forM_ [Foreground, Background] $ \layer ->  do
    forM_ [Dull, Vivid] $ \intensity -> do
      resetScreen
      forM_ colors $ \color -> do
        setSGR [Reset]
        setSGR [SetColor layer intensity color]
        putStrLn (show color)
      pause
  -- All the colors, 4 times in sequence

sgrOtherExample :: IO ()
sgrOtherExample = do
  let named_styles = [ (SetConsoleIntensity BoldIntensity, "Bold")
                     , (SetConsoleIntensity FaintIntensity, "Faint")
                     , (SetConsoleIntensity NormalIntensity, "Normal")
                     , (SetItalicized True, "Italic")
                     , (SetItalicized False, "No Italics")
                     , (SetUnderlining SingleUnderline, "Single Underline")
                     , (SetUnderlining DoubleUnderline, "Double Underline")
                     , (SetUnderlining NoUnderline, "No Underline")
                     , (SetBlinkSpeed SlowBlink, "Slow Blink")
                     , (SetBlinkSpeed RapidBlink, "Rapid Blink")
                     , (SetBlinkSpeed NoBlink, "No Blink")
                     , (SetVisible False, "Conceal")
                     , (SetVisible True, "Reveal")
                     ]
  forM_ named_styles $ \(style, name) -> do
    resetScreen
    setSGR [style]
    putStrLn name
    pause
  -- Text describing a style displayed in that style in sequence

  setSGR [SetColor Foreground Vivid Red]
  setSGR [SetColor Background Vivid Blue]

  clearScreen >> setCursorPosition 0 0
  setSGR [SetSwapForegroundBackground False]
  putStr "Red-On-Blue"
  pause
  -- Red-On-Blue

  clearScreen >> setCursorPosition 0 0
  setSGR [SetSwapForegroundBackground True]
  putStr "Blue-On-Red"
  pause
  -- Blue-On-Red

cursorVisibilityExample :: IO ()
cursorVisibilityExample = do
  putStr "Cursor Demo"
  pause
  -- Cursor Demo|

  hideCursor
  pause
  -- Cursor Demo

  showCursor
  pause
  -- Cursor Demo|

titleExample :: IO ()
titleExample = do
  putStr "Title Demo"
  pause
  -- ~/foo/ - ansi-terminal-ex - 83x70
  ------------------------------------
  -- Title Demo

  setTitle "Yup, I'm a new title!"
  pause
  -- Yup, I'm a new title! - ansi-terminal-ex - 83x70
  ---------------------------------------------------
  -- Title Demo

getCursorPositionExample :: IO ()
getCursorPositionExample = do
  putStrLn "         11111111112222222222"
  putStrLn "12345678901234567890123456789"
  putStr   "Report cursor position here:"
  pause
  --          11111111112222222222
  -- 12345678901234567890123456789
  -- Report cursor position here:|
  result <- getCursorPosition0
  putStrLn " (3rd row, 29th column) to stdin, as CSI 3 ; 29 R.\n"
  case result of
    Just (row, col) -> putStrLn $ "The cursor was at row number " ++
      show (row + 1) ++ " and column number " ++ show (col + 1) ++ ".\n"
    Nothing -> putStrLn "Error: unable to get the cursor position\n"
  pause
  --          11111111112222222222
  -- 12345678901234567890123456789
  -- Report cursor position here: (3rd row, 29th column) to stdin, as CSI 3 ; 29 R.
  --
  -- The cursor was at row number 3 and column number 29.
