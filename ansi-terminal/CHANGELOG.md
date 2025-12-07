Changes
=======

Version 1.1.4
-------------

* Fixes `hGetCursorPosition` and `hGetLayerColor` on Windows, allowing time for
  reported information to reach the standard input channel.
* Improvements to Haddock documentation.
* The executable `ansi-terminal-example` allows individual examples to be
  selected.

Version 1.1.3
-------------

* Module `System.Console.ANSI.Codes` spun out to dependency package
  `ansi-terminal-types-1.1.3`.

Version 1.1.2
-------------

* Add `enableLineWrap` and `disableLineWrap`, and support for enabling and
 disabling automatic line wrapping.

Version 1.1.1
-------------

* Use of C function `setenv()` in one thread can cause other threads calling C
  functions that query environment variables to crash. On Unix-like operating
  systems, where such functions are used, the implementations of
  `hSupportsANSI` and `hSupportsANSIColor` now avoid unnecessary calls of
  `System.Environment.lookupEnv`.

* Improvements to Haddock documentation.

Version 1.1
-----------

* Add support for console layer `Underlining` and underlining styles
  `CurlyUnderline`, `DottedUnderline` and `DashedUnderline`.
* Add type synonyms `Parameter`, `SubParam`, and `ParamWithSubs` to represent
  SGR parameter values with and without following parameter substrings comprised
  of one or more parameter elements (including empty elements).
* Add `csi'` and `sgrToCode'`, corresponding to `csi` and `sgrToCode` but
  capable of handling a parameter value followed by a parameter substring.

Version 1.0.2
-------------

* On Windows, fix linker error about a duplicate symbol `castUINTPtrToPtr`.

Version 1.0.1
-------------

* On Windows, the processing of \'ANSI\' control characters in output is enabled
  by default in Windows Terminal but is not enabled by default in ConHost
  terminals. Additions have been made to allow support of users of ConHost
  terminals.
* Add `hNowSupportsANSI`. On Unix, the function is equivalent to
  `hSupportsANSI`. On Windows, in Windows Terminal and ConHost terminals, the
  action can try to enable the processing of \'ANSI\' control characters in
  output.
* In Windows Terminal and ConHost terminals, `hSupportsANSI` will yield `False`
  if the processing of \'ANSI\' control characters in output is not enabled.
* Deprecated `hSupportsANSIWithoutEmulation` is now consistent with
  `hNowSupportsANSI`.
* Improvements to Haddock documentation.
Version 1.0
-----------

* On Windows, drop support for legacy Windows requiring emulation. The package
  assumes Windows Terminal has replaced ConHost terminals on supported versions
  of Windows. Functions that yield actions no longer enable (re-enable) the
  processing of \'ANSI\' control characters in output.
* On Windows, the package no longer depends (directly or indirectly) on the
  `Win32`, `array`,`containers`, `deepseq`, `filepath`, `ghc-boot-th`, `mintty`,
  `pretty` or `template-haskell` packages.
* `hSupportsANSI` no longer assumes that the given handle is writeable.
* `hSupportsANSIWithoutEmulation` is deprecated.

Version 0.11.5
--------------

* Module `System.Console.ANSI.Types` spun out to new dependency package
  `ansi-terminal-types-0.11.5`.
* Drop support for GHC versions before GHC 7.10.1 (released March 2015).
* Improvements to Haddock documentation.

Version 0.11.4
--------------

* Add `reportLayerColor`, `getReportedLayerColor` and `getLayerColor` for
  querying the layer color on terminals that support the functionality.
* Add `useAlternateScreenBuffer` and `useNormalScreenBuffer`, and support for
  switching between the Alternate and Normal Screen Buffers.
* When the argument is `0`, `cursorUpCode`, `cursorDownCode`,
  `cursorForwardCode`, `cursorBackwardCode`,`scrollPageUpCode` and
  `scrollPageDownCode` now yield `""`, and `cursorUpLineCode` and
  `cursorDownLineCode` now yield the equivalent of `setCursorColumnCode 0`. This
  is because, on some terminals, a `0` parameter for the underlying 'ANSI' code
  specifies a default parameter of `1`.
* Add `osc` as a utility function, for OSC sequences.
* `setTitle` now uses the recommended STRING TERMINATOR (ST) of `\ESC\\`, rather
  than the legacy `\BEL` (`\007`), and filters the title of all non-printable
  characters, not just `\BEL`.
* Improvements to Haddock documentation.

Version 0.11.3
--------------

* Add `hyperlink`, `hyperlinkWithId` and `hyperlinkWithParams`, and support for
  clickable hyperlinks.

Version 0.11.2
--------------

* On Windows, fix compatability with the Windows I/O Manager (WinIO) when
  GHC >= 9.0.1 but `Win32` < 2.9.0.0.
* Improvements to Haddock documentation.

Version 0.11.1
--------------

* On Windows, fix compatability with the Windows I/O Manager (WinIO) introduced
  in GHC 9.0.1, by incorporating changes made in package `Win32-2.13.2.0` in
  that regard.
* Improvements to Haddock documentation.

Version 0.11
------------

* Remove deprecated `getCursorPosition0`. (Use `getCursorPosition` instead.)
* On Unix-like operating systems, the temporary turning off of echoing is moved
  from `getReportedCursorPosition` to `hGetCursorPositon`.
* On Unix-like operating systems, fix a bug in `getCursorPosition` and
  `hGetCursorPosition`, where the console input stream was was not always
  clear before the cursor position was emitted into it.

Version 0.10.3
--------------

* Add `getCursorPosition` as a synonym of `getCursorPosition0` and deprecate the
  latter.

Version 0.10.2
--------------

* `hGetTerminalSize` now assumes a terminal is no bigger than 9,999 by 9,999
  (previously, no bigger than 999 by 999).
* On Windows, fix a bug where emulated cursor movement functions differed from
  Windows 10 (movement bounded by the current viewport).

Version 0.10.1
--------------

* Add `hGetCursorPosition` and `hGetTerminalSize`.
* On Unix-like operating systems, fix a bug where `getReportedCursorPosition`
  could block indefinitely if no information was forthcoming on the console
  input stream.
* Improvements to Haddock documentation.

Version 0.10
------------

* Add support for setting the default color with new `SetDefaultColor`
  constructor of the `SGR` type.
* `getTerminalSize` now flushes the `stdout` channel, to ensure the cursor
  position is unaffected.

Version 0.9.1
-------------

* Flag modules with GHC's 'Safe Haskell' language extensions (from GHC 7.2.1).
* Improvements and corrections to Haddock documentation.

Version 0.9
-----------

* Add support for 256-color palettes with new `SetPaletteColor` constructor of
  the `SGR` type, and `xterm6LevelRGB`, `xterm24LevelGray` and `xtermSystem`.
* Remove deprecated `getCursorPosition`. (Use `getCursorPosition0` instead.)
* Add `hSupportsANSIColor`.
* Add `getTerminalSize`.
* Improvements to Haddock documentation.

Version 0.8.2
-------------

* Add `getCursorPosition0` and deprecate `getCursorPosition`. Any position
  provided by the latter is 1-based. Any position provided by the former is
  0-based, consistent with `setCursorColumn` and `setCursorPosition`.
* Improvements to Haddock documentation in respect of 0-based and 1-based
  cursor positions.

Version 0.8.1
-------------

* Add `hSupportsANSIWithoutEmulation`. On Windows 10, if the handle is
  identified as connected to a native terminal ('Command Prompt' or
  'PowerShell'), the processing of 'ANSI' control characters will be enabled.

Version 0.8.0.4
---------------

* On Windows, `hSupportsANSI` now recognises if the handle is connected to a
  'mintty' terminal.
* Drop support for GHC versions before GHC 7.0.1 (released November 2010)

Version 0.8.0.3
---------------

* On Windows, try to enable ANSI on ConHost terminals even if a TERM environment
  variable exits (such as with the Hyper 2 terminal)
* Minor improvements to Haddock documentation

Version 0.8.0.2
---------------

* Improve README and Haddock documentation
* On Windows, fix compatability with earlier GHC versions
* Drop support for GHC versions before 6.12.1 (released December 2009)

Version 0.8.0.1
---------------

* On Windows, if the standard output channel is valid but not a ConHost
  terminal, assume it is ANSI-enabled rather than failing
* On Windows, output the improved error message to the standard error channel
  rather than the standard output channel

Version 0.8
-----------

* Make the fields of `SGR` strict
* Make compatible with GHC 8.2.2
* Improve the error message on Windows when not ANSI-capable or ConHost
* Recognise Appveyor build environment as ANSI-enabled

Version 0.7.1.1
---------------

`getReportedCursorPosition`: don't let the cursor reporting code be echo'd

Version 0.7.1
-------------

* Allow saving, restoring, and querying the current cursor position
* Fix a couple of issues with the Reset emulation on Windows

Version 0.7
-----------

Add 24-bit RGB color support

Version 0.6.3.1
---------------

Fix Windows + ghc 7.8 compatibility

Version 0.6.3
-------------

* Add ANSI support for Windows
* Add compatibility with Win32-2.5.0.0 and above

Version 0.6.2.3
---------------

Add an example to the haddocks

Version 0.6.2.2
---------------

Fix a GHC 7.10 warning

Version 0.6.2.1
---------------

Restore compatibility with GHC 7.4 and older

Version 0.6.2
-------------

* Add `hSupportsANSI`
* Drop support for `base < 4`

Version 0.6.1.1
---------------

Fix to build with GHC 7.8 on Windows

Version 0.6.1
-------------

* `BoldIntensity` no longer changes background color on Windows
* `setSGR []` was not equivalent to `setSGR [Reset]` on Windows, even though it
  should be according to the documentation. This is now fixed.
