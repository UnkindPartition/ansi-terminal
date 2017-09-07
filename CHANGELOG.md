Changes
=======

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
