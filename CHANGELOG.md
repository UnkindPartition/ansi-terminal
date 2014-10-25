Changes
=======

Version 0.6.2
---------------

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
