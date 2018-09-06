The package aims:
* to have limited dependency on other packages, other than those included with
  GHC; and
* to have a high degree of backwards compatibility with earlier versions of GHC.

As of `ansi-terminal-0.8.0.4`, the package aims to be compatible with versions
of GHC from GHC 7.0.1 (released November 2010). GHC 7.0.1 comes with:
* `base-4.3.0.0`
* `containers-0.4.0.0` (used for its `Data.Map.Strict.Map` and only in the
  Windows version of the package)
* `Win32-2.2.0.1` (only on Windows)

The package also depends on:
* `colour`, used for 24-bit colour (`Colour`, `RGB`, `toSRGB` and `toSRGB24`)
* `mintty` (which depends on `base >= 4.3`), used for its `isMinTTYHandle`
  function only in the Windows version of the package. The function was later
  added to `Win32-2.5.0.0` (first included with GHC 8.2.1 with `base-4.10.0.0`)

The package achieves that backward compatibility by using C Pre-Processor (CPP)
directives. The `CPP` GHC extension is specified for the library in
`ansi-terminal.cabal`. The `Win32` package has developed over time and module
`System.Win32.Compat` is used to reduce the use of CPP pragmas in other modules.

Of particular note is that, before GHC 7.10.1 and `base-4.8.0.0`, `<$>` was not
exported by the `Prelude`.

CPP `#include` pragmas are also used to include code from files to limit code
duplication in modules that are specific to the 'Unix' or Windows versions of
the package. The common code is located in folder `src\includes`.
