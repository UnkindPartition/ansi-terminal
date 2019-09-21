Many other packages depend on `ansi-terminal`. As at May 2019, over 190 packages
on [Hackage](https:https://hackage.haskell.org/) depend on it. Consider
raising an [issue](https://github.com/feuerbach/ansi-terminal/issues) to
discuss a proposed change before making a pull request.

The package aims to have:
* limited dependency on other packages, other than those included with
  GHC;
* a high degree of backwards compatibility with earlier versions of GHC; and
* comprehensive and high-quality Haddock documentation.

As of `ansi-terminal-0.8.0.4`, the package aims to be compatible with versions
of GHC from GHC 7.0.1 (released November 2010). GHC 7.0.1 comes with:
* `base-4.3.0.0`
* `containers-0.4.0.0` (used for its `Data.Map.Strict.Map` and only in the
  Windows version of the package)
* `Win32-2.2.0.1` (only on Windows)

The package also depends on:
* `colour-2.1.0` or later, used for 24-bit colour (`Colour`, `RGB`, `toSRGB` and
  `toSRGB24`)
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

Separate 'Unix' and Windows versions of the package exist because, before
Windows 10 version 1511, the native terminal software on Windows did not support
the control sequences and emulation was required. Microsoft has not supported
Windows XP since 8 April 2014 (and GHC has not supported Windows XP since
GHC 7.10.3 of December 2015); Windows Vista since 11 April 2017; Windows 7,
service pack 1 since 13 January 2015 (mainstream; extended support ends
14 January 2020); Windows 8.1 since 9 January 2018 (mainstream; extended support
ends 10 January 2023); and Windows 10, version 1507 (which 1511 of December 2015
updated) since 9 May 2017. Consequently, currently, there is no version of
Windows that both has mainstream support by Microsoft and needs emulation. This
may make changes to the emulation difficult to test.

The package uses GHC's 'Safe Haskell' language extensions `Trustworthy`
(introduced in GHC 7.2.1) and `Safe` (introduced in GHC 7.2.1 but not stable
until GHC 7.4.1) to add flags explicitly to all modules. For most modules, that
is done using a CPP pragma: `#include "Common-Safe-Haskell.hs"`. Modules
`System.Console.ANSI.Windows.Detect` and `System.Console.ANSI.Windows.Emulator`
use `System.IO.Unsafe.unsafePerformIO` but are flagged `Trustworthy`.

The source code generally follows
[Johan Tibell's style guide](https://github.com/tibbe/haskell-style-guide), but
with code blocks indented with 2 spaces (rather than 4 spaces).

Pull requests should be organised into logical commits with useful commit
messages. There is no need to change the package version number or update
`CHANGELOG.md`. This will be done by the package maintainers when a further
version is published on Hackage.
