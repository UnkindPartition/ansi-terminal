Many other packages depend on `ansi-terminal`. As at February 2024, over 4,700
packages on [Hackage](https:https://hackage.haskell.org/) depend on it, directly
or indirectly. Consider raising an
[issue](https://github.com/UnkindPartition/ansi-terminal/issues) to
discuss a proposed change before making a pull request.

The `ansi-terminal` and `ansi-terminal-types` packages aim to have:
* limited dependency on other packages, even those included with GHC;
* a high degree of backwards compatibility with earlier versions of GHC; and
* comprehensive and high-quality Haddock documentation.

As of `ansi-terminal-1.0.1`, the package aims to be compatible with versions
of GHC from GHC 7.10.1 (released March 2015). GHC 7.10.1 comes with:
* `base-4.8.0.0`

The `ansi-terminal` package also depends on:
* `ansi-terminal-types`, which exposes module `System.Console.ANSI.Types`. The
  package is provided to avoid circular dependencies in some circumstances; and
* `colour-2.1.0` or later, used for 24-bit colour (`Colour`, `RGB`, `toSRGB` and
  `toSRGB24`).

The `ansi-terminal` package aims to avoid a dependency on the `Win32` package,
because of its dependency on the `filepath` package. It does that by reproducing
the small part of the `Win32` and `mintty` packages on which it would otherwise
rely.

The `ansi-terminal` and `ansi-terminal-types` packages achieve that backward
compatibility by using C Pre-Processor (CPP) directives. The `CPP` GHC extension
is specified for the libraries in the Cabal files.

Separate 'Unix' and Windows versions of the package used to exist because,
before Windows 10 version 1511, the native terminal software on Windows did not
support the control sequences and emulation was required. Microsoft has not
supported Windows XP since 8 April 2014 (and GHC has not supported Windows XP
from GHC 8.0.1 of May 2016); Windows Server 2003 since 14 July 2015; Windows
Vista since 11 April 2017; Windows 7, service pack 1 since 10 January 2023;
Windows 8.1 since 10 January 2023; and Windows 10, version 1507 (which 1511 of
December 2015 updated) since 9 May 2017. Consequently, there is no longer any
version of Windows that both has support by Microsoft and needs emulation.

The package uses GHC's 'Safe Haskell' language extension `Safe` (introduced in
GHC 7.2.1 but not stable until GHC 7.4.1) to add flags explicitly to all
modules.

The source code generally follows
[Johan Tibell's style guide](https://github.com/tibbe/haskell-style-guide), but
with code blocks indented with 2 spaces (rather than 4 spaces).

Pull requests should be organised into logical commits with useful commit
messages. There is no need to change the package version number or update
`CHANGELOG.md`. This will be done by the package maintainers when a further
version is published on Hackage.
