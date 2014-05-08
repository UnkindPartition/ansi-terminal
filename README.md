ansi-terminal
=============

Haskell ANSI Terminal Package For Windows, OS X and Linux

Description
-----------

[ANSI](http://en.wikipedia.org/wiki/ANSI_escape_sequences) terminal
support for Haskell, which allows:

-   Cursor movement
-   Screen and line clearing
-   Color output
-   Showing or hiding the cursor
-   Changing the console title (though this is not strictly part of
    ANSI, it is widely supported in Unix)

It is compatible with Windows (via an emulation layer) and those Unixes
with ANSI terminals.

If you like this, you may be interested in
[ansi-wl-pprint](http://github.com/batterseapower/ansi-wl-pprint), which
provides a pretty-printer that can construct strings containing ANSI
colorisation.

Not all of the ANSI escape codes are provided by this module, but most
(if not all) of the popular and well supported ones are. For a full
list, have a look at the [current version of the
API](http://github.com/feuerbach/ansi-terminal/tree/master/includes/Common-Include.hs).
Each supported escape code or family of codes has a corresponding
function that comes in three variants:

-   A straight `IO` variant that doesn't take a `Handle` and just
    applies the ANSI escape code to the terminal attached to stdout
-   An `IO` variant similar to above, but which takes a `Handle` to
    which the ANSI escape should be applied
-   A `String` variant that returns a literal string that should be
    included to get the effect of the code. This is the only one of the
    three API variants that only works on Unix-like operating systems:
    on Windows these strings will always be blank!

Example
-------

A full example is
[available](http://github.com/feuerbach/ansi-terminal/tree/master/System/Console/ANSI/Example.hs),
but for a taste of how the library works try the following code:

``` haskell
import System.Console.ANSI

main = do
    setCursorPosition 5 0
    setTitle "ANSI Terminal Short Example"

    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Red
           ]
    putStr "Hello"

    setSGR [ SetConsoleIntensity NormalIntensity
           , SetColor Foreground Vivid White
           , SetColor Background Dull Blue
           ]
    putStrLn "World!"
```

![](https://raw.githubusercontent.com/feuerbach/ansi-terminal/master/example.png)

Documentation
-------------

Haddock documentation is [available at
Hackage](http://hackage.haskell.org/packages/archive/ansi-terminal/latest/doc/html/System-Console-ANSI.html).

Credits
-------

The library is originally written by [Max Bolingbroke](https://github.com/batterseapower)

Maintainers
-----------

[Roman Cheplyaka](https://github.com/feuerbach) is the primary maintainer.

[Oliver Charles](https://github.com/ocharles) is the backup maintainer. Please
get in touch with him if the primary maintainer cannot be reached.
