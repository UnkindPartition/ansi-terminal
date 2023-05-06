#ifndef _ANSI_TERMINAL_ERRORS_H
#define _ANSI_TERMINAL_ERRORS_H

#include <windows.h>

/* Copied from the Win32-2.13.4.0 package, but renamed `getErrorMessage` to
 * `_ansi_terminal_getErrorMessage`, in order to avoid problems with duplicate
 * symbols in GHC's object files. See:
 * https://gitlab.haskell.org/ghc/ghc/-/issues/23365.
 */

/* There's two ways we can generate error messages - with different tradeoffs:
 * If we do a function call, we have to use a static buffer.
 * If we use a macro and ANSI C's string splicing, we have to use constant
 * strings - and accept a certain amount of overhead from inserting the
 * boilerplate text.
 */

/* result should be freed using LocalFree */
extern LPTSTR _ansi_terminal_getErrorMessage(DWORD err);

#endif /* _ANSI_TERMINAL_ERRORS_H */
