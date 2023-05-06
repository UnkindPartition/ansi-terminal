#define UNICODE
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include "errors.h"

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
 *
 * Why the concern about performance? Error messages are only generated
 * in exceptional situations    -- sof 9/98
 *
 * sof 9/98 : Removed use of non-standard (and wimpy :-) snprintf().
 */

LPTSTR _ansi_terminal_getErrorMessage(DWORD err)
{
    LPTSTR what;

    FormatMessage(
	(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER) ,
	NULL,
	err,
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
	(LPTSTR) &what,
	0,
	NULL
	);
    return what;
}
