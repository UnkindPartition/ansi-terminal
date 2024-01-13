#ifndef _ANSI_TERMINAL_HSWIN32_H
#define _ANSI_TERMINAL_HSWIN32_H

#define UNICODE
#include <windows.h>

/* Copied from the Win32-2.13.4.0 package, but renamed `castUINTPtrToPtr` to
 * `_ansi_terminal_castUINTPtrToPtr`, in order to avoid problems with duplicate
 * symbols in GHC's object files. See:
 * https://gitlab.haskell.org/ghc/ghc/-/issues/23365.
 */

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE extern inline
# endif
#endif

INLINE void *_ansi_terminal_castUINTPtrToPtr(UINT_PTR n) { return (void *)n; }

#endif /* _ANSI_TERMINAL_HSWIN32_H */
