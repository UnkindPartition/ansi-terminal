#ifndef __HSWIN32_H
#define __HSWIN32_H

#define UNICODE
#include <windows.h>

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE extern inline
# endif
#endif

INLINE void *castUINTPtrToPtr(UINT_PTR n) { return (void *)n; }

#endif /* __HSWIN32_H */
