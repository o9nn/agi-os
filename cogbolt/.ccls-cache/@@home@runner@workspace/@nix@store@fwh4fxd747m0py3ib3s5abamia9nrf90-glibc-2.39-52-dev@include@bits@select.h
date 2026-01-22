#ifndef _SYS_SELECT_H
# error "Never use <bits/select.h> directly; include <sys/select.h> instead."
#endif
#define __FD_ZERO(s) \
do {									      \
unsigned int __i;							      \
fd_set *__arr = (s);						      \
for (__i = 0; __i < sizeof (fd_set) / sizeof (__fd_mask); ++__i)	      \
__FDS_BITS (__arr)[__i] = 0;					      \
} while (0)
#define __FD_SET(d, s) \
((void) (__FDS_BITS (s)[__FD_ELT(d)] |= __FD_MASK(d)))
#define __FD_CLR(d, s) \
((void) (__FDS_BITS (s)[__FD_ELT(d)] &= ~__FD_MASK(d)))
#define __FD_ISSET(d, s) \
((__FDS_BITS (s)[__FD_ELT (d)] & __FD_MASK (d)) != 0)