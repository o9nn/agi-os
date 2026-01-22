#ifndef _HACK_ASM_SYSTEM_H
#define _HACK_ASM_SYSTEM_H
#include <stdint.h>
#define xchg(ptr, x)							      \
({									      \
__typeof__ (*(ptr)) *_ptr = (ptr), _x = *_ptr;			      \
*_ptr = (x); _x;							      \
})
#define mb()	((void) 0)
#define rmb()	mb()
#define wmb()	mb()
#endif