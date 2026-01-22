#ifndef _KERN_LOG2_H
#define _KERN_LOG2_H
#include <kern/assert.h>
#ifdef __LP64__
#define LONG_BIT 64
#else
#define LONG_BIT 32
#endif
static inline unsigned int
ilog2(unsigned long x)
{
assert(x != 0);
return LONG_BIT - __builtin_clzl(x) - 1;
}
static inline unsigned int
iorder2(unsigned long size)
{
assert(size != 0);
if (size == 1)
return 0;
return ilog2(size - 1) + 1;
}
#endif