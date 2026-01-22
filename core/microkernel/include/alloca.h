#ifndef _MACH_ALLOCA_H_
#define _MACH_ALLOCA_H_
#define alloca(size) __builtin_alloca(size)
#endif