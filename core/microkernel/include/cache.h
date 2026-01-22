#ifndef _MACH_CACHE_H_
#define _MACH_CACHE_H_
#define __cacheline_aligned __attribute__((aligned(1 << CPU_L1_SHIFT)))
#endif