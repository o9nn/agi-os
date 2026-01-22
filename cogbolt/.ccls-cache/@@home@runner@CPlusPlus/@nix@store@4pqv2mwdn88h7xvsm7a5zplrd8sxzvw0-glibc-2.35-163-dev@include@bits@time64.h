#ifndef _BITS_TYPES_H
# error "Never include <bits/time64.h> directly; use <sys/types.h> instead."
#endif
#ifndef _BITS_TIME64_H
#define _BITS_TIME64_H 1
#if __TIMESIZE == 64
# define __TIME64_T_TYPE __TIME_T_TYPE
#else
# define __TIME64_T_TYPE __SQUAD_TYPE
#endif
#endif