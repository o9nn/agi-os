#ifndef _MACH_SA_STDARG_H_
#define _MACH_SA_STDARG_H_
#if __GNUC__ >= 3
typedef __builtin_va_list va_list;
#define va_start(v,l) __builtin_va_start(v,l)
#define va_end(v) __builtin_va_end(v)
#define va_arg(v,l) __builtin_va_arg(v,l)
#else
#define __va_size(type) ((sizeof(type)+sizeof(unsigned long)-1) & ~(sizeof(unsigned long)-1))
#ifndef _VA_LIST_
#define _VA_LIST_
typedef char *va_list;
#endif
#define va_start(pvar, lastarg) \
((pvar) = (char*)(void*)&(lastarg) + __va_size(lastarg))
#define va_end(pvar)
#define va_arg(pvar,type) \
((pvar) += __va_size(type), \
*((type *)((pvar) - __va_size(type))))
#endif
#endif