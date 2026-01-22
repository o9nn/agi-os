#ifndef _KERN_MACROS_H
#define _KERN_MACROS_H
#define MACRO_BEGIN ({
#define MACRO_END })
#define MACRO_RETURN if (1) return
#define __QUOTE(x) #x
#define QUOTE(x) __QUOTE(x)
#ifdef __ASSEMBLER__
#define DECL_CONST(x, s) x
#else
#define __DECL_CONST(x, s) x##s
#define DECL_CONST(x, s) __DECL_CONST(x, s)
#endif
#define STRLEN(x) (sizeof(x) - 1)
#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define DIV_CEIL(n, d) (((n) + (d) - 1) / (d))
#define P2ALIGNED(x, a) (((x) & ((a) - 1)) == 0)
#define ISP2(x) P2ALIGNED(x, x)
#define P2ALIGN(x, a) ((x) & -(a))
#define P2ROUND(x, a) (-(-(x) & -(a)))
#define P2END(x, a) (-(~(x) & -(a)))
#define structof(ptr, type, member) \
((type *)((char *)(ptr) - offsetof(type, member)))
#define access_once(x) (*(volatile typeof(x) *)&(x))
#define alignof(x) __alignof__(x)
#ifndef likely
#define likely(expr) __builtin_expect(!!(expr), 1)
#endif
#ifndef unlikely
#define unlikely(expr) __builtin_expect(!!(expr), 0)
#endif
#ifndef barrier
#define barrier() asm volatile("" : : : "memory")
#endif
#define __noreturn __attribute__((noreturn))
#define __aligned(x) __attribute__((aligned(x)))
#define __always_inline inline __attribute__((always_inline))
#ifndef __section
#define __section(x) __attribute__((section(x)))
#endif
#define __packed __attribute__((packed))
#define __alias(x) __attribute__((alias(x)))
#define __format_printf(fmt, args) \
__attribute__((format(printf, fmt, args)))
#endif