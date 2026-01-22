#ifndef	_KERN_ASSERT_H_
#define	_KERN_ASSERT_H_
#include <kern/macros.h>
#ifndef NDEBUG
#define MACH_ASSERT 1
#endif
#if	MACH_ASSERT
extern void Assert(const char *exp, const char *filename, int line,
const char *fun) __attribute__ ((noreturn));
#define assert(ex)							\
(likely(ex)							\
? (void) (0)							\
: Assert (#ex, __FILE__, __LINE__, __FUNCTION__))
#define	assert_static(x)	assert(x)
#else
#define assert(ex)
#define assert_static(ex)
#endif
#endif