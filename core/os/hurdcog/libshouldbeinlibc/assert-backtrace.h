#ifndef __ASSERT_BACKTRACE__
#define __ASSERT_BACKTRACE__
#ifdef NDEBUG
#define assert_backtrace(expr) ((void) 0)
#define assert_perror_backtrace(errnum) ((void) 0)
#else
#include <sys/cdefs.h>
void __assert_fail_backtrace (const char *assertion,
const char *file,
unsigned int line,
const char *function)
__attribute__ ((noreturn, unused));
void __assert_perror_fail_backtrace (int errnum,
const char *file,
unsigned int line,
const char *function)
__attribute__ ((noreturn, unused));
#define assert_backtrace(expr) \
(__builtin_expect(!!(expr), 1) \
? (void) 0 \
: __assert_fail_backtrace (__STRING(expr), \
__FILE__, __LINE__, \
__PRETTY_FUNCTION__))
#define assert_perror_backtrace(expr) \
(__builtin_expect(((expr) == 0), 1) \
? (void) 0 \
: __assert_perror_fail_backtrace ((expr), \
__FILE__, __LINE__, \
__PRETTY_FUNCTION__))
void backtrace_stderr (void);
void backtrace_mach (void);
#endif
#endif