#ifndef	_ERROR_H
#define	_ERROR_H
#include <sys/errno.h>
extern int errno;
#if	defined(__GNUC__)
extern void fatal(const char *format, ...)
#if	__GNUC__ > 1
__attribute__ ((format (printf, 1, 2),
noreturn))
#endif
;
extern void warn(const char *format, ...)
#if	__GNUC__ > 1
__attribute__ ((format (printf, 1, 2)))
#endif
;
extern void error(const char *format, ...)
#if	__GNUC__ > 1
__attribute__ ((format (printf, 1, 2)))
#endif
;
#else
extern void fatal(const char *format, ...);
extern void warn(const char *format, ...);
extern void error(const char *format, ...);
#endif
extern const char *unix_error_string(int error_num);
extern int errors;
extern void set_program_name(const char *name);
#endif