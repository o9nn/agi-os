#ifndef __sys_types_h__
#define __sys_types_h__
#include <MacTypes.h>
#include <unix.h>
#define CHECK_INTERRUPTS
#define GX_COLOR_INDEX_TYPE UInt64
#define main gs_main
#if (0)
#define fprintf myfprintf
#define fputs myfputs
#define getenv mygetenv
int myfprintf(FILE *file, const char *fmt, ...);
int myfputs(const char *string, FILE *file);
#endif
#ifndef __MACOS__
#define __MACOS__
#endif
#endif