#ifndef _MACH_SA_SYS_STRING_H_
#define _MACH_SA_SYS_STRING_H_
#include <sys/types.h>
extern void *memcpy (void *dest, const void *src, size_t n);
extern void *memmove (void *dest, const void *src, size_t n);
extern int memcmp (const void *s1, const void *s2, size_t n) __attribute__ ((pure));
extern void *memset (void *s, int c, size_t n);
extern char *strchr (const char *s, int c);
extern char *strcpy (char *dest, const char *src);
extern char *strncpy (char *dest, const char *src, size_t n);
extern char *strsep (char **strp, const char *delim);
extern int strcmp (const char *s1, const char *s2) __attribute__ ((pure));
extern int strncmp (const char *s1, const char *s2, size_t n) __attribute__ ((pure));
extern size_t strlen (const char *s) __attribute__ ((pure));
extern char *strstr(const char *haystack, const char *needle);
#endif