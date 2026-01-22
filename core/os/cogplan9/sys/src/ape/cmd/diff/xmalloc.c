#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#if __STDC__
#define VOID void
#else
#define VOID char
#endif
#include <sys/types.h>
#if STDC_HEADERS
#include <stdlib.h>
#else
VOID *malloc ();
VOID *realloc ();
void free ();
#endif
#if __STDC__ && defined (HAVE_VPRINTF)
void error (int, int, char const *, ...);
#else
void error ();
#endif
VOID *
xmalloc (n)
size_t n;
{
VOID *p;
p = malloc (n);
if (p == 0)
error (2, 0, "memory exhausted");
return p;
}
VOID *
xrealloc (p, n)
VOID *p;
size_t n;
{
if (p == 0)
return xmalloc (n);
if (n == 0)
{
free (p);
return 0;
}
p = realloc (p, n);
if (p == 0)
error (2, 0, "memory exhausted");
return p;
}