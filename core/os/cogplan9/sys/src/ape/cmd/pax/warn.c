#ifndef lint
static char *ident = "$Id: warn.c,v 1.2 89/02/12 10:06:15 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
static void prsize(FILE *, OFFSET);
#else
static void prsize();
#endif
#ifdef __STDC__
void warnarch(char *msg, OFFSET adjust)
#else
void warnarch(msg, adjust)
char *msg;
OFFSET adjust;
#endif
{
fprintf(stderr, "%s: [offset ", myname);
prsize(stderr, total - adjust);
fprintf(stderr, "]: %s\n", msg);
}
#ifdef __STDC__
char *strerror(void)
#else
char *strerror()
#endif
{
#ifdef _POSIX_SOURCE
#undef strerror
return (strerror(errno));
#else
static char msg[40];
if (errno > 0 && errno < sys_nerr) {
return (sys_errlist[errno]);
}
sprintf(msg, "Unknown error (errno %d)", errno);
return (msg);
#endif
}
#ifdef __STDC__
static void prsize(FILE *stream, OFFSET size)
#else
static void prsize(stream, size)
FILE *stream;
OFFSET size;
#endif
{
OFFSET n;
if (n = (size / (1024L * 1024L))) {
fprintf(stream, "%ldm+", n);
size -= n * 1024L * 1024L;
}
if (n = (size / 1024L)) {
fprintf(stream, "%ldk+", n);
size -= n * 1024L;
}
fprintf(stream, "%ld", size);
}
#ifdef __STDC__
void fatal(char *why)
#else
void fatal(why)
char *why;
#endif
{
fprintf(stderr, "%s: %s\n", myname, why);
exit(1);
}
#ifdef __STDC__
void warn(char *what, char *why)
#else
void warn(what, why)
char *what;
char *why;
#endif
{
fprintf(stderr, "%s: %s : %s\n", myname, what, why);
fflush(stderr);
}