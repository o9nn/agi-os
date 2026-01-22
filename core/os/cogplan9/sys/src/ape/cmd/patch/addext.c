#if HAVE_CONFIG_H
# include <config.h>
#endif
#ifndef HAVE_DOS_FILE_NAMES
#define HAVE_DOS_FILE_NAMES 0
#endif
#ifndef HAVE_LONG_FILE_NAMES
#define HAVE_LONG_FILE_NAMES 0
#endif
#include <backupfile.h>
#if HAVE_LIMITS_H
# include <limits.h>
#endif
#ifndef _POSIX_NAME_MAX
#define _POSIX_NAME_MAX 14
#endif
#include <sys/types.h>
#if HAVE_STRING_H
# include <string.h>
#else
# include <strings.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
void
addext (filename, ext, e)
char *filename;
char const *ext;
int e;
{
char *s = base_name (filename);
size_t slen = strlen (s), extlen = strlen (ext);
long slen_max = -1;
#if HAVE_PATHCONF && defined _PC_NAME_MAX
if (slen + extlen <= _POSIX_NAME_MAX && ! HAVE_DOS_FILE_NAMES)
slen_max = _POSIX_NAME_MAX;
else if (s == filename)
slen_max = pathconf (".", _PC_NAME_MAX);
else
{
char c = *s;
*s = 0;
slen_max = pathconf (filename, _PC_NAME_MAX);
*s = c;
}
#endif
if (slen_max < 0)
slen_max = HAVE_LONG_FILE_NAMES ? 255 : 14;
if (HAVE_DOS_FILE_NAMES && slen_max <= 12)
{
char *dot = strchr (s, '.');
if (dot)
{
slen -= dot + 1 - s;
s = dot + 1;
slen_max = 3;
}
else
slen_max = 8;
extlen = 9;
}
if (slen + extlen <= slen_max)
strcpy (s + slen, ext);
else
{
if (slen_max <= slen)
slen = slen_max - 1;
s[slen] = e;
s[slen + 1] = 0;
}
}