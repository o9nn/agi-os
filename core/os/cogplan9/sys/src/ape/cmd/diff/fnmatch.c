#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "system.h"
#include <errno.h>
#include "fnmatch.h"
#if !defined(__GNU_LIBRARY__) && !defined(STDC_HEADERS)
extern int errno;
#endif
int
#if __STDC__
fnmatch (const char *pattern, const char *string, int flags)
#else
fnmatch (pattern, string, flags)
char *pattern;
char *string;
int flags;
#endif
{
register const char *p = pattern, *n = string;
register char c;
if ((flags & ~__FNM_FLAGS) != 0)
{
errno = EINVAL;
return -1;
}
while ((c = *p++) != '\0')
{
switch (c)
{
case '?':
if (*n == '\0')
return FNM_NOMATCH;
else if ((flags & FNM_PATHNAME) && *n == '/')
return FNM_NOMATCH;
else if ((flags & FNM_PERIOD) && *n == '.' &&
(n == string || ((flags & FNM_PATHNAME) && n[-1] == '/')))
return FNM_NOMATCH;
break;
case '\\':
if (!(flags & FNM_NOESCAPE))
c = *p++;
if (FOLD_FN_CHAR (*n) != FOLD_FN_CHAR (c))
return FNM_NOMATCH;
break;
case '*':
if ((flags & FNM_PERIOD) && *n == '.' &&
(n == string || ((flags & FNM_PATHNAME) && n[-1] == '/')))
return FNM_NOMATCH;
for (c = *p++; c == '?' || c == '*'; c = *p++, ++n)
if (((flags & FNM_PATHNAME) && *n == '/') ||
(c == '?' && *n == '\0'))
return FNM_NOMATCH;
if (c == '\0')
return 0;
{
char c1 = (!(flags & FNM_NOESCAPE) && c == '\\') ? *p : c;
for (--p; *n != '\0'; ++n)
if ((c == '[' || FOLD_FN_CHAR (*n) == FOLD_FN_CHAR (c1)) &&
fnmatch(p, n, flags & ~FNM_PERIOD) == 0)
return 0;
return FNM_NOMATCH;
}
case '[':
{
register int not;
if (*n == '\0')
return FNM_NOMATCH;
if ((flags & FNM_PERIOD) && *n == '.' &&
(n == string || ((flags & FNM_PATHNAME) && n[-1] == '/')))
return FNM_NOMATCH;
not = (*p == '!' || *p == '^');
if (not)
++p;
c = *p++;
for (;;)
{
register char cstart = c, cend = c;
if (!(flags & FNM_NOESCAPE) && c == '\\')
cstart = cend = *p++;
if (c == '\0')
return FNM_NOMATCH;
c = *p++;
if ((flags & FNM_PATHNAME) && c == '/')
return FNM_NOMATCH;
if (c == '-' && *p != ']')
{
cend = *p++;
if (!(flags & FNM_NOESCAPE) && cend == '\\')
cend = *p++;
if (cend == '\0')
return FNM_NOMATCH;
c = *p++;
}
if (*n >= cstart && *n <= cend)
goto matched;
if (c == ']')
break;
}
if (!not)
return FNM_NOMATCH;
break;
matched:;
while (c != ']')
{
if (c == '\0')
return FNM_NOMATCH;
c = *p++;
if (!(flags & FNM_NOESCAPE) && c == '\\')
++p;
}
if (not)
return FNM_NOMATCH;
}
break;
default:
if (FOLD_FN_CHAR (c) != FOLD_FN_CHAR (*n))
return FNM_NOMATCH;
}
++n;
}
if (*n == '\0')
return 0;
return FNM_NOMATCH;
}