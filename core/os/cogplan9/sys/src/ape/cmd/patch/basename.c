#if HAVE_CONFIG_H
# include <config.h>
#endif
#include <backupfile.h>
#ifndef FILESYSTEM_PREFIX_LEN
#define FILESYSTEM_PREFIX_LEN(f) 0
#endif
#ifndef ISSLASH
#define ISSLASH(c) ((c) == '/')
#endif
char *
base_name (name)
char const *name;
{
char const *base = name += FILESYSTEM_PREFIX_LEN (name);
for (; *name; name++)
if (ISSLASH (*name))
base = name + 1;
return (char *) base;
}