#if HAVE_CONFIG_H
# include <config.h>
#endif
#include <argmatch.h>
#include <sys/types.h>
#include <stdio.h>
#if HAVE_STRING_H
# include <string.h>
#else
# include <strings.h>
#endif
int
argmatch (arg, optlist)
const char *arg;
const char *const *optlist;
{
int i;
size_t arglen;
int matchind = -1;
int ambiguous = 0;
arglen = strlen (arg);
for (i = 0; optlist[i]; i++)
{
if (!strncmp (optlist[i], arg, arglen))
{
if (strlen (optlist[i]) == arglen)
return i;
else if (matchind == -1)
matchind = i;
else
ambiguous = 1;
}
}
if (ambiguous)
return -2;
else
return matchind;
}
void
invalid_arg (kind, value, problem)
const char *kind;
const char *value;
int problem;
{
fprintf (stderr, "%s: ", program_name);
if (problem == -1)
fprintf (stderr, "invalid");
else
fprintf (stderr, "ambiguous");
fprintf (stderr, " %s `%s'\n", kind, value);
}