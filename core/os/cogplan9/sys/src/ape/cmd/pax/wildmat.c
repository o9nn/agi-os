#ifndef lint
static char *ident = "$Id: wildmat.c,v 1.2 89/02/12 10:06:20 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
static int star(char *, char *);
#else
static int      star();
#endif
#ifdef __STDC__
static int star(char *source, char *pattern)
#else
static int star(source, pattern)
char           *source;
char           *pattern;
#endif
{
while (!wildmat(pattern, source)) {
if (*++source == '\0') {
return (0);
}
}
return (1);
}
#ifdef __STDC__
int wildmat(char *pattern, char *source)
#else
int wildmat(pattern, source)
char           *pattern;
char           *source;
#endif
{
int             last;
int             matched;
int             reverse;
for (; *pattern; source++, pattern++) {
switch (*pattern) {
case '\\':
pattern++;
default:
if (*source != *pattern) {
return (0);
}
continue;
case '?':
if (*source == '\0') {
return (0);
}
continue;
case '*':
return (*++pattern ? star(source, pattern) : 1);
case '[':
if (reverse = pattern[1] == '^') {
pattern++;
}
for (last = 0400, matched = 0;
*++pattern && *pattern != ']'; last = *pattern) {
if (*pattern == '-'
? *source <= *++pattern && *source >= last
: *source == *pattern) {
matched = 1;
}
}
if (matched == reverse) {
return (0);
}
continue;
}
}
return (*source == '\0' || *source == '/');
}