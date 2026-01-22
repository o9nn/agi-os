#include "stat_.h"
#include "string_.h"
char *
mktemp(char *fname)
{
struct stat fst;
int len = strlen(fname);
char *end = fname + len - 6;
if (len < 6 || strcmp(end, "XXXXXX"))
return (char *)0;
strcpy(end, "AA.AAA");
while (stat(fname, &fst) == 0) {
char *inc = fname + len - 1;
while (*inc == 'Z' || *inc == '.') {
if (inc == end)
return (char *)0;
if (*inc == 'Z')
*inc = 'A';
--inc;
}
++*inc;
}
return fname;
}