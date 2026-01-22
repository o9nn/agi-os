#ifndef lint
static char *ident = "$Id: mem.c,v 1.2 89/02/12 10:04:53 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
char *mem_get(uint len)
#else
char *mem_get(len)
uint len;
#endif
{
char *mem;
static short outofmem = 0;
if ((mem = (char *)malloc(len)) == (char *)NULL && !outofmem) {
outofmem++;
warn("mem_get()", "Out of memory");
}
return (mem);
}
#ifdef __STDC__
char *mem_str(char *str)
#else
char *mem_str(str)
char *str;
#endif
{
char *mem;
if (mem = mem_get((uint) strlen(str) + 1)) {
strcpy(mem, str);
}
return (mem);
}