#include <u.h>
#include <libc.h>
static int isnumber(char *s);
sniff(Biobuf *b)
{
read first line of file into p;
nf = getfields(p, f, nelem(f), ":");
if(nf < 4)
return nil;
if(isnumber(f[0]) && !isnumber(f[2]))
return _plan9;
if(!isnumber(f[0]) && isnumber(f[2]))
return _unix;
return nil;
}
int
isnumber(char *s)
{
char *q;
strtol(s, &q, 10);
return *q == '\0';
}