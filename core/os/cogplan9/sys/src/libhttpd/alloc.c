#include <u.h>
#include <libc.h>
#include <bin.h>
#include <httpd.h>
char*
hstrdup(HConnect *c, char *s)
{
char *t;
int n;
n = strlen(s) + 1;
t = binalloc(&c->bin, n, 0);
if(t == nil)
sysfatal("out of memory");
memmove(t, s, n);
return t;
}
void*
halloc(HConnect *c, ulong n)
{
void *p;
p = binalloc(&c->bin, n, 1);
if(p == nil)
sysfatal("out of memory");
return p;
}