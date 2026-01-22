#include	"l.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>
#undef getwd
#include <unistd.h>
void*
malloc(size_t n)
{
void *p;
while(n & 7)
n++;
while(nhunk < n)
gethunk();
p = hunk;
nhunk -= n;
hunk += n;
return p;
}
void
free(void *p)
{
USED(p);
}
void*
calloc(size_t m, size_t n)
{
void *p;
n *= m;
p = malloc(n);
memset(p, 0, n);
return p;
}
void*
realloc(void *p, size_t n)
{
fprint(2, "realloc called\n", p, n);
abort();
return 0;
}
void*
mysbrk(ulong size)
{
return (void*)sbrk(size);
}
double
cputime(void)
{
struct tms tmbuf;
double	ret_val;
(void)times(&tmbuf);
ret_val = (double)(tmbuf.tms_utime + tmbuf.tms_stime +
tmbuf.tms_cutime + tmbuf.tms_cstime);
ret_val *= sysconf(_SC_CLK_TCK);
return ret_val;
}
int
fileexists(char *name)
{
struct stat sb;
return stat(name, &sb) >= 0;
}