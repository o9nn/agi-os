#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
char*
commonuser(void)
{
return up->user;
}
char*
commonerror(void)
{
return up->errstr;
}
int
bootpread(char*, ulong, int)
{
return	0;
}