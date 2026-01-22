#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
enum{
Qdir,
Qdata,
};
static
Dirtab XXXtab[]={
".", {Qdir, 0, QTDIR}, 0, 0555,
"data", {Qdata, 0}, 0, 0666,
};
static void
XXXreset(void)
{
}
static void
XXXinit(void)
{
}
static Chan*
XXXattach(char* spec)
{
return devattach('X', spec);
}
static Walkqid*
XXXwalk(Chan *c, Chan *nc, char **name, int nname)
{
return devwalk(c, nc, name, nname, XXXtab, nelem(XXXtab), devgen);
}
static int
XXXstat(Chan* c, uchar *db, int n)
{
return devstat(c, db, n, XXXtab, nelem(XXXtab), devgen);
}
static Chan*
XXXopen(Chan* c, int omode)
{
return devopen(c, omode, XXXtab, nelem(XXXtab), devgen);
}
static void
XXXcreate(Chan* c, char* name, int omode, ulong perm)
{
USED(c, name, omode, perm);
error(Eperm);
}
static void
XXXremove(Chan* c)
{
USED(c);
error(Eperm);
}
static int
XXXwstat(Chan* c, uchar *dp, int n)
{
USED(c, dp);
error(Eperm);
return n;
}
static void
XXXclose(Chan* c)
{
USED(c);
}
static long
XXXread(Chan* c, void* a, long n, vlong offset)
{
USED(offset);
switch((ulong)c->qid.path){
case Qdir:
return devdirread(c, a, n, XXXtab, nelem(XXXtab), devgen);
case Qdata:
break;
default:
n=0;
break;
}
return n;
}
static Block*
XXXbread(Chan* c, long n, ulong offset)
{
return devbread(c, n, offset);
}
static long
XXXwrite(Chan* c, void* a, long n, vlong offset)
{
USED(a, offset);
switch((ulong)c->qid.path){
case Qdata:
break;
default:
error(Ebadusefd);
}
return n;
}
static long
XXXbwrite(Chan* c, Block* bp, ulong offset)
{
return devbwrite(c, bp, offset);
}
Dev XXXdevtab = {
'X',
"XXX",
XXXreset,
XXXinit,
devshutdown,
XXXattach,
XXXwalk,
XXXstat,
XXXopen,
XXXcreate,
XXXclose,
XXXread,
XXXbread,
XXXwrite,
XXXbwrite,
XXXremove,
XXXwstat,
};