#include "logfsos.h"
#include "logfs.h"
#include "local.h"
int
logfsuserpermcheck(LogfsServer *s, Entry *e, Fid *f, ulong permmask)
{
if(s->openflags & LogfsOpenFlagNoPerm)
return 1;
if((e->perm & permmask) == permmask)
return 1;
if(((e->perm >> 6) & permmask) == permmask) {
char *uname = logfsisfindnamefromid(s->is, e->uid);
if(uname == f->uname)
return 1;
}
if(((e->perm >> 3) & permmask) == permmask) {
Group *g = logfsisfindgroupfromid(s->is, e->gid);
return g && logfsisgroupunameismember(s->is, g, f->uname);
}
return 0;
}