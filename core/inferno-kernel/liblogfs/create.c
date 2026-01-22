#include "logfsos.h"
#include "logfs.h"
#include "local.h"
char *
logfsservercreate(LogfsServer *server, u32int fid, char *name, u32int perm, uchar mode, Qid *qid)
{
Fid *f;
char *uid;
ulong newpath;
char *errmsg;
Entry *e, *xe, *pe;
Path *pp;
LogMessage s;
if(server->trace > 1)
print("logfsservercreate(%ud, %s, 0%uo, %.2ux)\n", fid, name, perm, mode);
f = logfsfidmapfindentry(server->fidmap, fid);
if(f == nil)
return logfsebadfid;
if(f->openmode >= 0)
return logfsefidopen;
pe = f->entry;
if((pe->qid.type & QTDIR) == 0)
return Enotdir;
if((perm & DMDIR) != 0 && ((mode & OTRUNC) != 0 || (mode &  3) != OREAD))
return Eperm;
if(!logfsuserpermcheck(server, pe, f, DMWRITE))
return Eperm;
if(strcmp(name, ".") == 0 || strcmp(name, "..") == 0)
return Eperm;
for(xe = pe->u.dir.list; xe; xe = xe->next)
if(strcmp(xe->name, name) == 0)
return Eexist;
newpath = ++server->path;
while(logfspathmapfindentry(server->pathmap, newpath))
newpath++;
uid = logfsisfindidfromname(server->is, f->uname);
errmsg = logfsentrynew(server, 1, newpath,
pe, name, uid, f->entry->gid, logfsnow(), uid, perm, 0, 0, &e);
if(errmsg)
return errmsg;
errmsg = logfspathmapnewentry(server->pathmap, newpath, e, &pp);
if(errmsg) {
logfsfreemem(e);
return errmsg;
}
s.type = LogfsLogTcreate;
s.path = e->parent->qid.path;
s.u.create.perm = e->perm;
s.u.create.newpath = e->qid.path;
s.u.create.mtime = e->mtime;
s.u.create.cvers = (e->qid.type & QTDIR) ? 0 : e->u.file.cvers;
s.u.create.name = e->name;
s.u.create.uid = e->uid;
s.u.create.gid = e->gid;
errmsg = logfslog(server, 1, &s);
if(errmsg) {
logfsfreemem(e);
logfspathmapdeleteentry(server->pathmap, newpath);
return errmsg;
}
server->path = newpath;
e->inuse++;
e->qid.vers++;
e->next = pe->u.dir.list;
pe->u.dir.list = e;
f->openmode = mode;
logfsentryclunk(pe);
f->entry = e;
*qid = e->qid;
return nil;
}