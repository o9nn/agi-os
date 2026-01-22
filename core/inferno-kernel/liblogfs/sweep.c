#include "logfsos.h"
#include "logfs.h"
#include "local.h"
enum {
ThrowAway,
Keep,
Repack,
Error,
};
#define setaction(a) if(*actionp < (a)) *actionp = a
#define REPACK setaction(Repack)
#define KEEP setaction(Keep)
#define OPTCOPYEX(name, etag, stag) \
if(e->etag != s->stag) { \
s->stag = e->etag; \
REPACK; \
}
#define OPTSTRCOPYEX(name, etag, stag) \
if(strcmp(e->etag, s->stag) != 0) { \
s->stag = e->etag; \
REPACK; \
}
#define OPTCOPY(name, tag, sunion) OPTCOPYEX(name, tag, u.sunion.tag)
#define OPTSTRCOPY(name, tag, sunion) OPTSTRCOPYEX(name, tag, u.sunion.tag)
static char *
sweepcreate(LogfsServer *server, LogMessage *s, int *actionp)
{
Entry *pe, *e;
e = logfspathmapfinde(server->pathmap, s->u.create.newpath);
if(e == nil)
return nil;
pe = logfspathmapfinde(server->pathmap, s->path);
if(pe == nil)
return "parent missing";
if((pe->perm & DMDIR) == 0 || (e->perm & DMDIR) != (s->u.create.perm & DMDIR))
return logfseinternal;
if((e->perm & DMDIR) == 0) {
OPTCOPYEX("cvers", u.file.cvers, u.create.cvers);
}
OPTSTRCOPY("name", name, create);
OPTCOPY("mtime", mtime, create);
OPTCOPY("perm", perm, create);
OPTSTRCOPY("uid", uid, create);
OPTSTRCOPY("gid", gid, create);
KEEP;
return nil;
}
static char *
sweepwrite(LogfsServer *server, LogMessage *s, int readoffset, Entry **ep, int *trimp, int *actionp)
{
Entry *e;
Extent extent;
Extent *ext;
*ep = nil;
e = logfspathmapfinde(server->pathmap, s->path);
if(e == nil)
return nil;
if(e->perm & DMDIR)
return logfseinternal;
if(e->u.file.cvers != s->u.write.cvers)
return nil;
extent.min = s->u.write.offset;
extent.max = extent.min + s->u.write.count;
extent.flashaddr = s->u.write.flashaddr;
ext = logfsextentlistmatch(e->u.file.extent, &extent);
if(ext == nil)
return nil;
if(s->u.write.data) {
int offset;
logfsflashaddr2o(server, ext->flashaddr, &offset);
*trimp = offset - readoffset;
*ep = e;
}
KEEP;
return nil;
}
typedef struct FixupState {
LogfsServer *server;
int oldoffset;
u32int newflashaddr;
} FixupState;
static int
fixup(void *magic, Extent *e)
{
FixupState *state = magic;
int offset;
logfsflashaddr2o(state->server, e->flashaddr, &offset);
e->flashaddr = state->newflashaddr + (offset - state->oldoffset);
return 1;
}
static char *
sweepblock(LogfsServer *server, uchar *buf)
{
char *errmsg;
LogSegment *active = server->activelog;
LogSegment *swept = server->sweptlog;
int pagesize, ppb, page;
LogfsLowLevel *ll = server->ll;
LogfsLowLevelReadResult llrr;
int markedbad;
long oblock;
if(active == nil)
return nil;
if(swept == nil) {
errmsg = logfslogsegmentnew(server, loggensucc(active->gen), &server->sweptlog);
if(errmsg)
return errmsg;
swept = server->sweptlog;
}
if(active->unsweptblockindex	== active->curblockindex)
logfslogsegmentflush(server, 1);
ppb = (1 << ll->l2pagesperblock);
pagesize = (1 << ll->l2pagesize);
for(page = 0; page < ppb; page++) {
uchar *p, *bufend;
errmsg = (*ll->readpagerange)(ll, buf, active->blockmap[active->unsweptblockindex], page, 0,  pagesize, &llrr);
if(errmsg)
goto fail;
if(llrr != LogfsLowLevelReadResultOk)
logfsserverreplacelogblock(server, active, active->unsweptblockindex);
p = buf;
if(*p == 0xff)
break;
bufend = p + pagesize;
while(p < bufend) {
int action;
uint size;
LogMessage s;
Entry *e;
int trim;
size = logfsconvM2S(p, bufend - p, &s);
if(size == 0)
return "parse failure";
if(server->trace > 1) {
print("A>> ");
logfsdumpS(&s);
print("\n");
}
if(s.type == LogfsLogTend)
break;
action = ThrowAway;
switch(s.type) {
case LogfsLogTstart:
break;
case LogfsLogTcreate:
errmsg = sweepcreate(server, &s, &action);
break;
case LogfsLogTremove:
break;
case LogfsLogTtrunc:
break;
case LogfsLogTwrite:
errmsg = sweepwrite(server, &s, s.u.write.data ? s.u.write.data - buf : 0, &e, &trim, &action);
break;
case LogfsLogTwstat:
break;
default:
return "bad tag in log page";
}
if(action == Error)
return errmsg;
if(errmsg)
print("bad sweep: %s\n", errmsg);
if(action == Keep)
action = Repack;
if(action == Keep) {
errmsg = logfslogbytes(server, 0, p, size);
if(errmsg)
goto fail;
}
else if(action == Repack) {
if(s.type == LogfsLogTwrite && s.u.write.data) {
FixupState state;
errmsg = logfslogwrite(server, 0, s.path, s.u.write.offset + trim, s.u.write.count - trim,
s.u.write.mtime, s.u.write.cvers,
s.u.write.muid, s.u.write.data + trim, &state.newflashaddr);
if(errmsg == nil && s.u.write.data != nil) {
Extent extent;
state.oldoffset = s.u.write.data - buf + trim;
state.server = server;
extent.min = s.u.write.offset;
extent.max = extent.min + s.u.write.count;
extent.flashaddr = s.u.write.flashaddr;
logfsextentlistmatchall(e->u.file.extent, fixup, &state, &extent);
}
}
else
errmsg = logfslog(server, 0, &s);
if(errmsg)
goto fail;
}
p += size;
}
}
oblock = active->blockmap[active->unsweptblockindex++];
errmsg = logfsbootfettleblock(server->lb, oblock, LogfsTnone, ~0, &markedbad);
if(errmsg)
goto fail;
if(active->unsweptblockindex  > active->curblockindex) {
logfslogsegmentfree(&active);
server->activelog = swept;
server->sweptlog = nil;
swept->dirty = 0;
}
return nil;
fail:
return errmsg;
}
char *
logfsserverlogsweep(LogfsServer *server, int justone, int *didsomething)
{
uchar *buf;
char *errmsg;
*didsomething = 0;
if(!server->activelog->dirty)
return nil;
buf = logfsrealloc(nil, (1 << server->ll->l2pagesize));
if(buf == nil)
return Enomem;
errmsg = nil;
while(server->activelog->unsweptblockindex <= server->activelog->curblockindex) {
errmsg = sweepblock(server, buf);
if(errmsg)
break;
if(server->sweptlog == nil || justone)
break;
}
logfsfreemem(buf);
*didsomething = 1;
return errmsg;
}