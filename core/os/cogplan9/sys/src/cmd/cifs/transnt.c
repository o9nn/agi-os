#include <u.h>
#include <libc.h>
#include <fcall.h>
#include <thread.h>
#include <9p.h>
#include "cifs.h"
static Pkt *
tnthdr(Session *s, Share *sp, int cmd)
{
Pkt *p;
p = cifshdr(s, sp, SMB_COM_NT_TRANSACT);
p->tbase = p8(p, 0);
pl16(p, 0);
pl32(p, 0);
pl32(p, 0);
pl32(p, 64);
pl32(p, (MTU - T2HDRLEN)-64);
pl32(p, 0);
pl32(p, 0);
pl32(p, 0);
pl32(p, 0);
p8(p, 1);
pl16(p, cmd);
pl16(p, 0);
pbytes(p);
return p;
}
static void
ptntparam(Pkt *p)
{
uchar *pos = p->pos;
assert(p->tbase != 0);
p->pos = p->tbase +23;
pl32(p, (pos - p->buf) - NBHDRLEN);
p->tparam = p->pos = pos;
}
static void
ptntdata(Pkt *p)
{
uchar *pos = p->pos;
assert(p->tbase != 0);
assert(p->tparam != 0);
p->pos = p->tbase +3;
pl32(p, pos - p->tparam);
p->pos = p->tbase +19;
pl32(p, pos - p->tparam);
p->pos = p->tbase +31;
pl32(p, (pos - p->buf) - NBHDRLEN);
p->tdata = p->pos = pos;
}
static int
tntrpc(Pkt *p)
{
int got;
uchar *pos;
assert(p->tbase != 0);
assert(p->tdata != 0);
pos = p->pos;
p->pos = p->tbase +7;
pl32(p, pos - p->tdata);
p->pos = p->tbase +27;
pl32(p, pos - p->tdata);
p->pos = pos;
if((got = cifsrpc(p)) == -1)
return -1;
g8(p);
g8(p);
g8(p);
gl32(p);
gl32(p);
gl32(p);
p->tparam = p->buf +NBHDRLEN +gl32(p);
gl32(p);
gl32(p);
p->tdata = p->buf +NBHDRLEN +gl32(p);
gl32(p);
g8(p);
gl16(p);
return got;
}
static void
gtntparam(Pkt *p)
{
p->pos = p->tparam;
}
static void
gtntdata(Pkt *p)
{
p->pos = p->tdata;
}
int
TNTquerysecurity(Session *s, Share *sp, int fh, char **usid, char **gsid)
{
Pkt *p;
uchar *base;
Fmt fmt, *f = &fmt;
int n, i, off2owner, off2group;
p = tnthdr(s, sp, NT_TRANSACT_QUERY_SECURITY_DESC);
ptntparam(p);
pl16(p, fh);
pl16(p, 0);
pl32(p, QUERY_OWNER_SECURITY_INFORMATION |
QUERY_GROUP_SECURITY_INFORMATION);
ptntdata(p);
if(tntrpc(p) == -1){
free(p);
return -1;
}
gtntdata(p);
base = p->pos;
gl16(p);
gl16(p);
off2owner = gl32(p);
off2group = gl32(p);
gl32(p);
gl32(p);
if(off2owner){
p->pos = base +  off2owner;
fmtstrinit(f);
fmtprint(f, "S-%ud", g8(p));
n = g8(p);
fmtprint(f, "-%llud", gb48(p));
for(i = 0; i < n; i++)
fmtprint(f, "-%ud", gl32(p));
*usid = fmtstrflush(f);
}
if(off2group){
p->pos = base +  off2group;
fmtstrinit(f);
fmtprint(f, "S-%ud", g8(p));
n = g8(p);
fmtprint(f, "-%llud", gb48(p));
for(i = 0; i < n; i++)
fmtprint(f, "-%ud", gl32(p));
*gsid = fmtstrflush(f);
}
free(p);
return 0;
}