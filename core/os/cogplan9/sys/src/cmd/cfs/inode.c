#include <u.h>
#include <libc.h>
#include "cformat.h"
#include "lru.h"
#include "bcache.h"
#include "disk.h"
#include "inode.h"
#include "stats.h"
int
iinit(Icache *ic, int f, int psize, char* name)
{
Ibuf *b;
Imap *m;
ulong ino;
Bbuf *bb;
Dinode *bi;
if(dinit(ic, f, psize, name) < 0)
return -1;
bb = bcread(ic, ic->nab);
if(bb == 0){
fprint(2, "iinit: can't read disk\n");
return -1;
}
bi = (Dinode*)bb->data;
if(bi->nino==0 || bi->nino>2048){
fprint(2, "iinit: bad nino\n");
return -1;
}
ic->nino = bi->nino;
ic->i2b = (ic->bsize - sizeof(Dihdr))/sizeof(Inode);
ic->nib = (ic->nino + ic->i2b - 1)/ic->i2b;
if(ic->map)
free(ic->map);
ic->map = malloc(sizeof(Imap)*ic->nino);
if(ic->map == 0){
fprint(2, "iinit: can't alloc map\n");
return -1;
}
lruinit(&ic->mlru);
for(m = ic->map; m < &ic->map[ic->nino]; m++){
m->inuse = 0;
m->b = 0;
lruadd(&ic->mlru, m);
}
lruinit(&ic->blru);
for(b = ic->ib; b < &ic->ib[Nicache]; b++){
b->inuse = 0;
lruadd(&ic->blru, b);
}
for(ino = 0; ino < ic->nino; ino++){
b = iread(ic, ino);
if(b == 0){
fprint(2, "iinit: can't read inode %ld\n", ino);
return -1;
}
if(b->inode.inuse){
m = &ic->map[ino];
m->inuse = 1;
m->qid = b->inode.qid;
lruref(&ic->mlru, m);
}
}
return 0;
}
int
iformat(Icache *ic, int f, ulong nino, char *name, int bsize, int psize)
{
int nib;
ulong bno, i2b, i;
Bbuf *bb;
Dinode *bi;
if(dformat(ic, f, name, bsize, psize) < 0)
return -1;
fprint(2, "formatting inodes\n");
i2b = (bsize - sizeof(Dihdr))/sizeof(Inode);
nib = (nino + i2b - 1)/i2b;
for(bno = ic->nab; bno < ic->nab + nib; bno++){
if(dalloc(ic, 0) == Notabno){
fprint(2, "iformat: balloc failed\n");
return -1;
}
bb = bcalloc(ic, bno);
if(bb == 0){
fprint(2, "iformat: bcalloc failed\n");
return -1;
}
bi = (Dinode*)bb->data;
bi->magic = Imagic;
bi->nino = nino;
for(i = 0; i < i2b; i++)
bi->inode[i].inuse = 0;
bcmark(ic, bb);
}
bcsync(ic);
return iinit(ic, f, psize, name);
}
Ibuf*
ialloc(Icache *ic, ulong ino)
{
Imap *m;
Ibuf *b;
b = (Ibuf*)ic->blru.lnext;
if(b->inuse)
ic->map[b->ino].b = 0;
b->ino = ino;
b->inuse = 1;
m = &ic->map[ino];
m->b = b;
return b;
}
void
ifree(Icache *ic, Ibuf *b)
{
b->inuse = 0;
if(b->inuse)
ic->map[b->ino].b = 0;
lruderef(&ic->blru, b);
}
Ibuf *
iget(Icache *ic, Qid qid)
{
Imap *m, *me;
Ibuf *b;
for(m = ic->map, me = &ic->map[ic->nino]; m < me; m++)
if(m->inuse && m->qid.path==qid.path){
if(m->qid.vers != qid.vers){
DPRINT(2, "updating old file %llud.%lud\n",
qid.path, qid.vers);
m->qid = qid;
iupdate(ic, m - ic->map, qid);
}
break;
}
if(m != me)
return iread(ic, m - ic->map);
m = (Imap*)ic->mlru.lnext;
if(m->inuse){
DPRINT(2, "superceding file %llud.%ld by %llud.%ld\n",
m->qid.path, m->qid.vers, qid.path, qid.vers);
if(iremove(ic, m - ic->map) < 0)
return 0;
}
if(statson)
cfsstat.ninsert++;
DPRINT(2, "new file %llud.%ld ino %ld\n",
qid.path, qid.vers, m - ic->map);
b = ialloc(ic, m - ic->map);
b->inode.inuse = m->inuse = 1;
b->inode.qid = qid;
b->inode.length = 0x7fffffffffffffffLL;
m->qid = qid;
b->inode.ptr.bno = Notabno;
iwrite(ic, b);
return b;
}
Ibuf*
iread(Icache *ic, ulong ino)
{
Ibuf *b;
Imap *m;
ulong bno;
Bbuf *bb;
Dinode *bi;
m = &ic->map[ino];
if(m->inuse && m->b){
b = m->b;
goto out;
}
b = ialloc(ic, ino);
bno = ic->nab + ino/ic->i2b;
bb = bcread(ic, bno);
if(bb == 0){
ifree(ic, b);
return 0;
}
bi = (Dinode*)bb->data;
b->inode = bi->inode[ino % ic->i2b];
if(bi->nino!=ic->nino || bi->magic!=Imagic){
fprint(2, "iread: inconsistent inode block\n");
ifree(ic, b);
return 0;
}
out:
b->inuse = 1;
m->b = b;
if(b->inode.inuse)
lruref(&ic->mlru, m);
lruref(&ic->blru, b);
return b;
}
int
iwrite(Icache *ic, Ibuf *b)
{
ulong bno;
Bbuf *bb;
Dinode *bi;
bno = ic->nab + b->ino/ic->i2b;
bb = bcread(ic, bno);
if(bb == 0)
return 0;
bi = (Dinode*)bb->data;
bi->inode[b->ino % ic->i2b] = b->inode;
bcmark(ic, bb);
lruref(&ic->mlru, &ic->map[b->ino]);
lruref(&ic->blru, b);
return 0;
}
int
iupdate(Icache *ic, ulong ino, Qid qid)
{
Ibuf *b;
Imap *m;
Dptr d;
if(statson)
cfsstat.nupdate++;
b = iread(ic, ino);
if(b == 0)
return -1;
b->inode.qid = qid;
b->inode.length = 0x7fffffffffffffffLL;
m = &ic->map[ino];
m->qid = qid;
d = b->inode.ptr;
b->inode.ptr.bno = Notabno;
if(iwrite(ic, b) < 0)
return -1;
dfree(ic, &d);
return 0;
}
int
iremove(Icache *ic, ulong ino)
{
Ibuf *b;
Imap *m;
if(statson)
cfsstat.ndelete++;
m = &ic->map[ino];
b = iread(ic, ino);
if(b == 0)
return -1;
b->inode.inuse = 0;
if(iwrite(ic, b) < 0)
return -1;
dfree(ic, &b->inode.ptr);
ifree(ic, b);
lruderef(&ic->mlru, m);
return 0;
}
void
iinc(Icache *ic, Ibuf *b)
{
b->inode.qid.vers++;
ic->map[b->ino].qid = b->inode.qid;
iwrite(ic, b);
}