#include <u.h>
#include <libc.h>
#include <bio.h>
#include <libsec.h>
#include "iso9660.h"
enum {
Big,
Little
};
static void
Crdpath(Cdimg *cd, Cpath *p)
{
p->namelen = Cgetc(cd);
if(p->namelen == 0) {
Crseek(cd, (Croffset(cd)+Blocksize-1)/Blocksize * Blocksize);
p->namelen = Cgetc(cd);
assert(p->namelen != 0);
}
p->xlen = Cgetc(cd);
assert(p->xlen == 0);
Cread(cd, p->dloc, 4);
Cread(cd, p->parent, 2);
p->name[0] = '\0';
Crseek(cd, Croffset(cd)+p->namelen+p->xlen+(p->namelen&1));
}
static void
writepath(Cdimg *cd, Cdir *c, int parent, int size)
{
Cputc(cd, c->namelen);
Cputc(cd, 0);
Cwrite(cd, c->dloc + (size==Little ? 0 : 4), 4);
(size==Little ? Cputnl : Cputnm)(cd, parent, 2);
Cwrite(cd, c->name, c->namelen);
if(c->namelen & 1)
Cputc(cd, 0);
}
static ulong*
addlength(ulong *a, ulong x, int n)
{
if(n%128==0)
a = erealloc(a, (n+128)*sizeof a[0]);
a[n] = x;
return a;
}
static ulong
writepathtable(Cdimg *cd, ulong vdblock, int size)
{
int rp, wp;
uchar buf[Blocksize];
ulong bk, i, *len, n;
uvlong start, end, rdoff;
Cdir *c;
Cpath p;
Creadblock(cd, buf, vdblock, Blocksize);
c = (Cdir*)(buf + offsetof(Cvoldesc, rootdir[0]));
rp = 0;
wp = 0;
len = nil;
start = (vlong)cd->nextblock * Blocksize;
Cwseek(cd, start);
Crseek(cd, start);
writepath(cd, c, 1, size);
len = addlength(len, little(c->dlen, 4), wp);
wp++;
while(rp < wp) {
Crdpath(cd, &p);
n = (len[rp]+Blocksize-1)/Blocksize;
rp++;
bk = (size==Big ? big : little)(p.dloc, 4);
rdoff = Croffset(cd);
for(i=0; i<n; i++) {
Creadblock(cd, buf, bk+i, Blocksize);
c = (Cdir*)buf;
if(i != 0 && c->namelen == 1 && c->name[0] == '\0')
break;
while(c->len && c->namelen &&
(uchar*)c + c->len < buf + Blocksize) {
if(c->flags & 0x02 &&
(c->namelen > 1 || c->name[0] > '\001')) {
writepath(cd, c, rp, size);
len = addlength(len, little(c->dlen, 4), wp);
wp++;
}
c = (Cdir*)((uchar*)c+c->len);
}
}
Crseek(cd, rdoff);
}
end = Cwoffset(cd);
Cpadblock(cd);
return end-start;
}
static void
writepathtablepair(Cdimg *cd, ulong vdblock)
{
ulong bloc, lloc, sz, sz2;
lloc = cd->nextblock;
sz = writepathtable(cd, vdblock, Little);
bloc = cd->nextblock;
sz2 = writepathtable(cd, vdblock, Big);
assert(sz == sz2);
setpathtable(cd, vdblock, sz, lloc, bloc);
}
void
writepathtables(Cdimg *cd)
{
cd->pathblock = cd->nextblock;
writepathtablepair(cd, cd->iso9660pvd);
if(cd->flags & CDjoliet)
writepathtablepair(cd, cd->jolietsvd);
}