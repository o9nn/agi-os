#include	"lib9.h"
#include	<bio.h>
void*
Brdline(Biobuf *bp, int delim)
{
char *ip, *ep;
int i, j;
i = -bp->icount;
if(i == 0) {
if(bp->state != Bractive) {
if(bp->state == Bracteof)
bp->state = Bractive;
bp->rdline = 0;
bp->gbuf = bp->ebuf;
return 0;
}
}
ip = (char*)bp->ebuf - i;
ep = memchr(ip, delim, i);
if(ep) {
j = (ep - ip) + 1;
bp->rdline = j;
bp->icount += j;
return ip;
}
if(i < bp->bsize)
memmove(bp->bbuf, ip, i);
bp->gbuf = bp->bbuf;
ip = (char*)bp->bbuf + i;
while(i < bp->bsize) {
j = read(bp->fid, ip, bp->bsize-i);
if(j <= 0) {
memmove(bp->ebuf-i, bp->bbuf, i);
bp->rdline = i;
bp->icount = -i;
bp->gbuf = bp->ebuf-i;
return 0;
}
bp->offset += j;
i += j;
ep = memchr(ip, delim, j);
if(ep) {
ip = (char*)bp->ebuf - i;
if(i < bp->bsize){
memmove(ip, bp->bbuf, i);
bp->gbuf = (uchar*)ip;
}
j = (ep - (char*)bp->bbuf) + 1;
bp->rdline = j;
bp->icount = j - i;
return ip;
}
ip += j;
}
bp->rdline = bp->bsize;
bp->icount = -bp->bsize;
bp->gbuf = bp->bbuf;
return 0;
}
int
Blinelen(Biobuf *bp)
{
return bp->rdline;
}