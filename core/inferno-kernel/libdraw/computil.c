#include "lib9.h"
#include "draw.h"
void
_twiddlecompressed(uchar *buf, int n)
{
uchar *ebuf;
int j, k, c;
ebuf = buf+n;
while(buf < ebuf){
c = *buf++;
if(c >= 128){
k = c-128+1;
for(j=0; j<k; j++, buf++)
*buf ^= 0xFF;
}else
buf++;
}
}
int
_compblocksize(Rectangle r, int depth)
{
int bpl;
bpl = bytesperline(r, depth);
bpl = 2*bpl;
if(bpl < NCBLOCK)
return NCBLOCK;
return bpl;
}