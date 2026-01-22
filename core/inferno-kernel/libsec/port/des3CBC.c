#include "os.h"
#include <mp.h>
#include <libsec.h>
void
des3CBCencrypt(uchar *p, int len, DES3state *s)
{
uchar *p2, *ip, *eip;
for(; len >= 8; len -= 8){
p2 = p;
ip = s->ivec;
for(eip = ip+8; ip < eip; )
*p2++ ^= *ip++;
triple_block_cipher(s->expanded, p, DES3EDE);
memmove(s->ivec, p, 8);
p += 8;
}
if(len > 0){
ip = s->ivec;
triple_block_cipher(s->expanded, ip, DES3EDE);
for(eip = ip+len; ip < eip; )
*p++ ^= *ip++;
}
}
void
des3CBCdecrypt(uchar *p, int len, DES3state *s)
{
uchar *ip, *eip, *tp;
uchar tmp[8];
for(; len >= 8; len -= 8){
memmove(tmp, p, 8);
triple_block_cipher(s->expanded, p, DES3DED);
tp = tmp;
ip = s->ivec;
for(eip = ip+8; ip < eip; ){
*p++ ^= *ip;
*ip++ = *tp++;
}
}
if(len > 0){
ip = s->ivec;
triple_block_cipher(s->expanded, ip, DES3EDE);
for(eip = ip+len; ip < eip; )
*p++ ^= *ip++;
}
}