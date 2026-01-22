#include "os.h"
#include <mp.h>
#include <libsec.h>
void
des3ECBencrypt(uchar *p, int len, DES3state *s)
{
int i;
uchar tmp[8];
for(; len >= 8; len -= 8){
triple_block_cipher(s->expanded, p, DES3EDE);
p += 8;
}
if(len > 0){
for (i=0; i<8; i++)
tmp[i] = i;
triple_block_cipher(s->expanded, tmp, DES3EDE);
for (i = 0; i < len; i++)
p[i] ^= tmp[i];
}
}
void
des3ECBdecrypt(uchar *p, int len, DES3state *s)
{
int i;
uchar tmp[8];
for(; len >= 8; len -= 8){
triple_block_cipher(s->expanded, p, DES3DED);
p += 8;
}
if(len > 0){
for (i=0; i<8; i++)
tmp[i] = i;
triple_block_cipher(s->expanded, tmp, DES3EDE);
for (i = 0; i < len; i++)
p[i] ^= tmp[i];
}
}