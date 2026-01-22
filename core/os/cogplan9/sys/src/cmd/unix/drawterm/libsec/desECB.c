#include "os.h"
#include <mp.h>
#include <libsec.h>
void
desECBencrypt(uchar *p, int len, DESstate *s)
{
int i;
uchar tmp[8];
for(; len >= 8; len -= 8){
block_cipher(s->expanded, p, 0);
p += 8;
}
if(len > 0){
for (i=0; i<8; i++)
tmp[i] = i;
block_cipher(s->expanded, tmp, 0);
for (i = 0; i < len; i++)
p[i] ^= tmp[i];
}
}
void
desECBdecrypt(uchar *p, int len, DESstate *s)
{
int i;
uchar tmp[8];
for(; len >= 8; len -= 8){
block_cipher(s->expanded, p, 1);
p += 8;
}
if(len > 0){
for (i=0; i<8; i++)
tmp[i] = i;
block_cipher(s->expanded, tmp, 0);
for (i = 0; i < len; i++)
p[i] ^= tmp[i];
}
}