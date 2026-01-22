#include <u.h>
#include <libc.h>
#include <libsec.h>
static void encode32(uchar*, u32int*, ulong);
static DigestState* sha2_64(uchar *, ulong, uchar *, SHA2_256state *, int);
extern void _sha2block64(uchar*, ulong, u32int*);
SHA2_224state*
sha2_224(uchar *p, ulong len, uchar *digest, SHA2_224state *s)
{
if(s == nil) {
s = mallocz(sizeof(*s), 1);
if(s == nil)
return nil;
s->malloced = 1;
}
if(s->seeded == 0){
s->state[0] = 0xc1059ed8;
s->state[1] = 0x367cd507;
s->state[2] = 0x3070dd17;
s->state[3] = 0xf70e5939;
s->state[4] = 0xffc00b31;
s->state[5] = 0x68581511;
s->state[6] = 0x64f98fa7;
s->state[7] = 0xbefa4fa4;
s->seeded = 1;
}
return sha2_64(p, len, digest, s, SHA2_224dlen);
}
SHA2_256state*
sha2_256(uchar *p, ulong len, uchar *digest, SHA2_256state *s)
{
if(s == nil) {
s = mallocz(sizeof(*s), 1);
if(s == nil)
return nil;
s->malloced = 1;
}
if(s->seeded == 0){
s->state[0] = 0x6a09e667;
s->state[1] = 0xbb67ae85;
s->state[2] = 0x3c6ef372;
s->state[3] = 0xa54ff53a;
s->state[4] = 0x510e527f;
s->state[5] = 0x9b05688c;
s->state[6] = 0x1f83d9ab;
s->state[7] = 0x5be0cd19;
s->seeded = 1;
}
return sha2_64(p, len, digest, s, SHA2_256dlen);
}
static DigestState*
sha2_64(uchar *p, ulong len, uchar *digest, SHA2_256state *s, int dlen)
{
int i;
u32int x[16];
uchar buf[128];
uchar *e;
if(s->blen){
i = 64 - s->blen;
if(len < i)
i = len;
memmove(s->buf + s->blen, p, i);
len -= i;
s->blen += i;
p += i;
if(s->blen == 64){
_sha2block64(s->buf, s->blen, s->state);
s->len += s->blen;
s->blen = 0;
}
}
i = len & ~(64-1);
if(i){
_sha2block64(p, i, s->state);
s->len += i;
len -= i;
p += i;
}
if(digest == 0){
if(len){
memmove(s->buf, p, len);
s->blen += len;
}
return s;
}
if(s->blen){
p = s->buf;
len = s->blen;
} else {
memmove(buf, p, len);
p = buf;
}
s->len += len;
e = p + len;
if(len < 56)
i = 56 - len;
else
i = 120 - len;
memset(e, 0, i);
*e = 0x80;
len += i;
x[0] = s->len>>29;
x[1] = s->len<<3;
encode32(p+len, x, 8);
_sha2block64(p, len+8, s->state);
s->len += len+8;
encode32(digest, s->state, dlen);
if(s->malloced == 1)
free(s);
return nil;
}
static void
encode32(uchar *output, u32int *input, ulong len)
{
u32int x;
uchar *e;
for(e = output + len; output < e;) {
x = *input++;
*output++ = x >> 24;
*output++ = x >> 16;
*output++ = x >> 8;
*output++ = x;
}
}
DigestState*
hmac_sha2_224(uchar *p, ulong len, uchar *key, ulong klen, uchar *digest,
DigestState *s)
{
return hmac_x(p, len, key, klen, digest, s, sha2_224, SHA2_224dlen);
}
DigestState*
hmac_sha2_256(uchar *p, ulong len, uchar *key, ulong klen, uchar *digest,
DigestState *s)
{
return hmac_x(p, len, key, klen, digest, s, sha2_256, SHA2_256dlen);
}