#include "os.h"
#include <libsec.h>
static void encode(uchar*, u32int*, ulong);
extern void _md5block(uchar*, ulong, u32int*);
MD5state*
md5(uchar *p, ulong len, uchar *digest, MD5state *s)
{
u32int x[16];
uchar buf[128];
int i;
uchar *e;
if(s == nil){
s = malloc(sizeof(*s));
if(s == nil)
return nil;
memset(s, 0, sizeof(*s));
s->malloced = 1;
}
if(s->seeded == 0){
s->state[0] = 0x67452301;
s->state[1] = 0xefcdab89;
s->state[2] = 0x98badcfe;
s->state[3] = 0x10325476;
s->seeded = 1;
}
if(s->blen){
i = 64 - s->blen;
if(len < i)
i = len;
memmove(s->buf + s->blen, p, i);
len -= i;
s->blen += i;
p += i;
if(s->blen == 64){
_md5block(s->buf, s->blen, s->state);
s->len += s->blen;
s->blen = 0;
}
}
i = len & ~0x3f;
if(i){
_md5block(p, i, s->state);
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
x[0] = s->len<<3;
x[1] = s->len>>29;
encode(p+len, x, 8);
_md5block(p, len+8, s->state);
s->len += len;
encode(digest, s->state, MD5dlen);
if(s->malloced == 1)
free(s);
return nil;
}
static void
encode(uchar *output, u32int *input, ulong len)
{
u32int x;
uchar *e;
for(e = output + len; output < e;) {
x = *input++;
*output++ = x;
*output++ = x >> 8;
*output++ = x >> 16;
*output++ = x >> 24;
}
}
DigestState*
hmac_md5(uchar *p, ulong len, uchar *key, ulong klen, uchar *digest,
DigestState *s)
{
return hmac_x(p, len, key, klen, digest, s, md5, MD5dlen);
}