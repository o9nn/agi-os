#include <u.h>
#include <libc.h>
#include <mp.h>
#include <auth.h>
#include <libsec.h>
#include <ctype.h>
#include <fcall.h>
#include <thread.h>
#include <9p.h>
#include "cifs.h"
#define DEF_AUTH "ntlmv2"
static enum {
MACkeylen = 40,
MAClen = 8,
MACoff = 14,
Bliplen = 8,
};
static void
dmp(char *s, int seq, void *buf, int n)
{
int i;
char *p = buf;
print("%s %3d      ", s, seq);
while(n > 0){
for(i = 0; i < 16 && n > 0; i++, n--)
print("%02x ", *p++ & 0xff);
if(n > 0)
print("\n");
}
print("\n");
}
static Auth *
auth_plain(char *windom, char *keyp, uchar *chal, int len)
{
UserPasswd *up;
static Auth *ap;
USED(chal, len);
up = auth_getuserpasswd(auth_getkey, "windom=%s proto=pass service=cifs %s",
windom, keyp);
if(! up)
sysfatal("cannot get key - %r");
ap = emalloc9p(sizeof(Auth));
memset(ap, 0, sizeof(ap));
ap->user = estrdup9p(up->user);
ap->windom = estrdup9p(windom);
ap->resp[0] = estrdup9p(up->passwd);
ap->len[0] = strlen(up->passwd);
memset(up->passwd, 0, strlen(up->passwd));
free(up);
return ap;
}
static Auth *
auth_lm_and_ntlm(char *windom, char *keyp, uchar *chal, int len)
{
int err;
char user[64];
Auth *ap;
MSchapreply mcr;
err = auth_respond(chal, len, user, sizeof user, &mcr, sizeof mcr,
auth_getkey, "windom=%s proto=mschap role=client service=cifs %s",
windom, keyp);
if(err == -1)
sysfatal("cannot get key - %r");
ap = emalloc9p(sizeof(Auth));
memset(ap, 0, sizeof(ap));
ap->user = estrdup9p(user);
ap->windom = estrdup9p(windom);
ap->len[0] = sizeof(mcr.LMresp);
ap->resp[0] = emalloc9p(ap->len[0]);
memcpy(ap->resp[0], mcr.LMresp, ap->len[0]);
ap->len[1] = sizeof(mcr.NTresp);
ap->resp[1] = emalloc9p(ap->len[1]);
memcpy(ap->resp[1], mcr.NTresp, ap->len[1]);
return ap;
}
static Auth *
auth_ntlm(char *windom, char *keyp, uchar *chal, int len)
{
Auth *ap;
if((ap = auth_lm_and_ntlm(windom, keyp, chal, len)) == nil)
return nil;
free(ap->resp[0]);
ap->len[0] = ap->len[1];
ap->resp[0] = emalloc9p(ap->len[0]);
memcpy(ap->resp[0], ap->resp[1], ap->len[0]);
return ap;
}
static DigestState *
hmac_t64(uchar *data, ulong dlen, uchar *key, ulong klen, uchar *digest,
DigestState *state)
{
if(klen > 64)
klen = 64;
return hmac_md5(data, dlen, key, klen, digest, state);
}
static int
ntv2_blob(uchar *blob, int len, char *windom)
{
int n;
uvlong nttime;
Rune r;
char *d;
uchar *p;
enum {
Beof,
Bnetbios,
Bdomain,
Bdnsfqdn,
Bdnsname,
};
p = blob;
*p++ = 1;
*p++ = 1;
*p++ = 0;
*p++ = 0;
*p++ = 0;
*p++ = 0;
*p++ = 0;
*p++ = 0;
nttime = time(nil);
nttime += 11644473600LL;
nttime *= 10000000LL;
*p++ = nttime;
*p++ = nttime >> 8;
*p++ = nttime >> 16;
*p++ = nttime >> 24;
*p++ = nttime >> 32;
*p++ = nttime >> 40;
*p++ = nttime >> 48;
*p++ = nttime >> 56;
genrandom(p, 8);
p += 8;
*p++ = 0x6f;
*p++ = 0;
*p++ = 0x6e;
*p++ = 0;
*p++ = Bdomain;
*p++ = 0;
n = utflen(windom) * 2;
*p++ = n;
*p++ = n >> 8;
d = windom;
while(*d && p-blob < (len-8)){
d += chartorune(&r, d);
r = toupperrune(r);
*p++ = r;
*p++ = r >> 8;
}
*p++ = 0;
*p++ = Beof;
*p++ = 0;
*p++ = 0;
*p++ = 0x65;
*p++ = 0;
*p++ = 0;
*p++ = 0;
return p - blob;
}
static Auth *
auth_ntlmv2(char *windom, char *keyp, uchar *chal, int len)
{
int i, n;
Rune r;
char *p, *u;
uchar v1hash[MD5dlen], blip[Bliplen], blob[1024], v2hash[MD5dlen];
uchar c, lm_hmac[MD5dlen], nt_hmac[MD5dlen], nt_sesskey[MD5dlen],
lm_sesskey[MD5dlen];
DigestState *ds;
UserPasswd *up;
static Auth *ap;
up = auth_getuserpasswd(auth_getkey, "windom=%s proto=pass  service=cifs-ntlmv2 %s",
windom, keyp);
if(!up)
sysfatal("cannot get key - %r");
ap = emalloc9p(sizeof(Auth));
memset(ap, 0, sizeof(ap));
if((n = strlen(up->passwd)) > 128)
n = 128;
ds = md4(nil, 0, nil, nil);
for(i=0, p=up->passwd; i < n; i++) {
p += chartorune(&r, p);
c = r;
md4(&c, 1, nil, ds);
c = r >> 8;
md4(&c, 1, nil, ds);
}
md4(nil, 0, v1hash, ds);
ds = hmac_t64(nil, 0, v1hash, MD5dlen, nil, nil);
u = up->user;
while(*u){
u += chartorune(&r, u);
r = toupperrune(r);
c = r;
hmac_t64(&c, 1, v1hash, MD5dlen, nil, ds);
c = r >> 8;
hmac_t64(&c, 1, v1hash, MD5dlen, nil, ds);
}
u = windom;
while(*u){
u += chartorune(&r, u);
c = r;
hmac_t64(&c, 1, v1hash, MD5dlen, nil, ds);
c = r >> 8;
hmac_t64(&c, 1, v1hash, MD5dlen, nil, ds);
}
hmac_t64(nil, 0, v1hash, MD5dlen, v2hash, ds);
ap->user = estrdup9p(up->user);
ap->windom = estrdup9p(windom);
genrandom(blip, Bliplen);
ds = hmac_t64(chal, len, v2hash, MD5dlen, nil, nil);
hmac_t64(blip, Bliplen, v2hash, MD5dlen, lm_hmac, ds);
ap->len[0] = MD5dlen+Bliplen;
ap->resp[0] = emalloc9p(ap->len[0]);
memcpy(ap->resp[0], lm_hmac, MD5dlen);
memcpy(ap->resp[0]+MD5dlen, blip, Bliplen);
hmac_t64(lm_hmac, MD5dlen, v2hash, MD5dlen, lm_sesskey, nil);
ap->mackey[0] = emalloc9p(MACkeylen);
memcpy(ap->mackey[0], lm_sesskey, MD5dlen);
memcpy(ap->mackey[0]+MD5dlen, ap->resp[0], MACkeylen-MD5dlen);
n = ntv2_blob(blob, sizeof(blob), windom);
ds = hmac_t64(chal, len, v2hash, MD5dlen, nil, nil);
hmac_t64(blob, n, v2hash, MD5dlen, nt_hmac, ds);
ap->len[1] = MD5dlen+n;
ap->resp[1] = emalloc9p(ap->len[1]);
memcpy(ap->resp[1], nt_hmac, MD5dlen);
memcpy(ap->resp[1]+MD5dlen, blob, n);
hmac_t64(nt_hmac, MD5dlen, v2hash, MD5dlen, nt_sesskey, nil);
ap->mackey[1] = emalloc9p(MACkeylen);
memcpy(ap->mackey[1], nt_sesskey, MD5dlen);
memcpy(ap->mackey[1]+MD5dlen, ap->resp[1], MACkeylen-MD5dlen);
free(up);
return ap;
}
struct {
char *name;
Auth *(*func)(char *, char *, uchar *, int);
} methods[] = {
{ "plain", auth_plain },
{ "lm+ntlm", auth_lm_and_ntlm },
{ "ntlm", auth_ntlm },
{ "ntlmv2", auth_ntlmv2 },
};
void
autherr(void)
{
int i;
fprint(2, "supported auth methods:\t");
for(i = 0; i < nelem(methods); i++)
fprint(2, "%s ", methods[i].name);
fprint(2, "\n");
exits("usage");
}
Auth *
getauth(char *name, char *windom, char *keyp, int secmode, uchar *chal, int len)
{
int i;
Auth *ap;
if(name == nil){
name = DEF_AUTH;
if((secmode & SECMODE_PW_ENCRYPT) == 0)
sysfatal("plaintext authentication required, use '-a plain'");
}
ap = nil;
for(i = 0; i < nelem(methods); i++)
if(strcmp(methods[i].name, name) == 0){
ap = methods[i].func(windom, keyp, chal, len);
break;
}
if(! ap){
fprint(2, "%s: %s - unknown auth method\n", argv0, name);
autherr();
}
return ap;
}
static int
genmac(uchar *buf, int len, int seq, uchar key[MACkeylen], uchar ours[MAClen])
{
DigestState *ds;
uchar *sig, digest[MD5dlen], theirs[MAClen];
sig = buf+MACoff;
memcpy(theirs, sig, MAClen);
memset(sig, 0, MAClen);
sig[0] = seq;
sig[1] = seq >> 8;
sig[2] = seq >> 16;
sig[3] = seq >> 24;
ds = md5(key, MACkeylen, nil, nil);
md5(buf, len, digest, ds);
memcpy(ours, digest, MAClen);
return memcmp(theirs, ours, MAClen);
}
int
macsign(Pkt *p, int seq)
{
int rc, len;
uchar *sig, *buf, mac[MAClen];
sig = p->buf + NBHDRLEN + MACoff;
buf = p->buf + NBHDRLEN;
len = (p->pos - p->buf) - NBHDRLEN;
#ifdef DEBUG_MAC
if(seq & 1)
dmp("rx", seq, sig, MAClen);
#endif
rc = 0;
if(! p->s->seqrun)
memcpy(mac, "BSRSPYL ", 8);
else
rc = genmac(buf, len, seq, p->s->auth->mackey[0], mac);
#ifdef DEBUG_MAC
if(!(seq & 1))
dmp("tx", seq, mac, MAClen);
#endif
memcpy(sig, mac, MAClen);
return rc;
}