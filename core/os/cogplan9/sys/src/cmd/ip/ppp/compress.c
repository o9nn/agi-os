#include <u.h>
#include <libc.h>
#include <ip.h>
#include <auth.h>
#include "ppp.h"
typedef struct Iphdr Iphdr;
struct Iphdr
{
uchar	vihl;
uchar	tos;
uchar	length[2];
uchar	id[2];
uchar	frag[2];
uchar	ttl;
uchar	proto;
uchar	cksum[2];
ulong	src;
ulong	dst;
};
typedef struct Tcphdr Tcphdr;
struct Tcphdr
{
ulong	ports;
uchar	seq[4];
uchar	ack[4];
uchar	flag[2];
uchar	win[2];
uchar	cksum[2];
uchar	urg[2];
};
typedef struct Ilhdr Ilhdr;
struct Ilhdr
{
uchar	sum[2];
uchar	len[2];
uchar	type;
uchar	spec;
uchar	src[2];
uchar	dst[2];
uchar	id[4];
uchar	ack[4];
};
enum
{
URG		= 0x20,
ACK		= 0x10,
PSH		= 0x08,
RST		= 0x04,
SYN		= 0x02,
FIN		= 0x01,
IP_DF		= 0x4000,
IP_TCPPROTO	= 6,
IP_ILPROTO	= 40,
IL_IPHDR	= 20,
};
typedef struct Hdr Hdr;
struct Hdr
{
uchar	buf[128];
Iphdr	*ip;
Tcphdr	*tcp;
int	len;
};
typedef struct Tcpc Tcpc;
struct Tcpc
{
uchar	lastrecv;
uchar	lastxmit;
uchar	basexmit;
uchar	err;
uchar	compressid;
Hdr	t[MAX_STATES];
Hdr	r[MAX_STATES];
};
enum
{
NEW_U=(1<<0),
NEW_W=(1<<1),
NEW_A=(1<<2),
NEW_S=(1<<3),
NEW_P=(1<<4),
NEW_I=(1<<5),
NEW_C=(1<<6),
NEW_T=(1<<7),
TCP_PUSH_BIT	= 0x10,
};
#define SPECIAL_I (NEW_S|NEW_W|NEW_U)
#define SPECIAL_D (NEW_S|NEW_A|NEW_W|NEW_U)
#define SPECIALS_MASK (NEW_S|NEW_A|NEW_W|NEW_U)
int
encode(void *p, ulong n)
{
uchar	*cp;
cp = p;
if(n >= 256 || n == 0) {
*cp++ = 0;
cp[0] = n >> 8;
cp[1] = n;
return 3;
}
*cp = n;
return 1;
}
#define DECODEL(f) { \
if (*cp == 0) {\
hnputl(f, nhgetl(f) + ((cp[1] << 8) | cp[2])); \
cp += 3; \
} else { \
hnputl(f, nhgetl(f) + (ulong)*cp++); \
} \
}
#define DECODES(f) { \
if (*cp == 0) {\
hnputs(f, nhgets(f) + ((cp[1] << 8) | cp[2])); \
cp += 3; \
} else { \
hnputs(f, nhgets(f) + (ulong)*cp++); \
} \
}
Block*
tcpcompress(Tcpc *comp, Block *b, int *protop)
{
Iphdr	*ip;
Tcphdr	*tcp;
ulong 	iplen, tcplen, hlen;
ulong 	deltaS, deltaA;
ulong 	changes;
uchar 	new_seq[16];
uchar 	*cp;
Hdr	*h;
int 	i, j;
ip = (Iphdr*)b->rptr;
iplen = (ip->vihl & 0xf) << 2;
tcp = (Tcphdr*)(b->rptr + iplen);
tcplen = (tcp->flag[0] & 0xf0) >> 2;
hlen = iplen + tcplen;
if((tcp->flag[1] & (SYN|FIN|RST|ACK)) != ACK){
*protop = Pip;
return b;
}
changes = 0;
cp = new_seq;
j = comp->lastxmit;
h = &comp->t[j];
if(ip->src != h->ip->src || ip->dst != h->ip->dst
|| tcp->ports != h->tcp->ports) {
for(i = 0; i < MAX_STATES; ++i) {
j = (comp->basexmit + i) % MAX_STATES;
h = &comp->t[j];
if(ip->src == h->ip->src && ip->dst == h->ip->dst
&& tcp->ports == h->tcp->ports)
goto found;
}
if(i == MAX_STATES) {
j = comp->basexmit;
j = (j + MAX_STATES - 1) % MAX_STATES;
comp->basexmit = j;
h = &comp->t[j];
goto rescue;
}
}
found:
if(ip->vihl  != h->ip->vihl || ip->tos   != h->ip->tos ||
ip->ttl   != h->ip->ttl  || ip->proto != h->ip->proto)
goto rescue;
if(iplen != sizeof(Iphdr) && memcmp(ip+1, h->ip+1, iplen - sizeof(Iphdr)))
goto rescue;
if(tcplen != sizeof(Tcphdr) && memcmp(tcp+1, h->tcp+1, tcplen - sizeof(Tcphdr)))
goto rescue;
if(tcp->flag[1] & URG) {
cp += encode(cp, nhgets(tcp->urg));
changes |= NEW_U;
} else if(memcmp(tcp->urg, h->tcp->urg, sizeof(tcp->urg)) != 0)
goto rescue;
if(deltaS = nhgets(tcp->win) - nhgets(h->tcp->win)) {
cp += encode(cp, deltaS);
changes |= NEW_W;
}
if(deltaA = nhgetl(tcp->ack) - nhgetl(h->tcp->ack)) {
if(deltaA > 0xffff)
goto rescue;
cp += encode(cp, deltaA);
changes |= NEW_A;
}
if(deltaS = nhgetl(tcp->seq) - nhgetl(h->tcp->seq)) {
if (deltaS > 0xffff)
goto rescue;
cp += encode(cp, deltaS);
changes |= NEW_S;
}
switch(changes) {
case 0:
if(nhgets(ip->length) == nhgets(h->ip->length) ||
nhgets(h->ip->length) != hlen)
goto rescue;
break;
case SPECIAL_I:
case SPECIAL_D:
goto rescue;
case NEW_S | NEW_A:
if (deltaS == deltaA &&
deltaS == nhgets(h->ip->length) - hlen) {
changes = SPECIAL_I;
cp = new_seq;
}
break;
case NEW_S:
if (deltaS == nhgets(h->ip->length) - hlen) {
changes = SPECIAL_D;
cp = new_seq;
}
break;
}
deltaS = nhgets(ip->id) - nhgets(h->ip->id);
if(deltaS != 1) {
cp += encode(cp, deltaS);
changes |= NEW_I;
}
if (tcp->flag[1] & PSH)
changes |= TCP_PUSH_BIT;
deltaA = nhgets(tcp->cksum);
memmove(h->buf, b->rptr, hlen);
h->len = hlen;
h->tcp = (Tcphdr*)(h->buf + iplen);
deltaS = cp - new_seq;
cp = b->rptr;
if(comp->lastxmit != j || comp->compressid == 0) {
comp->lastxmit = j;
hlen -= deltaS + 4;
cp += hlen;
*cp++ = (changes | NEW_C);
*cp++ = j;
} else {
hlen -= deltaS + 3;
cp += hlen;
*cp++ = changes;
}
b->rptr += hlen;
hnputs(cp, deltaA);
cp += 2;
memmove(cp, new_seq, deltaS);
*protop = Pvjctcp;
return b;
rescue:
memmove(h->buf, b->rptr, hlen);
h->tcp = (Tcphdr*)(h->buf + iplen);
h->len = hlen;
ip->proto = j;
comp->lastxmit = j;
*protop = Pvjutcp;
return b;
}
Block*
tcpuncompress(Tcpc *comp, Block *b, int type)
{
uchar	*cp, changes;
int	i;
int	iplen, len;
Iphdr	*ip;
Tcphdr	*tcp;
Hdr	*h;
if(type == Pvjutcp) {
ip = (Iphdr*)b->rptr;
if(ip->proto >= MAX_STATES)
goto rescue;
iplen = (ip->vihl & 0xf) << 2;
tcp = (Tcphdr*)(b->rptr + iplen);
comp->lastrecv = ip->proto;
len = iplen + ((tcp->flag[0] & 0xf0) >> 2);
comp->err = 0;
ip->proto = IP_TCPPROTO;
h = &comp->r[comp->lastrecv];
memmove(h->buf, b->rptr, len);
h->tcp = (Tcphdr*)(h->buf + iplen);
h->len = len;
h->ip->cksum[0] = h->ip->cksum[1] = 0;
return b;
}
cp = b->rptr;
changes = *cp++;
if(changes & NEW_C) {
if(*cp >= MAX_STATES)
goto rescue;
comp->err = 0;
comp->lastrecv = *cp++;
} else {
if(comp->err != 0){
freeb(b);
return nil;
}
}
h = &comp->r[comp->lastrecv];
ip = h->ip;
tcp = h->tcp;
len = h->len;
memmove(tcp->cksum, cp, sizeof tcp->cksum);
cp += 2;
if(changes & TCP_PUSH_BIT)
tcp->flag[1] |= PSH;
else
tcp->flag[1] &= ~PSH;
switch (changes & SPECIALS_MASK) {
case SPECIAL_I:
i = nhgets(ip->length) - len;
hnputl(tcp->ack, nhgetl(tcp->ack) + i);
hnputl(tcp->seq, nhgetl(tcp->seq) + i);
break;
case SPECIAL_D:
hnputl(tcp->seq, nhgetl(tcp->seq) + nhgets(ip->length) - len);
break;
default:
if(changes & NEW_U) {
tcp->flag[1] |= URG;
if(*cp == 0){
hnputs(tcp->urg, nhgets(cp+1));
cp += 3;
}else
hnputs(tcp->urg, *cp++);
} else
tcp->flag[1] &= ~URG;
if(changes & NEW_W)
DECODES(tcp->win)
if(changes & NEW_A)
DECODEL(tcp->ack)
if(changes & NEW_S)
DECODEL(tcp->seq)
break;
}
if(changes & NEW_I)
DECODES(ip->id)
else
hnputs(ip->id, nhgets(ip->id) + 1);
b->rptr = cp;
if(b->rptr - b->base < len){
b = padb(b, len);
b = pullup(b, blen(b));
} else
b->rptr -= len;
hnputs(ip->length, BLEN(b));
memmove(b->rptr, ip, len);
ip = (Iphdr*)b->rptr;
ip->cksum[0] = ip->cksum[1] = 0;
hnputs(ip->cksum, ipcsum(b->rptr));
return b;
rescue:
netlog("ppp: vj: Bad Packet!\n");
comp->err = 1;
freeb(b);
return nil;
}
Tcpc*
compress_init(Tcpc *c)
{
int i;
Hdr *h;
if(c == nil)
c = malloc(sizeof(Tcpc));
memset(c, 0, sizeof(*c));
for(i = 0; i < MAX_STATES; i++){
h = &c->t[i];
h->ip = (Iphdr*)h->buf;
h->tcp = (Tcphdr*)(h->buf + 20);
h->len = 40;
h = &c->r[i];
h->ip = (Iphdr*)h->buf;
h->tcp = (Tcphdr*)(h->buf + 20);
h->len = 40;
}
return c;
}
Block*
compress(Tcpc *tcp, Block *b, int *protop)
{
Iphdr		*ip;
ip = (Iphdr*)b->rptr;
if((nhgets(ip->frag) & 0x3fff) != 0){
*protop = Pip;
return b;
}
switch(ip->proto) {
case IP_TCPPROTO:
return tcpcompress(tcp, b, protop);
default:
*protop = Pip;
return b;
}
}
int
compress_negotiate(Tcpc *tcp, uchar *data)
{
if(data[0] != MAX_STATES - 1)
return -1;
tcp->compressid = data[1];
return 0;
}
void
compress_error(Tcpc *tcp)
{
tcp->err = 1;
}