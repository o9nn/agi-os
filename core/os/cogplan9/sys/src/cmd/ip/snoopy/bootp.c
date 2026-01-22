#include <u.h>
#include <libc.h>
#include <ip.h>
#include "dat.h"
#include "protos.h"
enum
{
OfferTimeout=	60,
MaxLease=	60*60,
MinLease=	15*60,
StaticLease=	30*60,
IPUDPHDRSIZE=	28,
MINSUPPORTED=	576,
Maxhwlen=	16,
Maxfilelen=	128,
Maxoptlen=	312-4,
Bootrequest=	1,
Bootreply= 	2,
Fbroadcast=	1<<15,
};
typedef struct Hdr	Hdr;
struct Hdr
{
uchar	op;
uchar	htype;
uchar	hlen;
uchar	hops;
uchar	xid[4];
uchar	secs[2];
uchar	flags[2];
uchar	ciaddr[IPv4addrlen];
uchar	yiaddr[IPv4addrlen];
uchar	siaddr[IPv4addrlen];
uchar	giaddr[IPv4addrlen];
uchar	chaddr[Maxhwlen];
char	sname[64];
char	file[Maxfilelen];
uchar	optmagic[4];
uchar	optdata[Maxoptlen];
};
enum
{
Oca,
Osa,
Ot,
};
static Field p_fields[] =
{
{"ca",		Fv4ip,	Oca,	"client IP addr",	} ,
{"sa",		Fv4ip,	Osa,	"server IP addr",	} ,
{0}
};
#define plan9opt ((ulong)(('p'<<24) | ('9'<<16) | (' '<<8) | ' '))
#define genericopt (0x63825363UL)
static Mux p_mux[] =
{
{"dhcp", 	genericopt,},
{"plan9bootp",	plan9opt,},
{"dump",	0,},
{0}
};
static void
p_compile(Filter *f)
{
Mux *m;
if(f->op == '='){
compile_cmp(bootp.name, f, p_fields);
return;
}
for(m = p_mux; m->name != nil; m++)
if(strcmp(f->s, m->name) == 0){
f->pr = m->pr;
f->ulv = m->val;
f->subop = Ot;
return;
}
sysfatal("unknown bootp field: %s", f->s);
}
static int
p_filter(Filter *f, Msg *m)
{
Hdr *h;
h = (Hdr*)m->ps;
if(m->pe < (uchar*)h->sname)
return 0;
m->ps = h->optdata;
switch(f->subop){
case Oca:
return NetL(h->ciaddr) == f->ulv || NetL(h->yiaddr) == f->ulv;
case Osa:
return NetL(h->siaddr) == f->ulv;
case Ot:
return NetL(h->optmagic) == f->ulv;
}
return 0;
}
static char*
op(int i)
{
static char x[20];
switch(i){
case Bootrequest:
return "Req";
case Bootreply:
return "Rep";
default:
sprint(x, "%d", i);
return x;
}
}
static int
p_seprint(Msg *m)
{
Hdr *h;
ulong x;
h = (Hdr*)m->ps;
if(m->pe < (uchar*)h->sname)
return -1;
m->ps = h->optdata;
m->pr = nil;
if(m->pe >= (uchar*)h->optdata){
x = NetL(h->optmagic);
demux(p_mux, x, x, m, &dump);
}
m->p = seprint(m->p, m->e, "t=%s ht=%d hl=%d hp=%d xid=%ux sec=%d fl=%4.4ux ca=%V ya=%V sa=%V ga=%V cha=%E magic=%lux",
op(h->op), h->htype, h->hlen, h->hops,
NetL(h->xid), NetS(h->secs), NetS(h->flags),
h->ciaddr, h->yiaddr, h->siaddr, h->giaddr, h->chaddr,
(ulong)NetL(h->optmagic));
if(m->pe > (uchar*)h->sname && *h->sname)
m->p = seprint(m->p, m->e, " snam=%s", h->sname);
if(m->pe > (uchar*)h->file && *h->file)
m->p = seprint(m->p, m->e, " file=%s", h->file);
return 0;
}
Proto bootp =
{
"bootp",
p_compile,
p_filter,
p_seprint,
p_mux,
"%#.8lux",
p_fields,
defaultframer,
};