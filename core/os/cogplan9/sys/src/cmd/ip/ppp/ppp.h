typedef struct Tcpc Tcpc;
typedef struct Pstate Pstate;
typedef struct Chap Chap;
typedef struct Qualstats Qualstats;
typedef struct Comptype Comptype;
typedef struct Uncomptype Uncomptype;
typedef struct PPP PPP;
typedef struct Lcpmsg Lcpmsg;
typedef struct Lcpopt Lcpopt;
typedef struct Qualpkt Qualpkt;
typedef struct Block Block;
typedef uchar Ipaddr[IPaddrlen];
#pragma incomplete Tcpc
struct Block
{
Block *next;
Block *flist;
Block *list;
uchar *rptr;
uchar *wptr;
uchar *lim;
uchar *base;
uchar flags;
void *flow;
ulong pc;
ulong bsz;
};
#define BLEN(b) ((b)->wptr-(b)->rptr)
enum
{
S_DELIM = (1<<0),
S_HANGUP = (1<<1),
S_RHANGUP = (1<<2),
QHUNGUP = (1<<0),
QFLOW = (1<<1),
};
Block* allocb(int);
void freeb(Block*);
Block* concat(Block*);
int blen(Block*);
Block* pullup(Block*, int);
Block* padb(Block*, int);
Block* btrim(Block*, int, int);
Block* copyb(Block*, int);
int pullb(Block**, int);
enum {
HDLC_frame= 0x7e,
HDLC_esc= 0x7d,
PPP_addr= 0xff,
PPP_ctl= 0x3,
PPP_initfcs= 0xffff,
PPP_goodfcs= 0xf0b8,
Pdead= 0,
Plink,
Pauth,
Pnet,
Pterm,
Pip= 0x21,
Pipv6= 0x57,
Pvjctcp= 0x2d,
Pvjutcp= 0x2f,
Pcdata= 0xfd,
Pipcp= 0x8021,
Pecp= 0x8053,
Pccp= 0x80fd,
Plcp= 0xc021,
Ppasswd= 0xc023,
Plqm= 0xc025,
Pchap= 0xc223,
Lconfreq= 1,
Lconfack= 2,
Lconfnak= 3,
Lconfrej= 4,
Ltermreq= 5,
Ltermack= 6,
Lcoderej= 7,
Lprotorej= 8,
Lechoreq= 9,
Lechoack= 10,
Ldiscard= 11,
Lresetreq= 14,
Lresetack= 15,
Omtu= 1,
Octlmap= 2,
Oauth= 3,
Oquality= 4,
Omagic= 5,
Opc= 7,
Oac= 8,
APmd5= 5,
APmschap= 128,
APpasswd= Ppasswd,
Fmtu= 1<<Omtu,
Fctlmap= 1<<Octlmap,
Fauth= 1<<Oauth,
Fquality= 1<<Oquality,
Fmagic= 1<<Omagic,
Fpc= 1<<Opc,
Fac= 1<<Oac,
Cchallenge= 1,
Cresponse= 2,
Csuccess= 3,
Cfailure= 4,
Pauthreq= 1,
Pauthack= 2,
Pauthnak= 3,
Cunauth= 0,
Cchalsent,
Cauthfail,
Cauthok,
Sclosed= 0,
Sclosing,
Sreqsent,
Sackrcvd,
Sacksent,
Sopened,
Ocoui= 0,
Ocstac= 17,
Ocmppc= 18,
Octhwack= 31,
Fcoui= 1<<Ocoui,
Fcstac= 1<<Ocstac,
Fcmppc= 1<<Ocmppc,
Fcthwack= 1<<Octhwack,
Oeoui= 0,
Oedese= 1,
Feoui= 1<<Oeoui,
Fedese= 1<<Oedese,
Oipaddrs= 1,
Oipcompress= 2,
Oipaddr= 3,
Oipdns= 129,
Oipwins= 130,
Oipdns2= 131,
Oipwins2= 132,
Fipaddrs= 1<<Oipaddrs,
Fipcompress= 1<<Oipcompress,
Fipaddr= 1<<Oipaddr,
Fipdns= 1<<8,
Fipwins= 1<<9,
Fipdns2= 1<<10,
Fipwins2= 1<<11,
Period= 5*1000,
Timeout= 20,
Buflen= 4096,
MAX_STATES= 16,
Defmtu= 1450,
Minmtu= 128,
Maxmtu= 2000,
};
struct Pstate
{
int proto;
int timeout;
int rxtimeout;
ulong flags;
uchar id;
uchar confid;
uchar termid;
uchar rcvdconfid;
uchar state;
ulong optmask;
int echoack;
int echotimeout;
};
struct Chap
{
int proto;
int state;
uchar id;
int timeout;
Chalstate *cs;
};
struct Qualstats
{
ulong reports;
ulong packets;
ulong uchars;
ulong discards;
ulong errors;
};
struct Comptype
{
void* (*init)(PPP*);
Block* (*compress)(PPP*, ushort, Block*, int*);
Block* (*resetreq)(void*, Block*);
void (*fini)(void*);
};
struct Uncomptype
{
void* (*init)(PPP*);
Block* (*uncompress)(PPP*, Block*, int*, Block**);
void (*resetack)(void*, Block*);
void (*fini)(void*);
};
struct PPP
{
QLock;
int ipfd;
int ipcfd;
int mediain;
int mediaout;
char *net;
int framing;
Ipaddr local;
Ipaddr curlocal;
int localfrozen;
Ipaddr remote;
Ipaddr curremote;
int remotefrozen;
Ipaddr dns[2];
Ipaddr wins[2];
Block* inbuf;
Block* outbuf;
QLock outlock;
ulong magic;
ulong rctlmap;
ulong xctlmap;
int phase;
Pstate* lcp;
Pstate* ccp;
Pstate* ipcp;
Chap* chap;
Tcpc* ctcp;
ulong mtu;
ulong mru;
int ctries;
Comptype *ctype;
void *cstate;
Uncomptype *unctype;
void *uncstate;
uchar key[16];
int sendencrypted;
char secret[256];
char chapname[256];
int period;
int timeout;
Qualstats in;
Qualstats out;
Qualstats pin;
Qualstats pout;
Qualstats sin;
struct {
ulong ipsend;
ulong iprecv;
ulong iprecvbadsrc;
ulong iprecvnotup;
ulong comp;
ulong compin;
ulong compout;
ulong compreset;
ulong uncomp;
ulong uncompin;
ulong uncompout;
ulong uncompreset;
ulong vjin;
ulong vjout;
ulong vjfail;
} stat;
};
extern Block* pppread(PPP*);
extern int pppwrite(PPP*, Block*);
extern void pppopen(PPP*, int, int, char*, Ipaddr, Ipaddr, int, int);
struct Lcpmsg
{
uchar code;
uchar id;
uchar len[2];
uchar data[1];
};
struct Lcpopt
{
uchar type;
uchar len;
uchar data[1];
};
struct Qualpkt
{
uchar magic[4];
uchar lastoutreports[4];
uchar lastoutpackets[4];
uchar lastoutuchars[4];
uchar peerinreports[4];
uchar peerinpackets[4];
uchar peerindiscards[4];
uchar peerinerrors[4];
uchar peerinuchars[4];
uchar peeroutreports[4];
uchar peeroutpackets[4];
uchar peeroutuchars[4];
};
extern Block* compress(Tcpc*, Block*, int*);
extern void compress_error(Tcpc*);
extern Tcpc* compress_init(Tcpc*);
extern int compress_negotiate(Tcpc*, uchar*);
extern Block* tcpcompress(Tcpc*, Block*, int*);
extern Block* tcpuncompress(Tcpc*, Block*, int);
extern Block* alloclcp(int, int, int, Lcpmsg**);
extern ushort ptclcsum(Block*, int, int);
extern ushort ptclbsum(uchar*, int);
extern ushort ipcsum(uchar*);
extern Comptype cmppc;
extern Uncomptype uncmppc;
extern Comptype cthwack;
extern Uncomptype uncthwack;
extern void netlog(char*, ...);
#pragma varargck argpos netlog 1
extern char *LOG;