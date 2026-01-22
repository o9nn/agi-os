typedef struct PPP PPP;
typedef struct Pstate Pstate;
typedef struct Lcpmsg Lcpmsg;
typedef struct Lcpopt Lcpopt;
typedef struct Qualpkt Qualpkt;
typedef struct Qualstats Qualstats;
typedef struct Tcpc Tcpc;
typedef uchar Ipaddr[IPaddrlen];
enum
{
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
Pvjctcp= 0x2d,
Pvjutcp= 0x2f,
Pcdata= 0xfd,
Pipcp= 0x8021,
Pecp= 0x8053,
Pccp= 0x80fd,
Plcp= 0xc021,
Ppap= 0xc023,
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
Omtu= 1,
Octlmap= 2,
Oauth= 3,
Oquality= 4,
Omagic= 5,
Opc= 7,
Oac= 8,
Obad= 12,
APmd5= 5,
Fmtu= 1<<Omtu,
Fctlmap= 1<<Octlmap,
Fauth= 1<<Oauth,
Fquality= 1<<Oquality,
Fmagic= 1<<Omagic,
Fpc= 1<<Opc,
Fac= 1<<Oac,
Fbad= 1<<Obad,
Cchallenge= 1,
Cresponse= 2,
Csuccess= 3,
Cfailure= 4,
Cpapreq= 1,
Cpapack= 2,
Cpapnak= 3,
Sclosed= 0,
Sclosing,
Sreqsent,
Sackrcvd,
Sacksent,
Sopened,
Ocoui= 0,
Ocstac= 17,
Ocmppc= 18,
Fcoui= 1<<Ocoui,
Fcstac= 1<<Ocstac,
Fcmppc= 1<<Ocmppc,
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
Period= 3*1000,
Timeout= 10,
MAX_STATES = 16,
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
struct Qualstats
{
ulong reports;
ulong packets;
ulong bytes;
ulong discards;
ulong errors;
};
struct PPP
{
QLock;
Chan* dchan;
Chan* cchan;
int framing;
Ipaddr local;
int localfrozen;
Ipaddr remote;
int remotefrozen;
int pppup;
Fs *f;
Ipifc* ifc;
Proc* readp;
Proc* timep;
Block* inbuf;
Block* outbuf;
QLock outlock;
ulong magic;
ulong rctlmap;
ulong xctlmap;
int phase;
Pstate* lcp;
Pstate* ipcp;
char secret[256];
char chapname[256];
Tcpc* ctcp;
ulong mtu;
ulong mru;
int baud;
int usepap;
int papid;
int usechap;
int usedns;
Ipaddr dns1;
Ipaddr dns2;
int period;
int timeout;
Qualstats in;
Qualstats out;
Qualstats pin;
Qualstats pout;
Qualstats sin;
};
PPP* pppopen(PPP*, char*, Ipaddr, Ipaddr, int, int, char*, char*);
Block* pppread(PPP*);
int pppwrite(PPP*, Block*);
void pppclose(PPP*);
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
uchar lastoutbytes[4];
uchar peerinreports[4];
uchar peerinpackets[4];
uchar peerindiscards[4];
uchar peerinerrors[4];
uchar peerinbytes[4];
uchar peeroutreports[4];
uchar peeroutpackets[4];
uchar peeroutbytes[4];
};
ushort compress(Tcpc*, Block*, Fs*);
Tcpc* compress_init(Tcpc*);
int compress_negotiate(Tcpc*, uchar*);
ushort tcpcompress(Tcpc*, Block*, Fs*);
Block* tcpuncompress(Tcpc*, Block*, ushort, Fs*);