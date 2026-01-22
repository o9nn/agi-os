#define isv6mcast(addr)	  ((addr)[0] == 0xff)
#define islinklocal(addr) ((addr)[0] == 0xfe && ((addr)[1] & 0xc0) == 0x80)
#define optexsts(np)	(nhgets((np)->ploadlen) > 24)
#define issmcast(addr)	(memcmp((addr), v6solicitednode, 13) == 0)
enum {
HBH		= 0,
ICMP		= 1,
IGMP		= 2,
GGP		= 3,
IPINIP		= 4,
ST		= 5,
TCP		= 6,
UDP		= 17,
ISO_TP4		= 29,
RH		= 43,
FH		= 44,
IDRP		= 45,
RSVP		= 46,
AH		= 51,
ESP		= 52,
ICMPv6		= 58,
NNH		= 59,
DOH		= 60,
ISO_IP		= 80,
IGRP		= 88,
OSPF		= 89,
Maxhdrtype	= 256,
};
enum {
Link_local_scop	= 2,
Global_scop	= 14,
SOLN_PREF_LEN	= 13,
Icmp6_no_route		= 0,
Icmp6_ad_prohib		= 1,
Icmp6_out_src_scope	= 2,
Icmp6_adr_unreach	= 3,
Icmp6_port_unreach	= 4,
Icmp6_gress_src_fail	= 5,
Icmp6_rej_route		= 6,
Icmp6_unknown		= 7,
v6MINTU		= 1280,
HOP_LIMIT	= 255,
IP6HDR		= 40,
SRC_LLADDR	= 1,
TARGET_LLADDR	= 2,
PREFIX_INFO	= 3,
REDIR_HEADER	= 4,
MTU_OPTION	= 5,
V6nd_home	= 8,
V6nd_srcaddrs	= 9,
V6nd_ip		= 17,
V6nd_rdns	= 25,
V6nd_9fs	= 250,
V6nd_9auth	= 251,
SRC_UNSPEC	= 0,
SRC_UNI		= 1,
TARG_UNI	= 2,
TARG_MULTI	= 3,
Tunitent	= 1,
Tuniproxy	= 2,
Tunirany	= 3,
MAX_MULTICAST_SOLICIT	= 3,
RETRANS_TIMER		= 1000,
};
typedef struct Ip6hdr	Ip6hdr;
typedef struct Opthdr	Opthdr;
typedef struct Routinghdr Routinghdr;
typedef struct Fraghdr6	Fraghdr6;
#define IPV6HDR \
uchar	vcf[4];		\
uchar	ploadlen[2];	 \
uchar	proto;		 \
uchar	ttl;		 \
uchar	src[IPaddrlen]; \
uchar	dst[IPaddrlen]
struct	Ip6hdr {
IPV6HDR;
uchar	payload[];
};
struct	Opthdr {
uchar	nexthdr;
uchar	len;
};
struct	Routinghdr {
uchar	nexthdr;
uchar	len;
uchar	rtetype;
uchar	segrem;
};
struct	Fraghdr6 {
uchar	nexthdr;
uchar	res;
uchar	offsetRM[2];
uchar	id[4];
};
extern uchar v6allnodesN[IPaddrlen];
extern uchar v6allnodesL[IPaddrlen];
extern uchar v6allroutersN[IPaddrlen];
extern uchar v6allroutersL[IPaddrlen];
extern uchar v6allnodesNmask[IPaddrlen];
extern uchar v6allnodesLmask[IPaddrlen];
extern uchar v6solicitednode[IPaddrlen];
extern uchar v6solicitednodemask[IPaddrlen];
extern uchar v6Unspecified[IPaddrlen];
extern uchar v6loopback[IPaddrlen];
extern uchar v6loopbackmask[IPaddrlen];
extern uchar v6linklocal[IPaddrlen];
extern uchar v6linklocalmask[IPaddrlen];
extern uchar v6multicast[IPaddrlen];
extern uchar v6multicastmask[IPaddrlen];
extern int v6llpreflen;
extern int v6mcpreflen;
extern int v6snpreflen;
extern int v6aNpreflen;
extern int v6aLpreflen;
extern int ReTransTimer;
void ipv62smcast(uchar *, uchar *);
void icmpns(Fs *f, uchar* src, int suni, uchar* targ, int tuni, uchar* mac);
void icmpna(Fs *f, uchar* src, uchar* dst, uchar* targ, uchar* mac, uchar flags);
void icmpttlexceeded6(Fs *f, Ipifc *ifc, Block *bp);
void icmppkttoobig6(Fs *f, Ipifc *ifc, Block *bp);
void icmphostunr(Fs *f, Ipifc *ifc, Block *bp, int code, int free);