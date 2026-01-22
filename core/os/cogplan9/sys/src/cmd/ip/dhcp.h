enum
{
OfferTimeout= 60,
MaxLease= 60*60,
MinLease= 15*60,
StaticLease= 30*60,
IPUDPHDRSIZE= 28,
MINSUPPORTED= 576,
Maxhwlen= 16,
Maxfilelen= 128,
Maxoptlen= 312-4,
Bootrequest= 1,
Bootreply= 2,
Fbroadcast= 1<<15,
Discover= 1,
Offer= 2,
Request= 3,
Decline= 4,
Ack= 5,
Nak= 6,
Release= 7,
Inform= 8,
OBend= 255,
OBpad= 0,
OBmask= 1,
OBtimeoff= 2,
OBrouter= 3,
OBtimeserver= 4,
OBnameserver= 5,
OBdnserver= 6,
OBlogserver= 7,
OBcookieserver= 8,
OBlprserver= 9,
OBimpressserver= 10,
OBrlserver= 11,
OBhostname= 12,
OBbflen= 13,
OBdumpfile= 14,
OBdomainname= 15,
OBswapserver= 16,
OBrootpath= 17,
OBextpath= 18,
OBipforward= 19,
OBnonlocal= 20,
OBpolicyfilter= 21,
OBmaxdatagram= 22,
OBttl= 23,
OBpathtimeout= 24,
OBpathplateau= 25,
OBmtu= 26,
OBsubnetslocal= 27,
OBbaddr= 28,
OBdiscovermask= 29,
OBsupplymask= 30,
OBdiscoverrouter= 31,
OBrsserver= 32,
OBstaticroutes= 33,
OBtrailerencap= 34,
OBarptimeout= 35,
OBetherencap= 36,
OBtcpttl= 37,
OBtcpka= 38,
OBtcpkag= 39,
OBnisdomain= 40,
OBniserver= 41,
OBntpserver= 42,
OBvendorinfo= 43,
OBnetbiosns= 44,
OBnetbiosdds= 45,
OBnetbiostype= 46,
OBnetbiosscope= 47,
OBxfontserver= 48,
OBxdispmanager= 49,
OBnisplusdomain= 64,
OBnisplusserver= 65,
OBhomeagent= 68,
OBsmtpserver= 69,
OBpop3server= 70,
OBnntpserver= 71,
OBwwwserver= 72,
OBfingerserver= 73,
OBircserver= 74,
OBstserver= 75,
OBstdaserver= 76,
ODipaddr= 50,
ODlease= 51,
ODoverload= 52,
ODtype= 53,
ODserverid= 54,
ODparams= 55,
ODmessage= 56,
ODmaxmsg= 57,
ODrenewaltime= 58,
ODrebindingtime= 59,
ODvendorclass= 60,
ODclientid= 61,
ODtftpserver= 66,
ODbootfile= 67,
ODpxearch= 93,
ODpxeni= 94,
ODpxeguid= 97,
OP9fsv4= 128,
OP9authv4= 129,
OP9fs= 130,
OP9auth= 131,
OP9ipaddr= 132,
OP9ipmask= 133,
OP9ipgw= 134,
};
#define Lforever ~0UL
enum {
Sinit,
Sselecting,
Srequesting,
Sbound,
Srenewing,
Srebinding,
};
typedef struct Bootp Bootp;
struct Bootp
{
uchar udphdr[Udphdrsize];
uchar op;
uchar htype;
uchar hlen;
uchar hops;
uchar xid[4];
uchar secs[2];
uchar flags[2];
uchar ciaddr[IPv4addrlen];
uchar yiaddr[IPv4addrlen];
uchar siaddr[IPv4addrlen];
uchar giaddr[IPv4addrlen];
uchar chaddr[Maxhwlen];
char sname[64];
char file[Maxfilelen];
uchar optmagic[4];
uchar optdata[Maxoptlen];
};