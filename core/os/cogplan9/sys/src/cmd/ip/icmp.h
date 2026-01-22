enum
{
EchoReply = 0,
Unreachable = 3,
SrcQuench = 4,
Redirect = 5,
EchoRequest = 8,
TimeExceed = 11,
InParmProblem = 12,
Timestamp = 13,
TimestampReply = 14,
InfoRequest = 15,
InfoReply = 16,
AddrMaskRequest = 17,
AddrMaskReply = 18,
Traceroute = 30,
IPv6WhereAreYou = 33,
IPv6IAmHere = 34,
UnreachableV6 = 1,
PacketTooBigV6 = 2,
TimeExceedV6 = 3,
ParamProblemV6 = 4,
EchoRequestV6 = 128,
EchoReplyV6 = 129,
RouterSolicit = 133,
RouterAdvert = 134,
NbrSolicit = 135,
NbrAdvert = 136,
RedirectV6 = 137,
Maxtype6 = 137,
ICMP_HDRSIZE = 8,
};
typedef struct Ip4hdr Ip4hdr;
struct Ip4hdr
{
uchar vihl;
uchar tos;
uchar length[2];
uchar id[2];
uchar frag[2];
uchar ttl;
uchar proto;
uchar ipcksum[2];
uchar src[4];
uchar dst[4];
uchar data[];
};
typedef struct Icmphdr Icmphdr;
struct Icmphdr {
uchar type;
uchar code;
uchar cksum[2];
uchar icmpid[2];
uchar seq[2];
uchar data[];
};