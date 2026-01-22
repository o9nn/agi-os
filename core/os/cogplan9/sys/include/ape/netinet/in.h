#ifndef __netinet_in__
#define __netinet_in__
#ifdef __cplusplus
extern "C" {
#endif
#define IPPROTO_IP 0
#define IPPROTO_ICMP 1
#define IPPROTO_GGP 3
#define IPPROTO_TCP 6
#define IPPROTO_EGP 8
#define IPPROTO_PUP 12
#define IPPROTO_UDP 17
#define IPPROTO_IDP 22
#define IPPROTO_TP 29
#define IPPROTO_EON 80
#define IPPROTO_RAW 255
#define IPPROTO_MAX 256
#define IPPORT_RESERVED 1024
#define IPPORT_USERRESERVED 5000
struct in_addr {
unsigned long s_addr;
};
#define IN_CLASSA(i) (((long)(i) & 0x80000000) == 0)
#define IN_CLASSA_NET 0xff000000
#define IN_CLASSA_NSHIFT 24
#define IN_CLASSA_HOST 0x00ffffff
#define IN_CLASSA_MAX 128
#define IN_CLASSB(i) (((long)(i) & 0xc0000000) == 0x80000000)
#define IN_CLASSB_NET 0xffff0000
#define IN_CLASSB_NSHIFT 16
#define IN_CLASSB_HOST 0x0000ffff
#define IN_CLASSB_MAX 65536
#define IN_CLASSC(i) (((long)(i) & 0xe0000000) == 0xc0000000)
#define IN_CLASSC_NET 0xffffff00
#define IN_CLASSC_NSHIFT 8
#define IN_CLASSC_HOST 0x000000ff
#define IN_CLASSD(i) (((long)(i) & 0xf0000000) == 0xe0000000)
#define IN_MULTICAST(i) IN_CLASSD(i)
#define IN_EXPERIMENTAL(i) (((long)(i) & 0xe0000000) == 0xe0000000)
#define IN_BADCLASS(i) (((long)(i) & 0xf0000000) == 0xf0000000)
#define INADDR_ANY (unsigned long)0x00000000
#define INADDR_BROADCAST (unsigned long)0xffffffff
#define INADDR_NONE (unsigned long)0xffffffff
#define IN_LOOPBACKNET 127
struct sockaddr_in {
short sin_family;
unsigned short sin_port;
struct in_addr sin_addr;
char sin_zero[8];
};
struct ip_opts {
struct in_addr ip_dst;
char ip_opts[40];
};
#define IP_OPTIONS 1
#define IP_HDRINCL 7
#define IP_TOS 8
#define IP_TTL 9
extern unsigned long ntohl(unsigned long x);
extern unsigned short ntohs(unsigned short x);
extern unsigned long htonl(unsigned long x);
extern unsigned short htons(unsigned short x);
extern unsigned long inet_addr(char*);
extern char* inet_ntoa(struct in_addr);
extern unsigned long nptohl(void*);
#ifdef __cplusplus
}
#endif
#endif