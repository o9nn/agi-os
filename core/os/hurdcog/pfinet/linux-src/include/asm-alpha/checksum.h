#ifndef _ALPHA_CHECKSUM_H
#define _ALPHA_CHECKSUM_H
extern unsigned short ip_fast_csum(unsigned char * iph, unsigned int ihl);
extern unsigned short int csum_tcpudp_magic(unsigned long saddr,
unsigned long daddr,
unsigned short len,
unsigned short proto,
unsigned int sum);
unsigned int csum_tcpudp_nofold(unsigned long saddr, unsigned long daddr,
unsigned short len, unsigned short proto,
unsigned int sum);
extern unsigned int csum_partial(const unsigned char * buff, int len, unsigned int sum);
unsigned int csum_partial_copy(const char *src, char *dst, int len, unsigned int sum);
#define csum_partial_copy_fromuser csum_partial_copy
unsigned int csum_partial_copy_from_user(const char *src, char *dst, int len, unsigned int sum, int *errp);
unsigned int csum_partial_copy_nocheck(const char *src, char *dst, int len, unsigned int sum);
extern unsigned short ip_compute_csum(unsigned char * buff, int len);
static inline unsigned short csum_fold(unsigned int sum)
{
sum = (sum & 0xffff) + (sum >> 16);
sum = (sum & 0xffff) + (sum >> 16);
return ~sum;
}
#define _HAVE_ARCH_IPV6_CSUM
extern unsigned short int csum_ipv6_magic(struct in6_addr *saddr,
struct in6_addr *daddr,
__u16 len,
unsigned short proto,
unsigned int sum);
#endif