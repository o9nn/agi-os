#ifndef _M68K_CHECKSUM_H
#define _M68K_CHECKSUM_H
unsigned int csum_partial(const unsigned char * buff, int len, unsigned int sum);
unsigned int csum_partial_copy(const char *src, char *dst, int len, int sum);
extern unsigned int csum_partial_copy_from_user(const char *src, char *dst,
int len, int sum, int *csum_err);
#define csum_partial_copy_nocheck(src, dst, len, sum)	\
csum_partial_copy((src), (dst), (len), (sum))
static inline unsigned short
ip_fast_csum(unsigned char *iph, unsigned int ihl)
{
unsigned int sum = 0;
__asm__ ("subqw #1,%2\n"
"1:\t"
"movel %1@+,%/d0\n\t"
"addxl %/d0,%0\n\t"
"dbra  %2,1b\n\t"
"movel %0,%/d0\n\t"
"swap  %/d0\n\t"
"addxw %/d0,%0\n\t"
"clrw  %/d0\n\t"
"addxw %/d0,%0\n\t"
: "=d" (sum), "=a" (iph), "=d" (ihl)
: "0" (sum), "1" (iph), "2" (ihl)
: "d0");
return ~sum;
}
static inline unsigned int csum_fold(unsigned int sum)
{
unsigned int tmp = sum;
__asm__("swap %1\n\t"
"addw %1, %0\n\t"
"clrw %1\n\t"
"addxw %1, %0"
: "=&d" (sum), "=&d" (tmp)
: "0" (sum), "1" (sum));
return ~sum;
}
static inline unsigned int
csum_tcpudp_nofold(unsigned long saddr, unsigned long daddr, unsigned short len,
unsigned short proto, unsigned int sum)
{
__asm__ ("addl  %1,%0\n\t"
"addxl %4,%0\n\t"
"addxl %5,%0\n\t"
"clrl %1\n\t"
"addxl %1,%0"
: "=&d" (sum), "=&d" (saddr)
: "0" (daddr), "1" (saddr), "d" (len + proto),
"d"(sum));
return sum;
}
static inline unsigned short int
csum_tcpudp_magic(unsigned long saddr, unsigned long daddr, unsigned short len,
unsigned short proto, unsigned int sum)
{
return csum_fold(csum_tcpudp_nofold(saddr,daddr,len,proto,sum));
}
static inline unsigned short
ip_compute_csum(unsigned char * buff, int len)
{
return csum_fold (csum_partial(buff, len, 0));
}
#define _HAVE_ARCH_IPV6_CSUM
static __inline__ unsigned short int
csum_ipv6_magic(struct in6_addr *saddr, struct in6_addr *daddr,
__u16 len, unsigned short proto, unsigned int sum)
{
register unsigned long tmp;
__asm__("addl %2@,%0\n\t"
"movel %2@(4),%1\n\t"
"addxl %1,%0\n\t"
"movel %2@(8),%1\n\t"
"addxl %1,%0\n\t"
"movel %2@(12),%1\n\t"
"addxl %1,%0\n\t"
"movel %3@,%1\n\t"
"addxl %1,%0\n\t"
"movel %3@(4),%1\n\t"
"addxl %1,%0\n\t"
"movel %3@(8),%1\n\t"
"addxl %1,%0\n\t"
"movel %3@(12),%1\n\t"
"addxl %1,%0\n\t"
"addxl %4,%0\n\t"
"clrl %1\n\t"
"addxl %1,%0"
: "=&d" (sum), "=&d" (tmp)
: "a" (saddr), "a" (daddr), "d" ((__u32) len + proto),
"0" (sum));
return csum_fold(sum);
}
#endif