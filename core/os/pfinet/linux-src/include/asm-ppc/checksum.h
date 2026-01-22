#ifndef _PPC_CHECKSUM_H
#define _PPC_CHECKSUM_H
extern unsigned int csum_partial(const unsigned char * buff, int len,
unsigned int sum);
extern unsigned int csum_partial_copy_generic(const char *src, char *dst,
int len, unsigned int sum,
int *src_err, int *dst_err);
#define csum_partial_copy_from_user(src, dst, len, sum, errp)	\
csum_partial_copy_generic((src), (dst), (len), (sum), (errp), 0)
#define csum_partial_copy_nocheck(src, dst, len, sum)	\
csum_partial_copy_generic((src), (dst), (len), (sum), 0, 0)
#define csum_partial_copy(src, dst, len, sum)	\
csum_partial_copy_generic((src), (dst), (len), (sum), 0, 0)
#define csum_partial_copy_fromuser(src, dst, len, sum)	\
csum_partial_copy_generic((src), (dst), (len), (sum), 0, 0)
static inline unsigned int csum_fold(unsigned int sum)
{
unsigned int tmp;
__asm__("rlwinm %0,%1,16,0,31" : "=r" (tmp) : "r" (sum));
sum = ~(sum + tmp) >> 16;
return sum;
}
static inline unsigned short ip_compute_csum(unsigned char * buff, int len)
{
return csum_fold(csum_partial(buff, len, 0));
}
static inline unsigned long csum_tcpudp_nofold(unsigned long saddr,
unsigned long daddr,
unsigned short len,
unsigned short proto,
unsigned int sum)
{
__asm__("
addc %0,%0,%1
adde %0,%0,%2
adde %0,%0,%3
addze %0,%0
"
: "=r" (sum)
: "r" (daddr), "r"(saddr), "r"((proto<<16)+len), "0"(sum));
return sum;
}
extern unsigned short ip_fast_csum(unsigned char * iph, unsigned int ihl);
extern unsigned short csum_tcpudp_magic(unsigned long saddr,
unsigned long daddr,
unsigned short len,
unsigned short proto,
unsigned int sum);
#endif