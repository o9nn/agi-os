#ifndef _S390_CHECKSUM_H
#define _S390_CHECKSUM_H
#include <asm/uaccess.h>
unsigned int
csum_partial(const unsigned char * buff, int len, unsigned int sum);
extern inline unsigned int
csum_partial_inline(const unsigned char * buff, int len, unsigned int sum)
{
__asm__ __volatile__ (
"    lr   2,%1\n"
"    lr   3,%2\n"
"0:  cksm %0,2\n"
"    jo   0b\n"
: "+&d" (sum)
: "d" (buff), "d" (len)
: "cc", "2", "3" );
return sum;
}
extern inline unsigned int
csum_partial_copy(const char *src, char *dst, int len,unsigned int sum)
{
memcpy(dst,src,len);
return csum_partial_inline(dst, len, sum);
}
extern inline unsigned int
csum_partial_copy_from_user(const char *src, char *dst,
int len, unsigned int sum, int *errp)
{
if (copy_from_user(dst, src, len)) {
*errp = -EFAULT;
memset(dst, 0, len);
return sum;
}
return csum_partial(dst, len, sum);
}
extern inline unsigned int
csum_partial_copy_nocheck (const char *src, char *dst, int len, unsigned int sum)
{
memcpy(dst,src,len);
return csum_partial_inline(dst, len, sum);
}
#if 1
unsigned short csum_fold(unsigned int sum);
#else
extern inline unsigned short
csum_fold(unsigned int sum)
{
__asm__ __volatile__ (
"    sr   3,3\n"
"    lr   2,%0\n"
"    srdl 2,16\n"
"    alr  2,3\n"
"    alr  %0,2\n"
"    srl  %0,16\n"
: "+&d" (sum) : : "cc", "2", "3");
return ((unsigned short) ~sum);
}
#endif
extern inline unsigned short
ip_fast_csum(unsigned char *iph, unsigned int ihl)
{
unsigned long sum;
__asm__ __volatile__ (
"    sr   %0,%0\n"
"    lr   2,%1\n"
"    lr   3,%2\n"
"0:  cksm %0,2\n"
"    jo   0b\n"
: "=&d" (sum)
: "d" (iph), "d" (ihl*4)
: "cc", "2", "3" );
return csum_fold(sum);
}
extern inline unsigned int
csum_tcpudp_nofold(unsigned long saddr, unsigned long daddr,
unsigned short len, unsigned short proto,
unsigned int sum)
{
__asm__ __volatile__ (
"    sll   %3,16\n"
"    or    %3,%4\n"
"    alr   %1,%2\n"
"    brc   12,0f\n"
"    ahi   %1,1\n"
"0:  alr   %1,%3\n"
"    brc   12,1f\n"
"    ahi   %1,1\n"
"1:  alr   %0,%1\n"
"    brc   12,2f\n"
"    ahi   %0,1\n"
"2:"
: "+&d" (sum)
: "d" (saddr), "d" (daddr), "d" (proto), "d" (len)
: "cc" );
return sum;
}
extern inline unsigned short int
csum_tcpudp_magic(unsigned long saddr, unsigned long daddr,
unsigned short len, unsigned short proto,
unsigned int sum)
{
return csum_fold(csum_tcpudp_nofold(saddr,daddr,len,proto,sum));
}
extern inline unsigned short
ip_compute_csum(unsigned char * buff, int len)
{
return csum_fold(csum_partial(buff, len, 0));
}
#endif