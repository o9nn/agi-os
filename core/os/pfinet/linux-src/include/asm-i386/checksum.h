#ifndef _I386_CHECKSUM_H
#define _I386_CHECKSUM_H
asmlinkage unsigned int csum_partial(const unsigned char * buff, int len, unsigned int sum);
asmlinkage unsigned int csum_partial_copy_generic( const char *src, char *dst, int len, int sum,
int *src_err_ptr, int *dst_err_ptr);
static __inline__
unsigned int csum_partial_copy_nocheck ( const char *src, char *dst,
int len, int sum)
{
return csum_partial_copy_generic ( src, dst, len, sum, NULL, NULL);
}
static __inline__
unsigned int csum_partial_copy_from_user ( const char *src, char *dst,
int len, int sum, int *err_ptr)
{
return csum_partial_copy_generic ( src, dst, len, sum, err_ptr, NULL);
}
#if 0
static __inline__
unsigned int csum_partial_copy_to_user ( const char *src, char *dst,
int len, int sum, int *err_ptr)
{
return csum_partial_copy_generic ( src, dst, len, sum, NULL, err_ptr);
}
#endif
#define csum_partial_copy_fromuser csum_partial_copy
unsigned int csum_partial_copy( const char *src, char *dst, int len, int sum);
static inline unsigned short ip_fast_csum(unsigned char * iph,
unsigned int ihl) {
unsigned int sum;
__asm__ __volatile__(
"	    movl (%1), %0\n"
"	    subl $4, %2\n"
"	    jbe 2f\n"
"	    addl 4(%1), %0\n"
"	    adcl 8(%1), %0\n"
"	    adcl 12(%1), %0\n"
"1:	    adcl 16(%1), %0\n"
"	    lea 4(%1), %1\n"
"	    decl %2\n"
"	    jne	1b\n"
"	    adcl $0, %0\n"
"	    movl %0, %2\n"
"	    shrl $16, %0\n"
"	    addw %w2, %w0\n"
"	    adcl $0, %0\n"
"	    notl %0\n"
"2:"
: "=r" (sum), "=r" (iph), "=r" (ihl)
: "1" (iph), "2" (ihl)
: "memory");
return(sum);
}
static inline unsigned int csum_fold(unsigned int sum)
{
__asm__("addl %1, %0\n"
"adcl $0xffff, %0\n"
: "=r" (sum)
: "r" (sum << 16), "0" (sum & 0xffff0000)
);
return (~sum) >> 16;
}
static inline unsigned long csum_tcpudp_nofold(unsigned long saddr,
unsigned long daddr,
unsigned short len,
unsigned short proto,
unsigned int sum)
{
__asm__(
"addl %1, %0\n"
"adcl %2, %0\n"
"adcl %3, %0\n"
"adcl $0, %0\n"
: "=r" (sum)
: "g" (daddr), "g"(saddr), "g"((ntohs(len)<<16)+proto*256), "0"(sum));
return sum;
}
static inline unsigned short int csum_tcpudp_magic(unsigned long saddr,
unsigned long daddr,
unsigned short len,
unsigned short proto,
unsigned int sum)
{
return csum_fold(csum_tcpudp_nofold(saddr,daddr,len,proto,sum));
}
static inline unsigned short ip_compute_csum(unsigned char * buff, int len) {
return csum_fold (csum_partial(buff, len, 0));
}
#define _HAVE_ARCH_IPV6_CSUM
static __inline__ unsigned short int csum_ipv6_magic(struct in6_addr *saddr,
struct in6_addr *daddr,
__u16 len,
unsigned short proto,
unsigned int sum)
{
__asm__("addl 0(%1), %0\n"
"adcl 4(%1), %0\n"
"adcl 8(%1), %0\n"
"adcl 12(%1), %0\n"
"adcl 0(%2), %0\n"
"adcl 4(%2), %0\n"
"adcl 8(%2), %0\n"
"adcl 12(%2), %0\n"
"adcl %3, %0\n"
"adcl %4, %0\n"
"adcl $0, %0\n"
: "=&r" (sum)
: "r" (saddr), "r" (daddr),
"r"(htonl((__u32) (len))), "r"(htonl(proto)), "0"(sum)
: "memory");
return csum_fold(sum);
}
#define HAVE_CSUM_COPY_USER
static __inline__ unsigned int csum_and_copy_to_user (const char *src, char *dst,
int len, int sum, int *err_ptr)
{
if (access_ok(VERIFY_WRITE, dst, len))
return csum_partial_copy_generic(src, dst, len, sum, NULL, err_ptr);
if (len)
*err_ptr = -EFAULT;
return -1;
}
#endif