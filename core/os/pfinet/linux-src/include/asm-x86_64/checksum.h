#ifndef _ASM_X86_CHECKSUM_64_H
#define _ASM_X86_CHECKSUM_64_H
#include <asm/uaccess.h>
#include <asm/byteorder.h>
static inline unsigned short csum_fold(unsigned int sum)
{
sum = (sum & 0xffff) + (sum >> 16);
sum = (sum & 0xffff) + (sum >> 16);
return ~sum;
}
extern unsigned short ip_fast_csum(unsigned char * iph, unsigned int ihl);
extern unsigned short int csum_tcpudp_magic(unsigned long saddr,
unsigned long daddr,
unsigned short len,
unsigned short proto,
unsigned int sum);
unsigned int csum_tcpudp_nofold(unsigned long saddr, unsigned long daddr,
unsigned short len, unsigned short proto,
unsigned int sum);
extern unsigned int csum_partial(const void *buff, int len, unsigned int sum);
#define  _HAVE_ARCH_COPY_AND_CSUM_FROM_USER 1
#define HAVE_CSUM_COPY_USER 1
static inline unsigned int
csum_partial_copy(const char *src, char *dst, int len,unsigned int sum)
{
memcpy(dst,src,len);
return csum_partial(dst, len, sum);
}
static inline unsigned int
csum_partial_copy_generic(const void *src, void *dst,
int len, unsigned int sum,
int *src_err_ptr, int *dst_err_ptr)
{
return csum_partial_copy(src, dst, len, sum);
}
static __inline__
unsigned int csum_partial_copy_to_user ( const void *src, void *dst,
int len, unsigned int sum, int *err_ptr)
{
return csum_partial_copy_generic ( src, dst, len, sum, NULL, err_ptr);
}
static __inline__
unsigned int csum_partial_copy_from_user ( const void *src, void *dst,
int len, unsigned int sum, int *err_ptr)
{
return csum_partial_copy_generic ( src, dst, len, sum, err_ptr, NULL);
}
extern unsigned int csum_partial_copy_nocheck(const void *src, void *dst,
int len, unsigned int sum);
#define csum_partial_copy_nocheck(src, dst, len, sum)	\
csum_partial_copy((src), (dst), (len), (sum))
#define csum_and_copy_to_user csum_partial_copy_to_user
#define csum_and_copy_from_user csum_partial_copy_from_user
extern unsigned short ip_compute_csum(const void *buff, int len);
#endif