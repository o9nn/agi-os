#ifndef _I386_CHECKSUM_H
#define _I386_CHECKSUM_H
unsigned int csum_partial(const unsigned char * buff, int len, unsigned int sum);
unsigned int csum_partial_copy( const char *src, char *dst, int len, int sum);
unsigned int csum_partial_copy_fromuser(const char *src, char *dst, int len, int sum);
static inline unsigned short ip_fast_csum(unsigned char * iph,
unsigned int ihl) {
unsigned int sum;
__asm__ __volatile__("
movl (%1), %0
subl $4, %2
jbe 2f
addl 4(%1), %0
adcl 8(%1), %0
adcl 12(%1), %0
1:	    adcl 16(%1), %0
lea 4(%1), %1
decl %2
jne	1b
adcl $0, %0
movl %0, %2
shrl $16, %0
addw %w2, %w0
adcl $0, %0
notl %0
2:
"
: "=r" (sum), "=r" (iph), "=r" (ihl)
: "1" (iph), "2" (ihl));
return(sum);
}
static inline unsigned int csum_fold(unsigned int sum)
{
__asm__("
addl %1, %0
adcl $0xffff, %0
"
: "=r" (sum)
: "r" (sum << 16), "0" (sum & 0xffff0000)
);
return (~sum) >> 16;
}
static inline unsigned short int csum_tcpudp_magic(unsigned long saddr,
unsigned long daddr,
unsigned short len,
unsigned short proto,
unsigned int sum) {
__asm__("
addl %1, %0
adcl %2, %0
adcl %3, %0
adcl $0, %0
"
: "=r" (sum)
: "g" (daddr), "g"(saddr), "g"((ntohs(len)<<16)+proto*256), "0"(sum));
return csum_fold(sum);
}
static inline unsigned short ip_compute_csum(unsigned char * buff, int len) {
return csum_fold (csum_partial(buff, len, 0));
}
#endif