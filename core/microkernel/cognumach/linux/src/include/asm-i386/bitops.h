#ifndef _I386_BITOPS_H
#define _I386_BITOPS_H
#ifdef __SMP__
#define LOCK_PREFIX "lock ; "
#define SMPVOL volatile
#else
#define LOCK_PREFIX ""
#define SMPVOL
#endif
struct __dummy { unsigned long a[100]; };
#define ADDR (*(struct __dummy *) addr)
#define CONST_ADDR (*(const struct __dummy *) addr)
static __inline__ int set_bit(int nr, SMPVOL void * addr)
{
int oldbit;
__asm__ __volatile__(LOCK_PREFIX
"btsl %2,%1\n\tsbbl %0,%0"
:"=r" (oldbit),"=m" (ADDR)
:"ir" (nr));
return oldbit;
}
static __inline__ int clear_bit(int nr, SMPVOL void * addr)
{
int oldbit;
__asm__ __volatile__(LOCK_PREFIX
"btrl %2,%1\n\tsbbl %0,%0"
:"=r" (oldbit),"=m" (ADDR)
:"ir" (nr));
return oldbit;
}
static __inline__ int change_bit(int nr, SMPVOL void * addr)
{
int oldbit;
__asm__ __volatile__(LOCK_PREFIX
"btcl %2,%1\n\tsbbl %0,%0"
:"=r" (oldbit),"=m" (ADDR)
:"ir" (nr));
return oldbit;
}
static __inline__ int test_and_set_bit(int nr, volatile void * addr)
{
int oldbit;
__asm__ __volatile__( LOCK_PREFIX
"btsl %2,%1\n\tsbbl %0,%0"
:"=r" (oldbit),"=m" (ADDR)
:"Ir" (nr));
return oldbit;
}
static __inline__ int test_and_clear_bit(int nr, volatile void * addr)
{
int oldbit;
__asm__ __volatile__( LOCK_PREFIX
"btrl %2,%1\n\tsbbl %0,%0"
:"=r" (oldbit),"=m" (ADDR)
:"Ir" (nr));
return oldbit;
}
static __inline__ int test_and_change_bit(int nr, volatile void * addr)
{
int oldbit;
__asm__ __volatile__( LOCK_PREFIX
"btcl %2,%1\n\tsbbl %0,%0"
:"=r" (oldbit),"=m" (ADDR)
:"Ir" (nr));
return oldbit;
}
static __inline__ int test_bit(int nr, const SMPVOL void * addr)
{
return ((1UL << (nr & 31)) & (((const unsigned int *) addr)[nr >> 5])) != 0;
}
static __inline__ int find_first_zero_bit(void * addr, unsigned size)
{
int d0, d1, d2;
int res;
if (!size)
return 0;
__asm__("cld\n\t"
"movl $-1,%%eax\n\t"
"xorl %%edx,%%edx\n\t"
"repe; scasl\n\t"
"je 1f\n\t"
"xorl -4(%%edi),%%eax\n\t"
"subl $4,%%edi\n\t"
"bsfl %%eax,%%edx\n"
"1:\tsubl %%ebx,%%edi\n\t"
"shll $3,%%edi\n\t"
"addl %%edi,%%edx"
:"=d" (res), "=&c" (d0), "=&D" (d1), "=&a" (d2)
:"1" ((size + 31) >> 5), "2" (addr), "b" (addr));
return res;
}
static __inline__ int find_next_zero_bit (void * addr, int size, int offset)
{
unsigned long * p = ((unsigned long *) addr) + (offset >> 5);
int set = 0, bit = offset & 31, res;
if (bit) {
__asm__("bsfl %1,%0\n\t"
"jne 1f\n\t"
"movl $32, %0\n"
"1:"
: "=r" (set)
: "r" (~(*p >> bit)));
if (set < (32 - bit))
return set + offset;
set = 32 - bit;
p++;
}
res = find_first_zero_bit (p, size - 32 * (p - (unsigned long *) addr));
return (offset + set + res);
}
static __inline__ unsigned long ffz(unsigned long word)
{
__asm__("bsfl %1,%0"
:"=r" (word)
:"r" (~word));
return word;
}
#ifdef __KERNEL__
static __inline__ int ffs(int x)
{
int r;
__asm__("bsfl %1,%0\n\t"
"jnz 1f\n\t"
"movl $-1,%0\n"
"1:" : "=r" (r) : "g" (x));
return r+1;
}
#define hweight32(x) generic_hweight32(x)
#define hweight16(x) generic_hweight16(x)
#define hweight8(x) generic_hweight8(x)
#endif
#endif