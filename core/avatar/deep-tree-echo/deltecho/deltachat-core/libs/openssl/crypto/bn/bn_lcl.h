#ifndef HEADER_BN_LCL_H
# define HEADER_BN_LCL_H
# include <openssl/bn.h>
#ifdef __cplusplus
extern "C" {
#endif
# if 1
# define BN_window_bits_for_exponent_size(b) \
((b) > 671 ? 6 : \
(b) > 239 ? 5 : \
(b) > 79 ? 4 : \
(b) > 23 ? 3 : 1)
# else
# define BN_window_bits_for_exponent_size(b) \
((b) > 255 ? 5 : \
(b) > 127 ? 4 : \
(b) > 17 ? 3 : 1)
# endif
# define MOD_EXP_CTIME_MIN_CACHE_LINE_WIDTH ( 64 )
# define MOD_EXP_CTIME_MIN_CACHE_LINE_MASK (MOD_EXP_CTIME_MIN_CACHE_LINE_WIDTH - 1)
# if MOD_EXP_CTIME_MIN_CACHE_LINE_WIDTH == 64
# define BN_window_bits_for_ctime_exponent_size(b) \
((b) > 937 ? 6 : \
(b) > 306 ? 5 : \
(b) > 89 ? 4 : \
(b) > 22 ? 3 : 1)
# define BN_MAX_WINDOW_BITS_FOR_CTIME_EXPONENT_SIZE (6)
# elif MOD_EXP_CTIME_MIN_CACHE_LINE_WIDTH == 32
# define BN_window_bits_for_ctime_exponent_size(b) \
((b) > 306 ? 5 : \
(b) > 89 ? 4 : \
(b) > 22 ? 3 : 1)
# define BN_MAX_WINDOW_BITS_FOR_CTIME_EXPONENT_SIZE (5)
# endif
# define BN_MULL_SIZE_NORMAL (16)
# define BN_MUL_RECURSIVE_SIZE_NORMAL (16)
# define BN_SQR_RECURSIVE_SIZE_NORMAL (16)
# define BN_MUL_LOW_RECURSIVE_SIZE_NORMAL (32)
# define BN_MONT_CTX_SET_SIZE_WORD (64)
# if !defined(OPENSSL_NO_ASM) && !defined(OPENSSL_NO_INLINE_ASM) && !defined(PEDANTIC)
# if defined(__alpha) && (defined(SIXTY_FOUR_BIT_LONG) || defined(SIXTY_FOUR_BIT))
# if defined(__DECC)
# include <c_asm.h>
# define BN_UMULT_HIGH(a,b) (BN_ULONG)asm("umulh %a0,%a1,%v0",(a),(b))
# elif defined(__GNUC__) && __GNUC__>=2
# define BN_UMULT_HIGH(a,b) ({ \
register BN_ULONG ret; \
asm ("umulh     %1,%2,%0" \
: "=r"(ret) \
: "r"(a), "r"(b)); \
ret; })
# endif
# elif defined(_ARCH_PPC) && defined(__64BIT__) && defined(SIXTY_FOUR_BIT_LONG)
# if defined(__GNUC__) && __GNUC__>=2
# define BN_UMULT_HIGH(a,b) ({ \
register BN_ULONG ret; \
asm ("mulhdu    %0,%1,%2" \
: "=r"(ret) \
: "r"(a), "r"(b)); \
ret; })
# endif
# elif (defined(__x86_64) || defined(__x86_64__)) && \
(defined(SIXTY_FOUR_BIT_LONG) || defined(SIXTY_FOUR_BIT))
# if defined(__GNUC__) && __GNUC__>=2
# define BN_UMULT_HIGH(a,b) ({ \
register BN_ULONG ret,discard; \
asm ("mulq      %3" \
: "=a"(discard),"=d"(ret) \
: "a"(a), "g"(b) \
: "cc"); \
ret; })
# define BN_UMULT_LOHI(low,high,a,b) \
asm ("mulq      %3" \
: "=a"(low),"=d"(high) \
: "a"(a),"g"(b) \
: "cc");
# endif
# elif (defined(_M_AMD64) || defined(_M_X64)) && defined(SIXTY_FOUR_BIT)
# if defined(_MSC_VER) && _MSC_VER>=1400
unsigned __int64 __umulh(unsigned __int64 a, unsigned __int64 b);
unsigned __int64 _umul128(unsigned __int64 a, unsigned __int64 b,
unsigned __int64 *h);
# pragma intrinsic(__umulh,_umul128)
# define BN_UMULT_HIGH(a,b) __umulh((a),(b))
# define BN_UMULT_LOHI(low,high,a,b) ((low)=_umul128((a),(b),&(high)))
# endif
# elif defined(__mips) && (defined(SIXTY_FOUR_BIT) || defined(SIXTY_FOUR_BIT_LONG))
# if defined(__GNUC__) && __GNUC__>=2
# if __GNUC__>4 || (__GNUC__>=4 && __GNUC_MINOR__>=4)
# define BN_UMULT_HIGH(a,b) (((__uint128_t)(a)*(b))>>64)
# define BN_UMULT_LOHI(low,high,a,b) ({ \
__uint128_t ret=(__uint128_t)(a)*(b); \
(high)=ret>>64; (low)=ret; })
# else
# define BN_UMULT_HIGH(a,b) ({ \
register BN_ULONG ret; \
asm ("dmultu    %1,%2" \
: "=h"(ret) \
: "r"(a), "r"(b) : "l"); \
ret; })
# define BN_UMULT_LOHI(low,high,a,b)\
asm ("dmultu    %2,%3" \
: "=l"(low),"=h"(high) \
: "r"(a), "r"(b));
# endif
# endif
# endif
# endif
# define Lw(t) (((BN_ULONG)(t))&BN_MASK2)
# define Hw(t) (((BN_ULONG)((t)>>BN_BITS2))&BN_MASK2)
# ifdef BN_DEBUG_RAND
# define bn_clear_top2max(a) \
{ \
int ind = (a)->dmax - (a)->top; \
BN_ULONG *ftl = &(a)->d[(a)->top-1]; \
for (; ind != 0; ind--) \
*(++ftl) = 0x0; \
}
# else
# define bn_clear_top2max(a)
# endif
# ifdef BN_LLONG
# define mul_add(r,a,w,c) { \
BN_ULLONG t; \
t=(BN_ULLONG)w * (a) + (r) + (c); \
(r)= Lw(t); \
(c)= Hw(t); \
}
# define mul(r,a,w,c) { \
BN_ULLONG t; \
t=(BN_ULLONG)w * (a) + (c); \
(r)= Lw(t); \
(c)= Hw(t); \
}
# define sqr(r0,r1,a) { \
BN_ULLONG t; \
t=(BN_ULLONG)(a)*(a); \
(r0)=Lw(t); \
(r1)=Hw(t); \
}
# elif defined(BN_UMULT_LOHI)
# define mul_add(r,a,w,c) { \
BN_ULONG high,low,ret,tmp=(a); \
ret = (r); \
BN_UMULT_LOHI(low,high,w,tmp); \
ret += (c); \
(c) = (ret<(c))?1:0; \
(c) += high; \
ret += low; \
(c) += (ret<low)?1:0; \
(r) = ret; \
}
# define mul(r,a,w,c) { \
BN_ULONG high,low,ret,ta=(a); \
BN_UMULT_LOHI(low,high,w,ta); \
ret = low + (c); \
(c) = high; \
(c) += (ret<low)?1:0; \
(r) = ret; \
}
# define sqr(r0,r1,a) { \
BN_ULONG tmp=(a); \
BN_UMULT_LOHI(r0,r1,tmp,tmp); \
}
# elif defined(BN_UMULT_HIGH)
# define mul_add(r,a,w,c) { \
BN_ULONG high,low,ret,tmp=(a); \
ret = (r); \
high= BN_UMULT_HIGH(w,tmp); \
ret += (c); \
low = (w) * tmp; \
(c) = (ret<(c))?1:0; \
(c) += high; \
ret += low; \
(c) += (ret<low)?1:0; \
(r) = ret; \
}
# define mul(r,a,w,c) { \
BN_ULONG high,low,ret,ta=(a); \
low = (w) * ta; \
high= BN_UMULT_HIGH(w,ta); \
ret = low + (c); \
(c) = high; \
(c) += (ret<low)?1:0; \
(r) = ret; \
}
# define sqr(r0,r1,a) { \
BN_ULONG tmp=(a); \
(r0) = tmp * tmp; \
(r1) = BN_UMULT_HIGH(tmp,tmp); \
}
# else
# define LBITS(a) ((a)&BN_MASK2l)
# define HBITS(a) (((a)>>BN_BITS4)&BN_MASK2l)
# define L2HBITS(a) (((a)<<BN_BITS4)&BN_MASK2)
# define LLBITS(a) ((a)&BN_MASKl)
# define LHBITS(a) (((a)>>BN_BITS2)&BN_MASKl)
# define LL2HBITS(a) ((BN_ULLONG)((a)&BN_MASKl)<<BN_BITS2)
# define mul64(l,h,bl,bh) \
{ \
BN_ULONG m,m1,lt,ht; \
\
lt=l; \
ht=h; \
m =(bh)*(lt); \
lt=(bl)*(lt); \
m1=(bl)*(ht); \
ht =(bh)*(ht); \
m=(m+m1)&BN_MASK2; if (m < m1) ht+=L2HBITS((BN_ULONG)1); \
ht+=HBITS(m); \
m1=L2HBITS(m); \
lt=(lt+m1)&BN_MASK2; if (lt < m1) ht++; \
(l)=lt; \
(h)=ht; \
}
# define sqr64(lo,ho,in) \
{ \
BN_ULONG l,h,m; \
\
h=(in); \
l=LBITS(h); \
h=HBITS(h); \
m =(l)*(h); \
l*=l; \
h*=h; \
h+=(m&BN_MASK2h1)>>(BN_BITS4-1); \
m =(m&BN_MASK2l)<<(BN_BITS4+1); \
l=(l+m)&BN_MASK2; if (l < m) h++; \
(lo)=l; \
(ho)=h; \
}
# define mul_add(r,a,bl,bh,c) { \
BN_ULONG l,h; \
\
h= (a); \
l=LBITS(h); \
h=HBITS(h); \
mul64(l,h,(bl),(bh)); \
\
\
l=(l+(c))&BN_MASK2; if (l < (c)) h++; \
(c)=(r); \
l=(l+(c))&BN_MASK2; if (l < (c)) h++; \
(c)=h&BN_MASK2; \
(r)=l; \
}
# define mul(r,a,bl,bh,c) { \
BN_ULONG l,h; \
\
h= (a); \
l=LBITS(h); \
h=HBITS(h); \
mul64(l,h,(bl),(bh)); \
\
\
l+=(c); if ((l&BN_MASK2) < (c)) h++; \
(c)=h&BN_MASK2; \
(r)=l&BN_MASK2; \
}
# endif
# if defined(OPENSSL_DOING_MAKEDEPEND) && defined(OPENSSL_FIPS)
# undef bn_div_words
# endif
void bn_mul_normal(BN_ULONG *r, BN_ULONG *a, int na, BN_ULONG *b, int nb);
void bn_mul_comba8(BN_ULONG *r, BN_ULONG *a, BN_ULONG *b);
void bn_mul_comba4(BN_ULONG *r, BN_ULONG *a, BN_ULONG *b);
void bn_sqr_normal(BN_ULONG *r, const BN_ULONG *a, int n, BN_ULONG *tmp);
void bn_sqr_comba8(BN_ULONG *r, const BN_ULONG *a);
void bn_sqr_comba4(BN_ULONG *r, const BN_ULONG *a);
int bn_cmp_words(const BN_ULONG *a, const BN_ULONG *b, int n);
int bn_cmp_part_words(const BN_ULONG *a, const BN_ULONG *b, int cl, int dl);
void bn_mul_recursive(BN_ULONG *r, BN_ULONG *a, BN_ULONG *b, int n2,
int dna, int dnb, BN_ULONG *t);
void bn_mul_part_recursive(BN_ULONG *r, BN_ULONG *a, BN_ULONG *b,
int n, int tna, int tnb, BN_ULONG *t);
void bn_sqr_recursive(BN_ULONG *r, const BN_ULONG *a, int n2, BN_ULONG *t);
void bn_mul_low_normal(BN_ULONG *r, BN_ULONG *a, BN_ULONG *b, int n);
void bn_mul_low_recursive(BN_ULONG *r, BN_ULONG *a, BN_ULONG *b, int n2,
BN_ULONG *t);
void bn_mul_high(BN_ULONG *r, BN_ULONG *a, BN_ULONG *b, BN_ULONG *l, int n2,
BN_ULONG *t);
BN_ULONG bn_add_part_words(BN_ULONG *r, const BN_ULONG *a, const BN_ULONG *b,
int cl, int dl);
BN_ULONG bn_sub_part_words(BN_ULONG *r, const BN_ULONG *a, const BN_ULONG *b,
int cl, int dl);
int bn_mul_mont(BN_ULONG *rp, const BN_ULONG *ap, const BN_ULONG *bp,
const BN_ULONG *np, const BN_ULONG *n0, int num);
#ifdef __cplusplus
}
#endif
#endif