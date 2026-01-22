#ifndef HEADER_DES_LOCL_H
# define HEADER_DES_LOCL_H
# include <openssl/e_os2.h>
# if defined(OPENSSL_SYS_WIN32)
# ifndef OPENSSL_SYS_MSDOS
# define OPENSSL_SYS_MSDOS
# endif
# endif
# include <stdio.h>
# include <stdlib.h>
# ifndef OPENSSL_SYS_MSDOS
# if !defined(OPENSSL_SYS_VMS) || defined(__DECC)
# ifdef OPENSSL_UNISTD
# include OPENSSL_UNISTD
# else
# include <unistd.h>
# endif
# include <math.h>
# endif
# endif
# include <openssl/des.h>
# ifdef OPENSSL_SYS_MSDOS
# include <stdlib.h>
# include <errno.h>
# include <time.h>
# include <io.h>
# endif
# if defined(__STDC__) || defined(OPENSSL_SYS_VMS) || defined(M_XENIX) || defined(OPENSSL_SYS_MSDOS)
# include <string.h>
# endif
# ifdef OPENSSL_BUILD_SHLIBCRYPTO
# undef OPENSSL_EXTERN
# define OPENSSL_EXTERN OPENSSL_EXPORT
# endif
# define ITERATIONS 16
# define HALF_ITERATIONS 8
# define MAXWRITE (1024*16)
# define BSIZE (MAXWRITE+4)
# define c2l(c,l) (l =((DES_LONG)(*((c)++))) , \
l|=((DES_LONG)(*((c)++)))<< 8L, \
l|=((DES_LONG)(*((c)++)))<<16L, \
l|=((DES_LONG)(*((c)++)))<<24L)
# define c2ln(c,l1,l2,n) { \
c+=n; \
l1=l2=0; \
switch (n) { \
case 8: l2 =((DES_LONG)(*(--(c))))<<24L; \
case 7: l2|=((DES_LONG)(*(--(c))))<<16L; \
case 6: l2|=((DES_LONG)(*(--(c))))<< 8L; \
case 5: l2|=((DES_LONG)(*(--(c)))); \
case 4: l1 =((DES_LONG)(*(--(c))))<<24L; \
case 3: l1|=((DES_LONG)(*(--(c))))<<16L; \
case 2: l1|=((DES_LONG)(*(--(c))))<< 8L; \
case 1: l1|=((DES_LONG)(*(--(c)))); \
} \
}
# define l2c(l,c) (*((c)++)=(unsigned char)(((l) )&0xff), \
*((c)++)=(unsigned char)(((l)>> 8L)&0xff), \
*((c)++)=(unsigned char)(((l)>>16L)&0xff), \
*((c)++)=(unsigned char)(((l)>>24L)&0xff))
# define HDRSIZE 4
# define n2l(c,l) (l =((DES_LONG)(*((c)++)))<<24L, \
l|=((DES_LONG)(*((c)++)))<<16L, \
l|=((DES_LONG)(*((c)++)))<< 8L, \
l|=((DES_LONG)(*((c)++))))
# define l2n(l,c) (*((c)++)=(unsigned char)(((l)>>24L)&0xff), \
*((c)++)=(unsigned char)(((l)>>16L)&0xff), \
*((c)++)=(unsigned char)(((l)>> 8L)&0xff), \
*((c)++)=(unsigned char)(((l) )&0xff))
# define l2cn(l1,l2,c,n) { \
c+=n; \
switch (n) { \
case 8: *(--(c))=(unsigned char)(((l2)>>24L)&0xff); \
case 7: *(--(c))=(unsigned char)(((l2)>>16L)&0xff); \
case 6: *(--(c))=(unsigned char)(((l2)>> 8L)&0xff); \
case 5: *(--(c))=(unsigned char)(((l2) )&0xff); \
case 4: *(--(c))=(unsigned char)(((l1)>>24L)&0xff); \
case 3: *(--(c))=(unsigned char)(((l1)>>16L)&0xff); \
case 2: *(--(c))=(unsigned char)(((l1)>> 8L)&0xff); \
case 1: *(--(c))=(unsigned char)(((l1) )&0xff); \
} \
}
# if (defined(OPENSSL_SYS_WIN32) && defined(_MSC_VER)) || defined(__ICC)
# define ROTATE(a,n) (_lrotr(a,n))
# elif defined(__GNUC__) && __GNUC__>=2 && !defined(__STRICT_ANSI__) && !defined(OPENSSL_NO_ASM) && !defined(OPENSSL_NO_INLINE_ASM) && !defined(PEDANTIC)
# if defined(__i386) || defined(__i386__) || defined(__x86_64) || defined(__x86_64__)
# define ROTATE(a,n) ({ register unsigned int ret; \
asm ("rorl %1,%0" \
: "=r"(ret) \
: "I"(n),"0"(a) \
: "cc"); \
ret; \
})
# endif
# endif
# ifndef ROTATE
# define ROTATE(a,n) (((a)>>(n))+((a)<<(32-(n))))
# endif
# ifdef DES_FCRYPT
# define LOAD_DATA_tmp(R,S,u,t,E0,E1) \
{ DES_LONG tmp; LOAD_DATA(R,S,u,t,E0,E1,tmp); }
# define LOAD_DATA(R,S,u,t,E0,E1,tmp) \
t=R^(R>>16L); \
u=t&E0; t&=E1; \
tmp=(u<<16); u^=R^s[S ]; u^=tmp; \
tmp=(t<<16); t^=R^s[S+1]; t^=tmp
# else
# define LOAD_DATA_tmp(a,b,c,d,e,f) LOAD_DATA(a,b,c,d,e,f,g)
# define LOAD_DATA(R,S,u,t,E0,E1,tmp) \
u=R^s[S ]; \
t=R^s[S+1]
# endif
# ifdef DES_PTR
# if defined(DES_RISC1) || defined(DES_RISC2)
# ifdef DES_RISC1
# define D_ENCRYPT(LL,R,S) { \
unsigned int u1,u2,u3; \
LOAD_DATA(R,S,u,t,E0,E1,u1); \
u2=(int)u>>8L; \
u1=(int)u&0xfc; \
u2&=0xfc; \
t=ROTATE(t,4); \
u>>=16L; \
LL^= *(const DES_LONG *)(des_SP +u1); \
LL^= *(const DES_LONG *)(des_SP+0x200+u2); \
u3=(int)(u>>8L); \
u1=(int)u&0xfc; \
u3&=0xfc; \
LL^= *(const DES_LONG *)(des_SP+0x400+u1); \
LL^= *(const DES_LONG *)(des_SP+0x600+u3); \
u2=(int)t>>8L; \
u1=(int)t&0xfc; \
u2&=0xfc; \
t>>=16L; \
LL^= *(const DES_LONG *)(des_SP+0x100+u1); \
LL^= *(const DES_LONG *)(des_SP+0x300+u2); \
u3=(int)t>>8L; \
u1=(int)t&0xfc; \
u3&=0xfc; \
LL^= *(const DES_LONG *)(des_SP+0x500+u1); \
LL^= *(const DES_LONG *)(des_SP+0x700+u3); }
# endif
# ifdef DES_RISC2
# define D_ENCRYPT(LL,R,S) { \
unsigned int u1,u2,s1,s2; \
LOAD_DATA(R,S,u,t,E0,E1,u1); \
u2=(int)u>>8L; \
u1=(int)u&0xfc; \
u2&=0xfc; \
t=ROTATE(t,4); \
LL^= *(const DES_LONG *)(des_SP +u1); \
LL^= *(const DES_LONG *)(des_SP+0x200+u2); \
s1=(int)(u>>16L); \
s2=(int)(u>>24L); \
s1&=0xfc; \
s2&=0xfc; \
LL^= *(const DES_LONG *)(des_SP+0x400+s1); \
LL^= *(const DES_LONG *)(des_SP+0x600+s2); \
u2=(int)t>>8L; \
u1=(int)t&0xfc; \
u2&=0xfc; \
LL^= *(const DES_LONG *)(des_SP+0x100+u1); \
LL^= *(const DES_LONG *)(des_SP+0x300+u2); \
s1=(int)(t>>16L); \
s2=(int)(t>>24L); \
s1&=0xfc; \
s2&=0xfc; \
LL^= *(const DES_LONG *)(des_SP+0x500+s1); \
LL^= *(const DES_LONG *)(des_SP+0x700+s2); }
# endif
# else
# define D_ENCRYPT(LL,R,S) { \
LOAD_DATA_tmp(R,S,u,t,E0,E1); \
t=ROTATE(t,4); \
LL^= \
*(const DES_LONG *)(des_SP +((u )&0xfc))^ \
*(const DES_LONG *)(des_SP+0x200+((u>> 8L)&0xfc))^ \
*(const DES_LONG *)(des_SP+0x400+((u>>16L)&0xfc))^ \
*(const DES_LONG *)(des_SP+0x600+((u>>24L)&0xfc))^ \
*(const DES_LONG *)(des_SP+0x100+((t )&0xfc))^ \
*(const DES_LONG *)(des_SP+0x300+((t>> 8L)&0xfc))^ \
*(const DES_LONG *)(des_SP+0x500+((t>>16L)&0xfc))^ \
*(const DES_LONG *)(des_SP+0x700+((t>>24L)&0xfc)); }
# endif
# else
# if defined(DES_RISC1) || defined(DES_RISC2)
# ifdef DES_RISC1
# define D_ENCRYPT(LL,R,S) {\
unsigned int u1,u2,u3; \
LOAD_DATA(R,S,u,t,E0,E1,u1); \
u>>=2L; \
t=ROTATE(t,6); \
u2=(int)u>>8L; \
u1=(int)u&0x3f; \
u2&=0x3f; \
u>>=16L; \
LL^=DES_SPtrans[0][u1]; \
LL^=DES_SPtrans[2][u2]; \
u3=(int)u>>8L; \
u1=(int)u&0x3f; \
u3&=0x3f; \
LL^=DES_SPtrans[4][u1]; \
LL^=DES_SPtrans[6][u3]; \
u2=(int)t>>8L; \
u1=(int)t&0x3f; \
u2&=0x3f; \
t>>=16L; \
LL^=DES_SPtrans[1][u1]; \
LL^=DES_SPtrans[3][u2]; \
u3=(int)t>>8L; \
u1=(int)t&0x3f; \
u3&=0x3f; \
LL^=DES_SPtrans[5][u1]; \
LL^=DES_SPtrans[7][u3]; }
# endif
# ifdef DES_RISC2
# define D_ENCRYPT(LL,R,S) {\
unsigned int u1,u2,s1,s2; \
LOAD_DATA(R,S,u,t,E0,E1,u1); \
u>>=2L; \
t=ROTATE(t,6); \
u2=(int)u>>8L; \
u1=(int)u&0x3f; \
u2&=0x3f; \
LL^=DES_SPtrans[0][u1]; \
LL^=DES_SPtrans[2][u2]; \
s1=(int)u>>16L; \
s2=(int)u>>24L; \
s1&=0x3f; \
s2&=0x3f; \
LL^=DES_SPtrans[4][s1]; \
LL^=DES_SPtrans[6][s2]; \
u2=(int)t>>8L; \
u1=(int)t&0x3f; \
u2&=0x3f; \
LL^=DES_SPtrans[1][u1]; \
LL^=DES_SPtrans[3][u2]; \
s1=(int)t>>16; \
s2=(int)t>>24L; \
s1&=0x3f; \
s2&=0x3f; \
LL^=DES_SPtrans[5][s1]; \
LL^=DES_SPtrans[7][s2]; }
# endif
# else
# define D_ENCRYPT(LL,R,S) {\
LOAD_DATA_tmp(R,S,u,t,E0,E1); \
t=ROTATE(t,4); \
LL^=\
DES_SPtrans[0][(u>> 2L)&0x3f]^ \
DES_SPtrans[2][(u>>10L)&0x3f]^ \
DES_SPtrans[4][(u>>18L)&0x3f]^ \
DES_SPtrans[6][(u>>26L)&0x3f]^ \
DES_SPtrans[1][(t>> 2L)&0x3f]^ \
DES_SPtrans[3][(t>>10L)&0x3f]^ \
DES_SPtrans[5][(t>>18L)&0x3f]^ \
DES_SPtrans[7][(t>>26L)&0x3f]; }
# endif
# endif
# define PERM_OP(a,b,t,n,m) ((t)=((((a)>>(n))^(b))&(m)),\
(b)^=(t),\
(a)^=((t)<<(n)))
# define IP(l,r) \
{ \
register DES_LONG tt; \
PERM_OP(r,l,tt, 4,0x0f0f0f0fL); \
PERM_OP(l,r,tt,16,0x0000ffffL); \
PERM_OP(r,l,tt, 2,0x33333333L); \
PERM_OP(l,r,tt, 8,0x00ff00ffL); \
PERM_OP(r,l,tt, 1,0x55555555L); \
}
# define FP(l,r) \
{ \
register DES_LONG tt; \
PERM_OP(l,r,tt, 1,0x55555555L); \
PERM_OP(r,l,tt, 8,0x00ff00ffL); \
PERM_OP(l,r,tt, 2,0x33333333L); \
PERM_OP(r,l,tt,16,0x0000ffffL); \
PERM_OP(l,r,tt, 4,0x0f0f0f0fL); \
}
extern const DES_LONG DES_SPtrans[8][64];
void fcrypt_body(DES_LONG *out, DES_key_schedule *ks,
DES_LONG Eswap0, DES_LONG Eswap1);
# ifdef OPENSSL_SMALL_FOOTPRINT
# undef DES_UNROLL
# endif
#endif