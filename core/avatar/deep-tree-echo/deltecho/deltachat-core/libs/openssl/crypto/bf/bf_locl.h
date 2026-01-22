#ifndef HEADER_BF_LOCL_H
# define HEADER_BF_LOCL_H
# include <openssl/opensslconf.h>
# undef c2l
# define c2l(c,l) (l =((unsigned long)(*((c)++))) , \
l|=((unsigned long)(*((c)++)))<< 8L, \
l|=((unsigned long)(*((c)++)))<<16L, \
l|=((unsigned long)(*((c)++)))<<24L)
# undef c2ln
# define c2ln(c,l1,l2,n) { \
c+=n; \
l1=l2=0; \
switch (n) { \
case 8: l2 =((unsigned long)(*(--(c))))<<24L; \
case 7: l2|=((unsigned long)(*(--(c))))<<16L; \
case 6: l2|=((unsigned long)(*(--(c))))<< 8L; \
case 5: l2|=((unsigned long)(*(--(c)))); \
case 4: l1 =((unsigned long)(*(--(c))))<<24L; \
case 3: l1|=((unsigned long)(*(--(c))))<<16L; \
case 2: l1|=((unsigned long)(*(--(c))))<< 8L; \
case 1: l1|=((unsigned long)(*(--(c)))); \
} \
}
# undef l2c
# define l2c(l,c) (*((c)++)=(unsigned char)(((l) )&0xff), \
*((c)++)=(unsigned char)(((l)>> 8L)&0xff), \
*((c)++)=(unsigned char)(((l)>>16L)&0xff), \
*((c)++)=(unsigned char)(((l)>>24L)&0xff))
# undef l2cn
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
# define n2ln(c,l1,l2,n) { \
c+=n; \
l1=l2=0; \
switch (n) { \
case 8: l2 =((unsigned long)(*(--(c)))) ; \
case 7: l2|=((unsigned long)(*(--(c))))<< 8; \
case 6: l2|=((unsigned long)(*(--(c))))<<16; \
case 5: l2|=((unsigned long)(*(--(c))))<<24; \
case 4: l1 =((unsigned long)(*(--(c)))) ; \
case 3: l1|=((unsigned long)(*(--(c))))<< 8; \
case 2: l1|=((unsigned long)(*(--(c))))<<16; \
case 1: l1|=((unsigned long)(*(--(c))))<<24; \
} \
}
# define l2nn(l1,l2,c,n) { \
c+=n; \
switch (n) { \
case 8: *(--(c))=(unsigned char)(((l2) )&0xff); \
case 7: *(--(c))=(unsigned char)(((l2)>> 8)&0xff); \
case 6: *(--(c))=(unsigned char)(((l2)>>16)&0xff); \
case 5: *(--(c))=(unsigned char)(((l2)>>24)&0xff); \
case 4: *(--(c))=(unsigned char)(((l1) )&0xff); \
case 3: *(--(c))=(unsigned char)(((l1)>> 8)&0xff); \
case 2: *(--(c))=(unsigned char)(((l1)>>16)&0xff); \
case 1: *(--(c))=(unsigned char)(((l1)>>24)&0xff); \
} \
}
# undef n2l
# define n2l(c,l) (l =((unsigned long)(*((c)++)))<<24L, \
l|=((unsigned long)(*((c)++)))<<16L, \
l|=((unsigned long)(*((c)++)))<< 8L, \
l|=((unsigned long)(*((c)++))))
# undef l2n
# define l2n(l,c) (*((c)++)=(unsigned char)(((l)>>24L)&0xff), \
*((c)++)=(unsigned char)(((l)>>16L)&0xff), \
*((c)++)=(unsigned char)(((l)>> 8L)&0xff), \
*((c)++)=(unsigned char)(((l) )&0xff))
# if defined(BF_PTR2)
# define BF_ENC(LL,R,KEY,Pi) (\
LL^=KEY[Pi], \
t= KEY[BF_ROUNDS+2 + 0 + ((R>>24)&0xFF)], \
t+= KEY[BF_ROUNDS+2 + 256 + ((R>>16)&0xFF)], \
t^= KEY[BF_ROUNDS+2 + 512 + ((R>>8 )&0xFF)], \
t+= KEY[BF_ROUNDS+2 + 768 + ((R )&0xFF)], \
LL^=t \
)
# elif defined(BF_PTR)
# ifndef BF_LONG_LOG2
# define BF_LONG_LOG2 2
# endif
# define BF_M (0xFF<<BF_LONG_LOG2)
# define BF_0 (24-BF_LONG_LOG2)
# define BF_1 (16-BF_LONG_LOG2)
# define BF_2 ( 8-BF_LONG_LOG2)
# define BF_3 BF_LONG_LOG2
# define BF_ENC(LL,R,S,P) ( \
LL^=P, \
LL^= (((*(BF_LONG *)((unsigned char *)&(S[ 0])+((R>>BF_0)&BF_M))+ \
*(BF_LONG *)((unsigned char *)&(S[256])+((R>>BF_1)&BF_M)))^ \
*(BF_LONG *)((unsigned char *)&(S[512])+((R>>BF_2)&BF_M)))+ \
*(BF_LONG *)((unsigned char *)&(S[768])+((R<<BF_3)&BF_M))) \
)
# else
# define BF_ENC(LL,R,S,P) ( \
LL^=P, \
LL^=((( S[ ((int)(R>>24)&0xff)] + \
S[0x0100+((int)(R>>16)&0xff)])^ \
S[0x0200+((int)(R>> 8)&0xff)])+ \
S[0x0300+((int)(R )&0xff)])&0xffffffffL \
)
# endif
#endif