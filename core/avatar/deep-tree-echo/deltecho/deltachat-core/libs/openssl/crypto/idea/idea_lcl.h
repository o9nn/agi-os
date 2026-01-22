#define idea_mul(r,a,b,ul) \
ul=(unsigned long)a*b; \
if (ul != 0) \
{ \
r=(ul&0xffff)-(ul>>16); \
r-=((r)>>16); \
} \
else \
r=(-(int)a-b+1);
#ifdef undef
# define idea_mul(r,a,b,ul,sl) \
if (a == 0) r=(0x10001-b)&0xffff; \
else if (b == 0) r=(0x10001-a)&0xffff; \
else    { \
ul=(unsigned long)a*b; \
sl=(ul&0xffff)-(ul>>16); \
if (sl <= 0) sl+=0x10001; \
r=sl; \
}
#endif
#define n2ln(c,l1,l2,n) { \
c+=n; \
l1=l2=0; \
switch (n) { \
case 8: l2 =((unsigned long)(*(--(c))))    ; \
case 7: l2|=((unsigned long)(*(--(c))))<< 8; \
case 6: l2|=((unsigned long)(*(--(c))))<<16; \
case 5: l2|=((unsigned long)(*(--(c))))<<24; \
case 4: l1 =((unsigned long)(*(--(c))))    ; \
case 3: l1|=((unsigned long)(*(--(c))))<< 8; \
case 2: l1|=((unsigned long)(*(--(c))))<<16; \
case 1: l1|=((unsigned long)(*(--(c))))<<24; \
} \
}
#define l2nn(l1,l2,c,n) { \
c+=n; \
switch (n) { \
case 8: *(--(c))=(unsigned char)(((l2)    )&0xff); \
case 7: *(--(c))=(unsigned char)(((l2)>> 8)&0xff); \
case 6: *(--(c))=(unsigned char)(((l2)>>16)&0xff); \
case 5: *(--(c))=(unsigned char)(((l2)>>24)&0xff); \
case 4: *(--(c))=(unsigned char)(((l1)    )&0xff); \
case 3: *(--(c))=(unsigned char)(((l1)>> 8)&0xff); \
case 2: *(--(c))=(unsigned char)(((l1)>>16)&0xff); \
case 1: *(--(c))=(unsigned char)(((l1)>>24)&0xff); \
} \
}
#undef n2l
#define n2l(c,l)        (l =((unsigned long)(*((c)++)))<<24L, \
l|=((unsigned long)(*((c)++)))<<16L, \
l|=((unsigned long)(*((c)++)))<< 8L, \
l|=((unsigned long)(*((c)++))))
#undef l2n
#define l2n(l,c)        (*((c)++)=(unsigned char)(((l)>>24L)&0xff), \
*((c)++)=(unsigned char)(((l)>>16L)&0xff), \
*((c)++)=(unsigned char)(((l)>> 8L)&0xff), \
*((c)++)=(unsigned char)(((l)     )&0xff))
#undef s2n
#define s2n(l,c)        (*((c)++)=(unsigned char)(((l)     )&0xff), \
*((c)++)=(unsigned char)(((l)>> 8L)&0xff))
#undef n2s
#define n2s(c,l)        (l =((IDEA_INT)(*((c)++)))<< 8L, \
l|=((IDEA_INT)(*((c)++)))      )
#ifdef undef
# define c2ln(c,l1,l2,n) { \
c+=n; \
l1=l2=0; \
switch (n) { \
case 8: l2 =((unsigned long)(*(--(c))))<<24; \
case 7: l2|=((unsigned long)(*(--(c))))<<16; \
case 6: l2|=((unsigned long)(*(--(c))))<< 8; \
case 5: l2|=((unsigned long)(*(--(c))));     \
case 4: l1 =((unsigned long)(*(--(c))))<<24; \
case 3: l1|=((unsigned long)(*(--(c))))<<16; \
case 2: l1|=((unsigned long)(*(--(c))))<< 8; \
case 1: l1|=((unsigned long)(*(--(c))));     \
} \
}
# define l2cn(l1,l2,c,n) { \
c+=n; \
switch (n) { \
case 8: *(--(c))=(unsigned char)(((l2)>>24)&0xff); \
case 7: *(--(c))=(unsigned char)(((l2)>>16)&0xff); \
case 6: *(--(c))=(unsigned char)(((l2)>> 8)&0xff); \
case 5: *(--(c))=(unsigned char)(((l2)    )&0xff); \
case 4: *(--(c))=(unsigned char)(((l1)>>24)&0xff); \
case 3: *(--(c))=(unsigned char)(((l1)>>16)&0xff); \
case 2: *(--(c))=(unsigned char)(((l1)>> 8)&0xff); \
case 1: *(--(c))=(unsigned char)(((l1)    )&0xff); \
} \
}
# undef c2s
# define c2s(c,l)        (l =((unsigned long)(*((c)++)))    , \
l|=((unsigned long)(*((c)++)))<< 8L)
# undef s2c
# define s2c(l,c)        (*((c)++)=(unsigned char)(((l)     )&0xff), \
*((c)++)=(unsigned char)(((l)>> 8L)&0xff))
# undef c2l
# define c2l(c,l)        (l =((unsigned long)(*((c)++)))     , \
l|=((unsigned long)(*((c)++)))<< 8L, \
l|=((unsigned long)(*((c)++)))<<16L, \
l|=((unsigned long)(*((c)++)))<<24L)
# undef l2c
# define l2c(l,c)        (*((c)++)=(unsigned char)(((l)     )&0xff), \
*((c)++)=(unsigned char)(((l)>> 8L)&0xff), \
*((c)++)=(unsigned char)(((l)>>16L)&0xff), \
*((c)++)=(unsigned char)(((l)>>24L)&0xff))
#endif
#define E_IDEA(num) \
x1&=0xffff; \
idea_mul(x1,x1,*p,ul); p++; \
x2+= *(p++); \
x3+= *(p++); \
x4&=0xffff; \
idea_mul(x4,x4,*p,ul); p++; \
t0=(x1^x3)&0xffff; \
idea_mul(t0,t0,*p,ul); p++; \
t1=(t0+(x2^x4))&0xffff; \
idea_mul(t1,t1,*p,ul); p++; \
t0+=t1; \
x1^=t1; \
x4^=t0; \
ul=x2^t0;  \
x2=x3^t1; \
x3=ul;