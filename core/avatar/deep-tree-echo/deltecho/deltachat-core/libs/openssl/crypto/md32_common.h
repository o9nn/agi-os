#if !defined(DATA_ORDER_IS_BIG_ENDIAN) && !defined(DATA_ORDER_IS_LITTLE_ENDIAN)
# error "DATA_ORDER must be defined!"
#endif
#ifndef HASH_CBLOCK
# error "HASH_CBLOCK must be defined!"
#endif
#ifndef HASH_LONG
# error "HASH_LONG must be defined!"
#endif
#ifndef HASH_CTX
# error "HASH_CTX must be defined!"
#endif
#ifndef HASH_UPDATE
# error "HASH_UPDATE must be defined!"
#endif
#ifndef HASH_TRANSFORM
# error "HASH_TRANSFORM must be defined!"
#endif
#ifndef HASH_FINAL
# error "HASH_FINAL must be defined!"
#endif
#ifndef HASH_BLOCK_DATA_ORDER
# error "HASH_BLOCK_DATA_ORDER must be defined!"
#endif
#undef ROTATE
#ifndef PEDANTIC
# if defined(_MSC_VER)
#  define ROTATE(a,n)   _lrotl(a,n)
# elif defined(__ICC)
#  define ROTATE(a,n)   _rotl(a,n)
# elif defined(__MWERKS__)
#  if defined(__POWERPC__)
#   define ROTATE(a,n)  __rlwinm(a,n,0,31)
#  elif defined(__MC68K__)
#   define ROTATE(a,n)  ( n<24 ? __rol(a,n) : __ror(a,32-n) )
#  else
#   define ROTATE(a,n)  __rol(a,n)
#  endif
# elif defined(__GNUC__) && __GNUC__>=2 && !defined(OPENSSL_NO_ASM) && !defined(OPENSSL_NO_INLINE_ASM)
#  if defined(__i386) || defined(__i386__) || defined(__x86_64) || defined(__x86_64__)
#   define ROTATE(a,n)  ({ register unsigned int ret;   \
asm (                   \
"roll %1,%0"            \
: "=r"(ret)             \
: "I"(n), "0"((unsigned int)(a))        \
: "cc");                \
ret;                         \
})
#  elif defined(_ARCH_PPC) || defined(_ARCH_PPC64) || \
defined(__powerpc) || defined(__ppc__) || defined(__powerpc64__)
#   define ROTATE(a,n)  ({ register unsigned int ret;   \
asm (                   \
"rlwinm %0,%1,%2,0,31"  \
: "=r"(ret)             \
: "r"(a), "I"(n));      \
ret;                         \
})
#  elif defined(__s390x__)
#   define ROTATE(a,n) ({ register unsigned int ret;    \
asm ("rll %0,%1,%2"     \
: "=r"(ret)             \
: "r"(a), "I"(n));      \
ret;                          \
})
#  endif
# endif
#endif
#ifndef ROTATE
# define ROTATE(a,n)     (((a)<<(n))|(((a)&0xffffffff)>>(32-(n))))
#endif
#if defined(DATA_ORDER_IS_BIG_ENDIAN)
# ifndef PEDANTIC
#  if defined(__GNUC__) && __GNUC__>=2 && !defined(OPENSSL_NO_ASM) && !defined(OPENSSL_NO_INLINE_ASM)
#   if ((defined(__i386) || defined(__i386__)) && !defined(I386_ONLY)) || \
(defined(__x86_64) || defined(__x86_64__))
#    if !defined(B_ENDIAN)
#     define HOST_c2l(c,l)        ({ unsigned int r=*((const unsigned int *)(c)); \
asm ("bswapl %0":"=r"(r):"0"(r));    \
(c)+=4; (l)=r;                       })
#     define HOST_l2c(l,c)        ({ unsigned int r=(l);                  \
asm ("bswapl %0":"=r"(r):"0"(r));    \
*((unsigned int *)(c))=r; (c)+=4; r; })
#    endif
#   endif
#  endif
# endif
# if defined(__s390__) || defined(__s390x__)
#  define HOST_c2l(c,l) ((l)=*((const unsigned int *)(c)), (c)+=4, (l))
#  define HOST_l2c(l,c) (*((unsigned int *)(c))=(l), (c)+=4, (l))
# endif
# ifndef HOST_c2l
#  define HOST_c2l(c,l)   (l =(((unsigned long)(*((c)++)))<<24),          \
l|=(((unsigned long)(*((c)++)))<<16),          \
l|=(((unsigned long)(*((c)++)))<< 8),          \
l|=(((unsigned long)(*((c)++)))    )           )
# endif
# ifndef HOST_l2c
#  define HOST_l2c(l,c)   (*((c)++)=(unsigned char)(((l)>>24)&0xff),      \
*((c)++)=(unsigned char)(((l)>>16)&0xff),      \
*((c)++)=(unsigned char)(((l)>> 8)&0xff),      \
*((c)++)=(unsigned char)(((l)    )&0xff),      \
l)
# endif
#elif defined(DATA_ORDER_IS_LITTLE_ENDIAN)
# ifndef PEDANTIC
#  if defined(__GNUC__) && __GNUC__>=2 && !defined(OPENSSL_NO_ASM) && !defined(OPENSSL_NO_INLINE_ASM)
#   if defined(__s390x__)
#    define HOST_c2l(c,l)        ({ asm ("lrv    %0,%1"                  \
:"=d"(l) :"m"(*(const unsigned int *)(c)));\
(c)+=4; (l);                         })
#    define HOST_l2c(l,c)        ({ asm ("strv   %1,%0"                  \
:"=m"(*(unsigned int *)(c)) :"d"(l));\
(c)+=4; (l);                         })
#   endif
#  endif
# endif
# if defined(__i386) || defined(__i386__) || defined(__x86_64) || defined(__x86_64__)
#  ifndef B_ENDIAN
#   define HOST_c2l(c,l) ((l)=*((const unsigned int *)(c)), (c)+=4, l)
#   define HOST_l2c(l,c) (*((unsigned int *)(c))=(l), (c)+=4, l)
#  endif
# endif
# ifndef HOST_c2l
#  define HOST_c2l(c,l)   (l =(((unsigned long)(*((c)++)))    ),          \
l|=(((unsigned long)(*((c)++)))<< 8),          \
l|=(((unsigned long)(*((c)++)))<<16),          \
l|=(((unsigned long)(*((c)++)))<<24)           )
# endif
# ifndef HOST_l2c
#  define HOST_l2c(l,c)   (*((c)++)=(unsigned char)(((l)    )&0xff),      \
*((c)++)=(unsigned char)(((l)>> 8)&0xff),      \
*((c)++)=(unsigned char)(((l)>>16)&0xff),      \
*((c)++)=(unsigned char)(((l)>>24)&0xff),      \
l)
# endif
#endif
int HASH_UPDATE(HASH_CTX *c, const void *data_, size_t len)
{
const unsigned char *data = data_;
unsigned char *p;
HASH_LONG l;
size_t n;
if (len == 0)
return 1;
l = (c->Nl + (((HASH_LONG) len) << 3)) & 0xffffffffUL;
if (l < c->Nl)
c->Nh++;
c->Nh += (HASH_LONG) (len >> 29);
c->Nl = l;
n = c->num;
if (n != 0) {
p = (unsigned char *)c->data;
if (len >= HASH_CBLOCK || len + n >= HASH_CBLOCK) {
memcpy(p + n, data, HASH_CBLOCK - n);
HASH_BLOCK_DATA_ORDER(c, p, 1);
n = HASH_CBLOCK - n;
data += n;
len -= n;
c->num = 0;
memset(p, 0, HASH_CBLOCK);
} else {
memcpy(p + n, data, len);
c->num += (unsigned int)len;
return 1;
}
}
n = len / HASH_CBLOCK;
if (n > 0) {
HASH_BLOCK_DATA_ORDER(c, data, n);
n *= HASH_CBLOCK;
data += n;
len -= n;
}
if (len != 0) {
p = (unsigned char *)c->data;
c->num = (unsigned int)len;
memcpy(p, data, len);
}
return 1;
}
void HASH_TRANSFORM(HASH_CTX *c, const unsigned char *data)
{
HASH_BLOCK_DATA_ORDER(c, data, 1);
}
int HASH_FINAL(unsigned char *md, HASH_CTX *c)
{
unsigned char *p = (unsigned char *)c->data;
size_t n = c->num;
p[n] = 0x80;
n++;
if (n > (HASH_CBLOCK - 8)) {
memset(p + n, 0, HASH_CBLOCK - n);
n = 0;
HASH_BLOCK_DATA_ORDER(c, p, 1);
}
memset(p + n, 0, HASH_CBLOCK - 8 - n);
p += HASH_CBLOCK - 8;
#if   defined(DATA_ORDER_IS_BIG_ENDIAN)
(void)HOST_l2c(c->Nh, p);
(void)HOST_l2c(c->Nl, p);
#elif defined(DATA_ORDER_IS_LITTLE_ENDIAN)
(void)HOST_l2c(c->Nl, p);
(void)HOST_l2c(c->Nh, p);
#endif
p -= HASH_CBLOCK;
HASH_BLOCK_DATA_ORDER(c, p, 1);
c->num = 0;
memset(p, 0, HASH_CBLOCK);
#ifndef HASH_MAKE_STRING
# error "HASH_MAKE_STRING must be defined!"
#else
HASH_MAKE_STRING(c, md);
#endif
return 1;
}
#ifndef MD32_REG_T
# if defined(__alpha) || defined(__sparcv9) || defined(__mips)
#  define MD32_REG_T long
# else
#  define MD32_REG_T int
# endif
#endif