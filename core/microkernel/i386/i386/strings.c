#include <stddef.h>
#include <string.h>
#define ARCH_STRING_MEMCPY
#define ARCH_STRING_MEMMOVE
#define ARCH_STRING_MEMSET
#define ARCH_STRING_MEMCMP
#ifdef ARCH_STRING_MEMCPY
void *
memcpy(void *dest, const void *src, size_t n)
{
void *orig_dest;
orig_dest = dest;
asm volatile("rep movsb"
: "+D" (dest), "+S" (src), "+c" (n)
: : "memory");
return orig_dest;
}
#endif
#ifdef ARCH_STRING_MEMMOVE
void *
memmove(void *dest, const void *src, size_t n)
{
void *orig_dest;
orig_dest = dest;
if (dest <= src)
asm volatile("rep movsb"
: "+D" (dest), "+S" (src), "+c" (n)
: : "memory");
else {
dest += n - 1;
src += n - 1;
asm volatile("std; rep movsb; cld"
: "+D" (dest), "+S" (src), "+c" (n)
: : "memory");
}
return orig_dest;
}
#endif
#ifdef ARCH_STRING_MEMSET
void *
memset(void *s, int c, size_t n)
{
void *orig_s;
orig_s = s;
asm volatile("rep stosb"
: "+D" (s), "+c" (n)
: "a" (c)
: "memory");
return orig_s;
}
#endif
#ifdef ARCH_STRING_MEMCMP
int
memcmp(const void *s1, const void *s2, size_t n)
{
unsigned char c1, c2;
if (n == 0)
return 0;
asm volatile("repe cmpsb"
: "+D" (s1), "+S" (s2), "+c" (n)
: : "memory");
c1 = *(((const unsigned char *)s1) - 1);
c2 = *(((const unsigned char *)s2) - 1);
return (int)c1 - (int)c2;
}
#endif