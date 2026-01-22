#ifndef _LINUX_MEM_OP_H
#define _LINUX_MEM_OP_H
#include <asm/uaccess.h>
#ifdef UNSAFE_MEMCPY
#define copy_from_pc memcpy_fromio
#define copy_to_pc memcpy_toio
static inline void copy_pc_to_user(void *to, const void *from, size_t n)
{
size_t odd = (n & 3);
n -= odd;
while (n) {
put_user(readl_ns(from), (int *)to);
from += 4; to += 4; n -= 4;
}
while (odd--)
put_user(readb((char *)from++), (char *)to++);
}
static inline void copy_user_to_pc(void *to, const void *from, size_t n)
{
int l;
char c;
size_t odd = (n & 3);
n -= odd;
while (n) {
l = get_user((int *)from);
writel_ns(l, to);
to += 4; from += 4; n -= 4;
}
while (odd--) {
c = get_user((char *)from++);
writeb(c, (char *)to++);
}
}
#else
static inline void copy_from_pc(void *to, const void *from, size_t n)
{
size_t odd = (n & 1);
n -= odd;
while (n) {
*(u_short *)to = readw_ns(from);
to += 2; from += 2; n -= 2;
}
if (odd)
*(u_char *)to = readb(from);
}
static inline void copy_to_pc(void *to, const void *from, size_t n)
{
size_t odd = (n & 1);
n -= odd;
while (n) {
writew_ns(*(u_short *)from, to);
to += 2; from += 2; n -= 2;
}
if (odd)
writeb(*(u_char *)from, to);
}
static inline void copy_pc_to_user(void *to, const void *from, size_t n)
{
size_t odd = (n & 1);
n -= odd;
while (n) {
put_user(readw_ns(from), (short *)to);
to += 2; from += 2; n -= 2;
}
if (odd)
put_user(readb(from), (char *)to);
}
static inline void copy_user_to_pc(void *to, const void *from, size_t n)
{
short s;
char c;
size_t odd = (n & 1);
n -= odd;
while (n) {
s = get_user((short *)from);
writew_ns(s, to);
to += 2; from += 2; n -= 2;
}
if (odd) {
c = get_user((char *)from);
writeb(c, to);
}
}
#endif
#endif