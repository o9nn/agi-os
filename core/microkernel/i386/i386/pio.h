#ifndef _I386_PIO_H_
#define _I386_PIO_H_
#ifndef __GNUC__
#error You do not stand a chance. This file is gcc only.
#endif
#define inl(y) \
({ unsigned int _tmp__; \
asm volatile("inl %1, %0" : "=a" (_tmp__) : "dN" ((unsigned short)(y))); \
_tmp__; })
#define inw(y) \
({ unsigned short _tmp__; \
asm volatile("inw %1, %0" : "=a" (_tmp__) : "dN" ((unsigned short)(y))); \
_tmp__; })
#define inb(y) \
({ unsigned char _tmp__; \
asm volatile("inb %1, %0" : "=a" (_tmp__) : "dN" ((unsigned short)(y))); \
_tmp__; })
#define outl(x, y) \
MACRO_BEGIN asm volatile("outl %0, %1" : : "a" ((unsigned int)(y)) , "dN" ((unsigned short)(x))); MACRO_END
#define outw(x, y) \
MACRO_BEGIN asm volatile("outw %0, %1" : : "a" ((unsigned short)(y)) , "dN" ((unsigned short)(x))); MACRO_END
#define outb(x, y) \
MACRO_BEGIN asm volatile("outb %0, %1" : : "a" ((unsigned char)(y)) , "dN" ((unsigned short)(x))); MACRO_END
#endif