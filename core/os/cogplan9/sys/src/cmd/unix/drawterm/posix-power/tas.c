#include "u.h"
#include "libc.h"
int
tas(long *x)
{
int     v;
__asm__("\n	sync\n"
"	li	r0,0\n"
"	mr	r4,%1		\n"
"	lis	r5,0xdead	\n"
"	ori	r5,r5,0xdead	\n"
"tas1:\n"
"	dcbf	r4,r0	\n"
"	lwarx	%0,r4,r0	\n"
"	cmp	cr0,0,%0,r0	\n"
"	bne	tas0\n"
"	stwcx.	r5,r4,r0   \n"
"	bne	tas1\n"
"tas0:\n"
"	sync\n"
"	isync\n"
: "=r" (v)
: "r"  (x)
: "cc", "memory", "r0", "r4", "r5"
);
switch(v) {
case 0:		return 0;
case 0xdeaddead: return 1;
default:	print("tas: corrupted 0x%lux\n", v);
}
return 0;
}