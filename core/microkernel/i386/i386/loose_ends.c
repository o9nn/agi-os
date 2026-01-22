#include <i386/i386/loose_ends.h>
#ifndef NDEBUG
#define MACH_ASSERT 1
#else
#define MACH_ASSERT 0
#endif
int cpuspeed = 4;
#define	DELAY(n)	{ volatile int N = cpuspeed * (n); while (--N > 0); }
void
delay(int n)
{
DELAY(n);
}