#ifndef _LINUX_DELAY_H
#define _LINUX_DELAY_H
extern unsigned long loops_per_sec;
#include <asm/delay.h>
#ifndef MAX_UDELAY_MS
#define MAX_UDELAY_MS	5
#endif
#ifdef notdef
#define mdelay(n) (\
{unsigned long msec=(n); while (msec--) udelay(1000);})
#else
#define mdelay(n) (\
(__builtin_constant_p(n) && (n)<=MAX_UDELAY_MS) ? udelay((n)*1000) : \
({unsigned long msec=(n); while (msec--) udelay(1000);}))
#endif
#endif