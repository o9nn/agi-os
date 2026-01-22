#define _K_ 1024
#define _M_ 1048576
#define _G_ 1073741824
#define _T_ 1099511627776UL
#define BI2BY 8
#define BI2WD 32
#define BY2WD 4
#define BY2V 8
#define BY2PG 4096
#define WD2PG (BY2PG/BY2WD)
#define PGSHIFT 12
#define ROUND(s, sz) (((s)+(sz-1))&~(sz-1))
#define PGROUND(s) ROUND(s, BY2PG)
#define BIT(n) (1<<n)
#define BITS(a,b) ((1<<(b+1))-(1<<a))
#define MAXMACH 1
#define HZ (100)
#define MS2HZ (1000/HZ)
#define TK2SEC(t) ((t)/HZ)
#define MS2TK(t) ((t)/MS2HZ)
#define TIMER_HZ 50000000
#define MS2TMR(t) ((ulong)(((uvlong)(t)*TIMER_HZ)/1000))
#define US2TMR(t) ((ulong)(((uvlong)(t)*TIMER_HZ)/1000000))
#define KZERO 0x0
#define MACHADDR ((ulong)&Mach0)
#define KTZERO bootparam->entry
#define KSTACK 8192
#include "armv7.h"