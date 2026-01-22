#ifndef _I386_KTSS_
#define _I386_KTSS_
#include "tss.h"
extern struct task_tss ktss;
extern void ktss_init(void);
extern void ap_ktss_init(int cpu);
#endif