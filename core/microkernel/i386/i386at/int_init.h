#ifndef _INT_INIT_H_
#define _INT_INIT_H_
#include <mach/std_types.h>
#ifndef __ASSEMBLER__
extern void int_init (void);
extern void ap_int_init (int cpu);
#endif
#endif