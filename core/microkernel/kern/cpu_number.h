#ifndef _KERN_CPU_NUMBER_H_
#define _KERN_CPU_NUMBER_H_
#include <machine/cpu_number.h>
extern int	master_cpu;
#if	(NCPUS == 1)
#define	cpu_number()		(0)
#define	cpu_number_slow()	(0)
#endif
#define CPU_L1_SIZE (1 << CPU_L1_SHIFT)
#endif