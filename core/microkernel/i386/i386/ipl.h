#ifndef _I386_IPL_H_
#define _I386_IPL_H_
#define SPL0            0
#define SPL1            1
#define SPL2            2
#define SPL3            3
#define SPL4            4
#define SPL5            5
#define SPL6            6
#define SPL7		7
#define SPLPP           5
#define SPLTTY          6
#define SPLNI           6
#define SPLHI           7
#define IPLHI           SPLHI
#define NSPL		(SPL7 + 1)
#ifdef	KERNEL
#ifndef	__ASSEMBLER__
#include <machine/spl.h>
typedef void (*interrupt_handler_fn)(int);
extern interrupt_handler_fn ivect[];
extern int	iunit[];
extern spl_t	curr_ipl[NCPUS];
#endif
#endif
#endif