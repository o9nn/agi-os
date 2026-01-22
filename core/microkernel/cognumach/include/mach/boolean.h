#ifndef	_MACH_BOOLEAN_H_
#define	_MACH_BOOLEAN_H_
#ifndef	__ASSEMBLER__
#include <mach/machine/boolean.h>
#endif
#endif
#if	!defined(NOBOOL)
#ifndef	TRUE
#define TRUE	((boolean_t) 1)
#endif
#ifndef	FALSE
#define FALSE	((boolean_t) 0)
#endif
#endif