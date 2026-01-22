#ifndef	_MACH_THREAD_SWITCH_H_
#define	_MACH_THREAD_SWITCH_H_
#define	SWITCH_OPTION_NONE	0
#define SWITCH_OPTION_DEPRESS	1
#define SWITCH_OPTION_WAIT	2
#define valid_switch_option(opt)	((0 <= (opt)) && ((opt) <= 2))
#endif