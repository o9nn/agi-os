#ifndef	_KTTD_MACHDEP_H_
#define	_KTTD_MACHDEP_H_
#define MAX_KTTD_ACTIVE	2
#define MIN_KTTD_ACTIVE	0
struct i386_gdb_register_state {
int	eax;
int	ecx;
int	edx;
int	ebx;
int	esp;
int	ebp;
int	esi;
int	edi;
int	eip;
int	efl;
int	cs;
int	ss;
int	ds;
int	es;
int	fs;
int	gs;
};
typedef struct i386_gdb_register_state ttd_machine_state;
typedef unsigned long ttd_saved_inst;
#endif