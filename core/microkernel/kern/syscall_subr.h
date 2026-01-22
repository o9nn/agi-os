#include <sys/types.h>
#include <mach/mach_types.h>
#include <kern/kern_types.h>
#ifndef	_KERN_SYSCALL_SUBR_H_
#define _KERN_SYSCALL_SUBR_H_
extern int	swtch(void);
extern int	swtch_pri(int);
extern int	thread_switch(mach_port_name_t, int, mach_msg_timeout_t);
extern void	thread_depress_timeout(thread_t);
extern kern_return_t thread_depress_abort(thread_t);
extern void	mach_print(const char *);
extern void thread_depress_priority(thread_t thread, mach_msg_timeout_t depress_time);
#endif