#ifndef _KERN_MACH_CLOCK_H_
#define _KERN_MACH_CLOCK_H_
#include <mach/machine/kern_return.h>
#include <mach/time_value.h>
#include <kern/host.h>
#include <kern/queue.h>
#include <sys/types.h>
struct io_req;
typedef struct io_req *io_req_t;
extern unsigned long	elapsed_ticks;
extern int		hz;
extern int		tick;
extern time_value64_t	time;
extern time_value64_t	uptime;
typedef void timer_func_t(void *);
struct timer_elt {
queue_chain_t	chain;
timer_func_t	*fcn;
void *		param;
unsigned long	ticks;
int		set;
};
#define	TELT_UNSET	0
#define	TELT_SET	1
#define	TELT_ALLOC	2
typedef	struct timer_elt	timer_elt_data_t;
typedef	struct timer_elt	*timer_elt_t;
extern void clock_interrupt(
int usec,
boolean_t usermode,
boolean_t basepri,
vm_offset_t pc);
extern void softclock (void);
extern void set_timeout(
timer_elt_t telt,
unsigned int interval);
extern boolean_t reset_timeout(timer_elt_t telt);
#define	set_timeout_setup(telt,fcn,param,interval)	\
((telt)->fcn = (fcn),				\
(telt)->param = (param),			\
(telt)->private = TRUE,			\
set_timeout((telt), (interval)))
#define	reset_timeout_check(t)				\
MACRO_BEGIN					\
if ((t)->set)					\
reset_timeout((t));				\
MACRO_END
extern void init_timeout (void);
extern void record_time_stamp (time_value64_t *stamp);
extern void read_time_stamp (const time_value64_t *stamp, time_value64_t *result);
extern void mapable_time_init (void);
extern void timeout(timer_func_t *fcn, void *param, int interval);
extern boolean_t untimeout(timer_func_t *fcn, const void *param);
extern int timeopen(dev_t dev, int flag, io_req_t ior);
extern void timeclose(dev_t dev, int flag);
extern uint32_t hpclock_read_counter(void);
extern uint32_t hpclock_get_counter_period_nsec(void);
#endif