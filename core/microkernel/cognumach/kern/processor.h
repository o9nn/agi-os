#ifndef	_KERN_PROCESSOR_H_
#define	_KERN_PROCESSOR_H_
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/port.h>
#include <mach/processor_info.h>
#include <kern/lock.h>
#include <kern/queue.h>
#include <kern/sched.h>
#include <kern/kern_types.h>
#include <kern/host.h>
#if	NCPUS > 1
#include <machine/ast_types.h>
#endif
struct processor_set {
struct run_queue	runq;
queue_head_t		idle_queue;
int			idle_count;
decl_simple_lock_data(,	idle_lock)
queue_head_t		processors;
int			processor_count;
boolean_t		empty;
queue_head_t		tasks;
int			task_count;
queue_head_t		threads;
int			thread_count;
int			ref_count;
decl_simple_lock_data(,	ref_lock)
queue_chain_t		all_psets;
boolean_t		active;
decl_simple_lock_data(,	lock)
struct ipc_port	*	pset_self;
struct ipc_port *	pset_name_self;
int			max_priority;
#if	MACH_FIXPRI
int			policies;
#endif
int			set_quantum;
#if	NCPUS > 1
int			quantum_adj_index;
decl_simple_lock_irq_data(, quantum_adj_lock)
int			machine_quantum[NCPUS+1];
#endif
long			mach_factor;
long			load_average;
long			sched_load;
};
extern struct processor_set	default_pset;
#if	MACH_HOST
extern struct processor_set	*slave_pset;
#endif
#ifdef MACH_LDEBUG
#define pset_idle_lock()	\
MACRO_BEGIN \
assert_splsched(); \
simple_lock_nocheck(&pset->idle_lock); \
MACRO_END
#define pset_idle_unlock()	\
MACRO_BEGIN \
assert_splsched(); \
simple_unlock_nocheck(&pset->idle_lock); \
MACRO_END
#else
#define pset_idle_lock()	simple_lock_nocheck(&pset->idle_lock)
#define pset_idle_unlock()	simple_unlock_nocheck(&pset->idle_lock)
#endif
struct processor {
struct run_queue runq;
queue_chain_t	processor_queue;
int		state;
struct thread	*next_thread;
struct thread	*idle_thread;
int		quantum;
boolean_t	first_quantum;
int		last_quantum;
processor_set_t	processor_set;
processor_set_t processor_set_next;
queue_chain_t	processors;
decl_simple_lock_data(,	lock)
struct ipc_port *processor_self;
int		slot_num;
#if	NCPUS > 1
ast_check_t	ast_check_data;
unsigned int	load_average;
unsigned int	migration_in;
unsigned int	migration_out;
unsigned int	last_balance_tick;
#endif
};
typedef struct processor Processor;
extern struct processor	processor_array[NCPUS];
#include <kern/cpu_number.h>
#include <machine/percpu.h>
extern queue_head_t		all_psets;
extern int			all_psets_count;
decl_simple_lock_data(extern, all_psets_lock);
extern processor_t	master_processor;
#define PROCESSOR_OFF_LINE	0
#define	PROCESSOR_RUNNING	1
#define	PROCESSOR_IDLE		2
#define PROCESSOR_DISPATCHING	3
#define	PROCESSOR_ASSIGN	4
#define PROCESSOR_SHUTDOWN	5
#define processor_ptr(i)	(&percpu_array[i].processor)
#define cpu_to_processor	processor_ptr
#define current_processor()	(percpu_ptr(struct processor, processor))
#define current_processor_set()	(current_processor()->processor_set)
#define cpu_state(slot_num)	(processor_ptr(slot_num)->state)
#define cpu_idle(slot_num)	(cpu_state(slot_num) == PROCESSOR_IDLE)
#define	pset_lock(pset)		simple_lock(&(pset)->lock)
#define pset_unlock(pset)	simple_unlock(&(pset)->lock)
#define	pset_ref_lock(pset)	simple_lock(&(pset)->ref_lock)
#define	pset_ref_unlock(pset)	simple_unlock(&(pset)->ref_lock)
#define processor_lock(pr)	simple_lock(&(pr)->lock)
#define processor_unlock(pr)	simple_unlock(&(pr)->lock)
typedef mach_port_t	*processor_array_t;
typedef mach_port_t	*processor_set_array_t;
typedef mach_port_t	*processor_set_name_array_t;
#ifdef KERNEL
#if	MACH_HOST
extern void	pset_sys_init(void);
#endif
extern void	pset_sys_bootstrap(void);
extern void	pset_reference(processor_set_t);
extern void	pset_deallocate(processor_set_t);
extern void	pset_remove_processor(processor_set_t, processor_t);
extern void	pset_add_processor(processor_set_t, processor_t);
extern void	pset_remove_task(processor_set_t, struct task *);
extern void	pset_add_task(processor_set_t, struct task *);
extern void	pset_remove_thread(processor_set_t, struct thread *);
extern void	pset_add_thread(processor_set_t, struct thread *);
extern void	thread_change_psets(struct thread *,
processor_set_t, processor_set_t);
extern kern_return_t processor_get_assignment(
processor_t	processor,
processor_set_t *processor_set);
extern kern_return_t processor_info(
processor_t	processor,
int		flavor,
host_t *	host,
processor_info_t info,
natural_t *	count);
extern kern_return_t processor_start(
processor_t	processor);
extern kern_return_t processor_exit(
processor_t	processor);
extern kern_return_t processor_control(
processor_t	processor,
processor_info_t info,
natural_t 	count);
extern kern_return_t processor_set_create(
host_t		host,
processor_set_t *new_set,
processor_set_t *new_name);
extern kern_return_t processor_set_destroy(
processor_set_t	pset);
extern kern_return_t processor_set_info(
processor_set_t	pset,
int		flavor,
host_t		*host,
processor_set_info_t info,
natural_t	*count);
extern kern_return_t processor_set_max_priority(
processor_set_t	pset,
int		max_priority,
boolean_t	change_threads);
extern kern_return_t processor_set_policy_enable(
processor_set_t	pset,
int		policy);
extern kern_return_t processor_set_policy_disable(
processor_set_t	pset,
int		policy,
boolean_t	change_threads);
extern kern_return_t processor_set_tasks(
processor_set_t	pset,
task_array_t	*task_list,
natural_t	*count);
extern kern_return_t processor_set_threads(
processor_set_t	pset,
thread_array_t	*thread_list,
natural_t	*count);
#endif
void processor_doshutdown(processor_t processor);
void quantum_set(processor_set_t pset);
void pset_init(processor_set_t pset);
void processor_init(processor_t pr, int slot_num);
#endif