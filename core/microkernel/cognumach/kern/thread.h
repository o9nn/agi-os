#ifndef _KERN_THREAD_H_
#define _KERN_THREAD_H_
#include <mach/boolean.h>
#include <mach/thread_info.h>
#include <mach/thread_status.h>
#include <mach/machine/vm_types.h>
#include <mach/message.h>
#include <mach/port.h>
#include <mach/vm_prot.h>
#include <kern/ast.h>
#include <kern/mach_clock.h>
#include <kern/queue.h>
#include <kern/pc_sample.h>
#include <kern/processor.h>
#include <kern/sched_prim.h>
#include <kern/timer.h>
#include <kern/lock.h>
#include <kern/sched.h>
#include <kern/task.h>
#include <machine/thread.h>
#include <ipc/ipc_kmsg_queue.h>
#if NCPUS > 1
#include <kern/smp.h>
#endif
#define THREAD_NAME_SIZE TASK_NAME_SIZE
struct thread {
queue_chain_t links;
run_queue_t runq;
task_t task;
queue_chain_t thread_list;
union {
struct {
unsigned state:16;
unsigned wake_active:1;
unsigned active:1;
};
event_t event_key;
#define TH_EV_WAKE_ACTIVE(t) ((event_t) (&(t)->event_key + 0))
#define TH_EV_STATE(t) ((event_t) (&(t)->event_key + 1))
};
queue_chain_t pset_threads;
decl_simple_lock_data(,lock)
int ref_count;
pcb_t pcb;
vm_offset_t kernel_stack;
vm_offset_t stack_privilege;
continuation_t swap_func;
event_t wait_event;
int suspend_count;
kern_return_t wait_result;
#define TH_WAIT 0x01
#define TH_SUSP 0x02
#define TH_RUN 0x04
#define TH_UNINT 0x08
#define TH_HALTED 0x10
#define TH_IDLE 0x80
#define TH_SCHED_STATE (TH_WAIT|TH_SUSP|TH_RUN|TH_UNINT)
#define TH_SWAPPED 0x0100
#define TH_SW_COMING_IN 0x0200
#define TH_SWAP_STATE (TH_SWAPPED | TH_SW_COMING_IN)
int priority;
int max_priority;
int sched_pri;
#if MACH_FIXPRI
int sched_data;
int policy;
#endif
int depress_priority;
unsigned int cpu_usage;
unsigned int sched_usage;
unsigned int sched_stamp;
vm_offset_t recover;
unsigned int vm_privilege;
int user_stop_count;
struct thread *ith_next, *ith_prev;
mach_msg_return_t ith_state;
union {
mach_msg_size_t msize;
struct ipc_kmsg *kmsg;
} data;
mach_port_seqno_t ith_seqno;
struct ipc_kmsg_queue ith_messages;
decl_simple_lock_data(, ith_lock_data)
struct ipc_port *ith_self;
struct ipc_port *ith_sself;
struct ipc_port *ith_exception;
mach_port_name_t ith_mig_reply;
struct ipc_port *ith_rpc_reply;
union {
struct {
mach_msg_user_header_t *msg;
mach_msg_option_t option;
mach_msg_size_t rcv_size;
mach_msg_timeout_t timeout;
mach_port_name_t notify;
struct ipc_object *object;
struct ipc_mqueue *mqueue;
} receive;
struct {
struct ipc_port *port;
int exc;
int code;
long subcode;
} exception;
void *other;
} saved;
timer_data_t user_timer;
timer_data_t system_timer;
timer_save_data_t user_timer_save;
timer_save_data_t system_timer_save;
unsigned int cpu_delta;
unsigned int sched_delta;
time_value64_t creation_time;
timer_elt_data_t timer;
timer_elt_data_t depress_timer;
int ast;
processor_set_t processor_set;
processor_t bound_processor;
#if NCPUS > 1
cpu_mask_t cpu_affinity;
#endif
sample_control_t pc_sample;
#if MACH_HOST
boolean_t may_assign;
boolean_t assign_active;
#endif
#if NCPUS > 1
processor_t last_processor;
unsigned int migration_count;
unsigned int cache_warmth;
#endif
#if MACH_LOCK_MON
unsigned lock_stack;
#endif
char name[THREAD_NAME_SIZE];
};
#include <kern/cpu_number.h>
typedef struct thread_shuttle *thread_shuttle_t;
#define THREAD_NULL ((thread_t) 0)
#define THREAD_SHUTTLE_NULL ((thread_shuttle_t)0)
#define ith_msize data.msize
#define ith_kmsg data.kmsg
#define ith_wait_result wait_result
#define ith_msg saved.receive.msg
#define ith_option saved.receive.option
#define ith_rcv_size saved.receive.rcv_size
#define ith_timeout saved.receive.timeout
#define ith_notify saved.receive.notify
#define ith_object saved.receive.object
#define ith_mqueue saved.receive.mqueue
#define ith_port saved.exception.port
#define ith_exc saved.exception.exc
#define ith_exc_code saved.exception.code
#define ith_exc_subcode saved.exception.subcode
#define ith_other saved.other
#ifndef _KERN_KERN_TYPES_H_
typedef struct thread *thread_t;
#define THREAD_NULL ((thread_t) 0)
typedef mach_port_t *thread_array_t;
#endif
#ifdef KERNEL
extern kern_return_t thread_create(
task_t parent_task,
thread_t *child_thread);
extern kern_return_t thread_terminate(
thread_t thread);
extern kern_return_t thread_terminate_release(
thread_t thread,
task_t task,
mach_port_name_t thread_name,
mach_port_name_t reply_port,
vm_offset_t address,
vm_size_t size);
extern kern_return_t thread_suspend(
thread_t thread);
extern kern_return_t thread_resume(
thread_t thread);
extern kern_return_t thread_abort(
thread_t thread);
extern void thread_start(
thread_t thread,
continuation_t start);
extern kern_return_t thread_priority(
thread_t thread,
int priority,
boolean_t set_max);
extern void thread_set_own_priority(
int priority);
extern kern_return_t thread_max_priority(
thread_t thread,
processor_set_t pset,
int max_priority);
extern kern_return_t thread_policy(
thread_t thread,
int policy,
int data);
extern void consider_thread_collect(
void);
extern void stack_privilege(
thread_t thread);
extern kern_return_t thread_get_state(
thread_t thread,
int flavor,
thread_state_t old_state,
natural_t *old_state_count);
extern kern_return_t thread_set_state(
thread_t thread,
int flavor,
thread_state_t new_state,
natural_t new_state_count);
extern kern_return_t thread_get_special_port(
thread_t thread,
int which,
struct ipc_port **portp);
extern kern_return_t thread_set_special_port(
thread_t thread,
int which,
struct ipc_port *port);
extern kern_return_t thread_info(
thread_t thread,
int flavor,
thread_info_t thread_info_out,
natural_t *thread_info_count);
extern kern_return_t thread_assign(
thread_t thread,
processor_set_t new_pset);
extern kern_return_t thread_assign_default(
thread_t thread);
extern void stack_collect(void);
extern kern_return_t thread_set_name(
thread_t thread,
const_kernel_debug_name_t name);
extern kern_return_t thread_get_name(
thread_t thread,
kernel_debug_name_t name);
#endif
extern void thread_init(void);
extern void thread_reference(thread_t);
extern void thread_deallocate(thread_t);
extern void thread_hold(thread_t);
extern kern_return_t thread_dowait(
thread_t thread,
boolean_t must_halt);
extern void thread_release(thread_t);
extern kern_return_t thread_halt(
thread_t thread,
boolean_t must_halt);
extern void thread_halt_self(continuation_t);
extern void thread_force_terminate(thread_t);
extern thread_t kernel_thread(
task_t task,
const char * name,
void (*start)(void),
void * arg);
extern void reaper_thread(void) __attribute__((noreturn));
#if NCPUS > 1
extern kern_return_t thread_set_cpu_affinity(
thread_t thread,
cpu_mask_t affinity_mask);
extern cpu_mask_t thread_get_cpu_affinity(
thread_t thread);
extern boolean_t thread_can_run_on_cpu(
thread_t thread,
int cpu);
#endif
#if MACH_HOST
extern void thread_freeze(
thread_t thread);
extern void thread_doassign(
thread_t thread,
processor_set_t new_pset,
boolean_t release_freeze);
extern void thread_unfreeze(
thread_t thread);
#endif
#define thread_pcb(th) ((th)->pcb)
#ifdef MACH_LDEBUG
#define thread_lock(th) \
MACRO_BEGIN \
assert_splsched(); \
simple_lock_nocheck(&(th)->lock); \
MACRO_END
#define thread_unlock(th) \
MACRO_BEGIN \
assert_splsched(); \
simple_unlock_nocheck(&(th)->lock); \
MACRO_END
#else
#define thread_lock(th) simple_lock_nocheck(&(th)->lock)
#define thread_unlock(th) simple_unlock_nocheck(&(th)->lock)
#endif
#define thread_should_halt(thread) \
((thread)->ast & (AST_HALT|AST_TERMINATE))
#ifndef CURRENT_THREAD
#define current_thread() (percpu_get(thread_t, active_thread))
#endif
#define current_stack() (percpu_get(vm_offset_t, active_stack))
#define current_task() (current_thread()->task)
#define current_space() (current_task()->itk_space)
#define current_map() (current_task()->map)
#if MACH_DEBUG
void stack_init(vm_offset_t stack);
void stack_finalize(vm_offset_t stack);
void thread_stats(void);
#endif
#endif