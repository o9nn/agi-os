#ifndef _KERN_TASK_H_
#define _KERN_TASK_H_
#include <mach/boolean.h>
#include <mach/port.h>
#include <mach/time_value.h>
#include <mach/mach_param.h>
#include <mach/task_info.h>
#include <mach_debug/mach_debug_types.h>
#include <kern/kern_types.h>
#include <kern/lock.h>
#include <kern/queue.h>
#include <kern/pc_sample.h>
#include <kern/processor.h>
#include <kern/syscall_emulation.h>
#include <vm/vm_types.h>
#include <machine/task.h>
#define TASK_NAME_SIZE 32
struct task {
decl_simple_lock_data(,lock)
int ref_count;
unsigned char assign_active;
unsigned char active:1,
may_assign:1,
essential:1;
vm_map_t map;
queue_chain_t pset_tasks;
int suspend_count;
queue_head_t thread_list;
int thread_count;
processor_set_t processor_set;
int user_stop_count;
int priority;
time_value64_t total_user_time;
time_value64_t total_system_time;
time_value64_t creation_time;
decl_simple_lock_data(, itk_lock_data)
struct ipc_port *itk_self;
struct ipc_port *itk_sself;
struct ipc_port *itk_exception;
struct ipc_port *itk_bootstrap;
struct ipc_port *itk_registered[TASK_PORT_REGISTER_MAX];
struct ipc_space *itk_space;
struct eml_dispatch *eml_dispatch;
sample_control_t pc_sample;
#if FAST_TAS
#define TASK_FAST_TAS_NRAS 8
vm_offset_t fast_tas_base[TASK_FAST_TAS_NRAS];
vm_offset_t fast_tas_end[TASK_FAST_TAS_NRAS];
#endif
machine_task_t machine;
long_natural_t faults;
long_natural_t zero_fills;
long_natural_t reactivations;
long_natural_t pageins;
long_natural_t cow_faults;
long_natural_t messages_sent;
long_natural_t messages_received;
char name[TASK_NAME_SIZE];
};
#define task_lock(task) simple_lock(&(task)->lock)
#define task_unlock(task) simple_unlock(&(task)->lock)
#define itk_lock_init(task) simple_lock_init(&(task)->itk_lock_data)
#define itk_lock(task) simple_lock(&(task)->itk_lock_data)
#define itk_unlock(task) simple_unlock(&(task)->itk_lock_data)
extern kern_return_t task_create(
task_t parent_task,
boolean_t inherit_memory,
task_t *child_task);
extern kern_return_t task_create_kernel(
task_t parent_task,
boolean_t inherit_memory,
task_t *child_task);
extern kern_return_t task_terminate(
task_t task);
extern kern_return_t task_suspend(
task_t task);
extern kern_return_t task_resume(
task_t task);
extern kern_return_t task_threads(
task_t task,
thread_array_t *thread_list,
natural_t *count);
extern kern_return_t task_info(
task_t task,
int flavor,
task_info_t task_info_out,
natural_t *task_info_count);
extern kern_return_t task_get_special_port(
task_t task,
int which,
struct ipc_port **portp);
extern kern_return_t task_set_special_port(
task_t task,
int which,
struct ipc_port *port);
extern kern_return_t task_assign(
task_t task,
processor_set_t new_pset,
boolean_t assign_threads);
extern kern_return_t task_assign_default(
task_t task,
boolean_t assign_threads);
extern kern_return_t task_set_name(
task_t task,
const_kernel_debug_name_t name);
extern void consider_task_collect(void);
extern void task_init(void);
extern void task_reference(task_t);
extern void task_deallocate(task_t);
extern void task_hold_locked(task_t);
extern kern_return_t task_hold(task_t);
extern kern_return_t task_dowait(task_t, boolean_t);
extern kern_return_t task_release(task_t);
extern task_t kernel_task;
#endif