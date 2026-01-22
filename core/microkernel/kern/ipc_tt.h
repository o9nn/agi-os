#ifndef	_KERN_IPC_TT_H_
#define _KERN_IPC_TT_H_
#include <mach/boolean.h>
#include <mach/mach_types.h>
#include <mach/port.h>
extern void ipc_task_init(task_t, task_t);
extern void ipc_task_enable(task_t);
extern void ipc_task_disable(task_t);
extern void ipc_task_terminate(task_t);
extern void ipc_thread_init(thread_t);
extern void ipc_thread_enable(thread_t);
extern void ipc_thread_disable(thread_t);
extern void ipc_thread_terminate(thread_t);
extern struct ipc_port *
retrieve_task_self(task_t);
extern struct ipc_port *
retrieve_task_self_fast(task_t);
extern struct ipc_port *
retrieve_thread_self(thread_t);
extern struct ipc_port *
retrieve_thread_self_fast(thread_t);
extern struct ipc_port *
retrieve_task_exception(task_t);
extern struct ipc_port *
retrieve_thread_exception(thread_t);
extern struct task *
convert_port_to_task(struct ipc_port *);
extern struct ipc_port *
convert_task_to_port(task_t);
extern void
task_deallocate(task_t);
extern struct thread *
convert_port_to_thread(struct ipc_port *);
extern struct ipc_port *
convert_thread_to_port(thread_t);
extern void
thread_deallocate(thread_t);
extern struct vm_map *
convert_port_to_map(struct ipc_port *);
extern struct ipc_space *
convert_port_to_space(struct ipc_port *);
extern void
space_deallocate(ipc_space_t);
mach_port_name_t
mach_reply_port (void);
#endif