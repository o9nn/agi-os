#ifndef	_KERN_ACT_H_
#define _KERN_ACT_H_
#ifdef MIGRATING_THREADS
#ifndef __dead
#define __dead
#endif
#include <mach/vm_param.h>
#include <mach/port.h>
#include <kern/lock.h>
#include <kern/refcount.h>
#include <kern/queue.h>
struct task;
struct thread;
struct Act;
struct ReturnHandler {
struct ReturnHandler *next;
void (*handler)(struct ReturnHandler *rh, struct Act *act);
};
typedef struct ReturnHandler ReturnHandler;
struct Act {
queue_chain_t	task_links;
struct task	*task;
MachineAct	mact;
RefCount	ref_count;
decl_simple_lock_data(,lock)
struct ipc_target	*ipt;
struct Act	*ipt_next;
struct thread	*thread;
struct Act	*higher, *lower;
unsigned	alerts;
unsigned	alert_mask;
int		suspend_count;
int		active;
ReturnHandler	*handlers;
ReturnHandler	special_handler;
struct ipc_port *self;
struct ipc_port *self_port;
struct ipc_port *exception_port;
struct ipc_port *syscall_port;
};
typedef struct Act Act;
typedef struct Act *act_t;
typedef mach_port_t *act_array_t;
#define ACT_NULL ((Act*)0)
kern_return_t	act_create(struct task *task, vm_offset_t user_stack, vm_offset_t user_rbuf, vm_size_t user_rbuf_size, struct Act **new_act);
kern_return_t	act_alert_mask(struct Act *act, unsigned alert_mask);
kern_return_t	act_alert(struct Act *act, unsigned alerts);
kern_return_t	act_abort(struct Act *act);
kern_return_t	act_abort_safely(struct Act *act);
kern_return_t	act_terminate(struct Act *act);
kern_return_t	act_suspend(struct Act *act);
kern_return_t	act_resume(struct Act *act);
kern_return_t	act_get_state(struct Act *act, int flavor,
natural_t *state, natural_t *pcount);
kern_return_t	act_set_state(struct Act *act, int flavor,
natural_t *state, natural_t count);
#define		act_lock(act)		simple_lock(&(act)->lock)
#define		act_unlock(act)		simple_unlock(&(act)->lock)
#define		act_reference(act)	refcount_take(&(act)->ref_count)
void		act_deallocate(struct Act *act);
void		act_init(void);
kern_return_t	act_terminate_task_locked(struct Act *act);
extern Act	null_act;
void		act_execute_returnhandlers(void);
kern_return_t	act_machine_create(struct task *task, Act *inc, vm_offset_t user_stack, vm_offset_t user_rbuf, vm_size_t user_rbuf_size);
void		act_machine_destroy(Act *inc);
kern_return_t	act_machine_set_state(Act *inc, int flavor, int *tstate, unsigned count);
kern_return_t	act_machine_get_state(Act *inc, int flavor, int *tstate, unsigned *count);
#endif
#endif