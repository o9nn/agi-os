#ifndef	_IPC_IPC_NOTIFY_H_
#define _IPC_IPC_NOTIFY_H_
extern void
ipc_notify_init(void);
extern void
ipc_notify_port_deleted(ipc_port_t, mach_port_name_t);
extern void
ipc_notify_msg_accepted(ipc_port_t, mach_port_name_t);
extern void
ipc_notify_port_destroyed(ipc_port_t, ipc_port_t);
extern void
ipc_notify_no_senders(ipc_port_t, mach_port_mscount_t);
extern void
ipc_notify_send_once(ipc_port_t);
extern void
ipc_notify_dead_name(ipc_port_t, mach_port_name_t);
#endif