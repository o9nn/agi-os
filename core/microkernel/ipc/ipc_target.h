#ifndef	_IPC_IPC_RECEIVER_H_
#define _IPC_IPC_RECEIVER_H_
#include "ipc_mqueue.h"
#include "ipc_object.h"
typedef struct ipc_target {
struct ipc_object ipt_object;
mach_port_name_t ipt_name;
struct ipc_mqueue ipt_messages;
#ifdef MIGRATING_THREADS
int ipt_type;
rpc_info_t ipt_rpcinfo;
struct Act *ipt_acts;
int ipt_waiting;
#endif
} *ipc_target_t;
#define IPT_TYPE_MESSAGE_RPC	1
#define IPT_TYPE_MIGRATE_RPC	2
void ipc_target_init(struct ipc_target *ipt, mach_port_name_t name);
void ipc_target_terminate(struct ipc_target *ipt);
#define ipt_lock(ipt)		io_lock(&(ipt)->ipt_object)
#define ipt_unlock(ipt)		io_unlock(&(ipt)->ipt_object)
#define ipt_reference(ipt)	io_reference(&(ipt)->ipt_object)
#define ipt_release(ipt)	io_release(&(ipt)->ipt_object)
#define ipt_check_unlock(ipt)	io_check_unlock(&(ipt)->ipt_object)
#endif