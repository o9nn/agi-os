#ifndef _KERN_IPC_KOBJECT_H_
#define _KERN_IPC_KOBJECT_H_
#include <mach/machine/vm_types.h>
#include <ipc/ipc_types.h>
#include <ipc/ipc_kmsg.h>
typedef vm_offset_t ipc_kobject_t;
#define IKO_NULL ((ipc_kobject_t) 0)
typedef unsigned int ipc_kobject_type_t;
#define IKOT_NONE 0
#define IKOT_THREAD 1
#define IKOT_TASK 2
#define IKOT_HOST 3
#define IKOT_HOST_PRIV 4
#define IKOT_PROCESSOR 5
#define IKOT_PSET 6
#define IKOT_PSET_NAME 7
#define IKOT_PAGER 8
#define IKOT_PAGING_REQUEST 9
#define IKOT_DEVICE 10
#define IKOT_XMM_OBJECT 11
#define IKOT_XMM_PAGER 12
#define IKOT_XMM_KERNEL 13
#define IKOT_XMM_REPLY 14
#define IKOT_PAGER_TERMINATING 15
#define IKOT_PAGING_NAME 16
#define IKOT_HOST_SECURITY 17
#define IKOT_LEDGER 18
#define IKOT_MASTER_DEVICE 19
#define IKOT_ACT 20
#define IKOT_SUBSYSTEM 21
#define IKOT_IO_DONE_QUEUE 22
#define IKOT_SEMAPHORE 23
#define IKOT_LOCK_SET 24
#define IKOT_CLOCK 25
#define IKOT_CLOCK_CTRL 26
#define IKOT_PAGER_PROXY 27
#define IKOT_UNKNOWN 28
#define IKOT_MAX_TYPE 29
#define is_ipc_kobject(ikot) (ikot != IKOT_NONE)
#define ipc_kobject_vm_page_list(ikot) \
((ikot == IKOT_PAGING_REQUEST) || (ikot == IKOT_DEVICE))
#define ipc_kobject_vm_page_steal(ikot) (ikot == IKOT_PAGING_REQUEST)
extern ipc_kmsg_t ipc_kobject_server(
ipc_kmsg_t request);
extern void ipc_kobject_set(
ipc_port_t port,
ipc_kobject_t kobject,
ipc_kobject_type_t type);
extern void ipc_kobject_destroy(
ipc_port_t port);
extern boolean_t ipc_kobject_notify (
mach_msg_header_t *request_header,
mach_msg_header_t *reply_header);
#define null_conversion(port) (port)
#endif