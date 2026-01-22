#ifndef _IPC_IPC_TABLE_H_
#define _IPC_IPC_TABLE_H_
#include <mach/boolean.h>
#include <mach/vm_param.h>
typedef unsigned int ipc_table_index_t;
typedef unsigned int ipc_table_elems_t;
typedef struct ipc_table_size {
ipc_table_elems_t its_size;
} *ipc_table_size_t;
#define ITS_NULL ((ipc_table_size_t) 0)
extern ipc_table_size_t ipc_table_dnrequests;
extern void
ipc_table_init(void);
extern vm_offset_t ipc_table_alloc(
vm_size_t size);
extern void ipc_table_free(
vm_size_t size,
vm_offset_t table);
void ipc_table_fill(
ipc_table_size_t its,
unsigned int num,
unsigned int min,
vm_size_t elemsize);
#define it_dnrequests_alloc(its) \
((ipc_port_request_t) \
ipc_table_alloc((its)->its_size * \
sizeof(struct ipc_port_request)))
#define it_dnrequests_free(its, table) \
ipc_table_free((its)->its_size * \
sizeof(struct ipc_port_request), \
(vm_offset_t)(table))
#endif