#ifndef	_IPC_IPC_ENTRY_H_
#define _IPC_IPC_ENTRY_H_
#include <mach/mach_types.h>
#include <mach/port.h>
#include <mach/kern_return.h>
#include <kern/slab.h>
#include <ipc/port.h>
#include <ipc/ipc_table.h>
#include <ipc/ipc_types.h>
typedef unsigned int ipc_entry_bits_t;
typedef ipc_table_elems_t ipc_entry_num_t;
typedef struct ipc_entry {
mach_port_name_t ie_name;
ipc_entry_bits_t ie_bits;
struct ipc_object *ie_object;
union {
struct ipc_entry *next_free;
unsigned int request;
} index;
} *ipc_entry_t;
#define	IE_NULL		((ipc_entry_t) 0)
#define	ie_request	index.request
#define	ie_next_free	index.next_free
#define	IE_BITS_UREFS_MASK	0x0000ffff
#define	IE_BITS_UREFS(bits)	((bits) & IE_BITS_UREFS_MASK)
#define	IE_BITS_TYPE_MASK	0x001f0000
#define	IE_BITS_TYPE(bits)	((bits) & IE_BITS_TYPE_MASK)
#define	IE_BITS_MAREQUEST	0x00200000
#define	IE_BITS_RIGHT_MASK	0x003fffff
#if PORT_GENERATIONS
#error "not supported"
#define	IE_BITS_GEN_MASK	0xff000000U
#define	IE_BITS_GEN(bits)	((bits) & IE_BITS_GEN_MASK)
#define	IE_BITS_GEN_ONE		0x01000000
#else
#define	IE_BITS_GEN_MASK	0
#define	IE_BITS_GEN(bits)	0
#define	IE_BITS_GEN_ONE		0
#endif
extern struct kmem_cache ipc_entry_cache;
#define ie_alloc()	((ipc_entry_t) kmem_cache_alloc(&ipc_entry_cache))
#define	ie_free(e)	kmem_cache_free(&ipc_entry_cache, (vm_offset_t) (e))
extern kern_return_t
ipc_entry_alloc(ipc_space_t space, mach_port_name_t *namep, ipc_entry_t *entryp);
extern kern_return_t
ipc_entry_alloc_name(ipc_space_t space, mach_port_name_t name, ipc_entry_t *entryp);
ipc_entry_t
db_ipc_object_by_name(
task_t        	  task,
mach_port_name_t   name);
#endif