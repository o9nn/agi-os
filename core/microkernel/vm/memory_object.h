#ifndef	_VM_MEMORY_OBJECT_H_
#define	_VM_MEMORY_OBJECT_H_
#include <mach/boolean.h>
#include <ipc/ipc_types.h>
extern ipc_port_t memory_manager_default_reference(void);
extern boolean_t memory_manager_default_port(ipc_port_t);
extern void memory_manager_default_init(void);
extern ipc_port_t memory_manager_default;
#endif