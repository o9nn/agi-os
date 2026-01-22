#ifndef	_KERN_IPC_HOST_H_
#define	_KERN_IPC_HOST_H_
#include <mach/port.h>
#include <kern/processor.h>
extern void ipc_host_init(void);
extern void ipc_processor_init(processor_t);
extern void ipc_pset_init(processor_set_t);
extern void ipc_pset_enable(processor_set_t);
extern void ipc_pset_disable(processor_set_t);
extern void ipc_pset_terminate(processor_set_t);
extern struct host *
convert_port_to_host(struct ipc_port *);
extern struct ipc_port *
convert_host_to_port(struct host *);
extern struct host *
convert_port_to_host_priv(struct ipc_port *);
extern processor_t
convert_port_to_processor(struct ipc_port *);
extern struct ipc_port *
convert_processor_to_port(processor_t);
extern processor_set_t
convert_port_to_pset(struct ipc_port *);
extern struct ipc_port *
convert_pset_to_port(processor_set_t);
extern processor_set_t
convert_port_to_pset_name(struct ipc_port *);
extern struct ipc_port *
convert_pset_name_to_port(processor_set_t);
#endif