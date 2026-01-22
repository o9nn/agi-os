#ifndef _I386_IO_PERM_H_
#define _I386_IO_PERM_H_
#include <device/dev_hdr.h>
#include <ipc/ipc_types.h>
#define	IOPB_MAX	0xffff
#define	IOPB_BYTES	(((IOPB_MAX + 1) + 7) / 8)
#define IOPB_INVAL	0x2fff
typedef unsigned short io_port_t;
struct io_perm
{
struct device device;
ipc_port_t port;
io_port_t from, to;
};
typedef struct io_perm *io_perm_t;
#define IO_PERM_NULL ((io_perm_t) 0)
extern io_perm_t convert_port_to_io_perm (ipc_port_t);
extern ipc_port_t convert_io_perm_to_port (io_perm_t);
extern void io_perm_deallocate (io_perm_t);
#endif