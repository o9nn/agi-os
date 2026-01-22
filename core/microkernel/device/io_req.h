#ifndef	_IO_REQ_
#define	_IO_REQ_
#include <mach/boolean.h>
#include <mach/port.h>
#include <mach/message.h>
#include <mach/vm_param.h>
#include <kern/slab.h>
#include <kern/kalloc.h>
#include <kern/lock.h>
#include <vm/vm_map.h>
#include <vm/vm_page.h>
#include <device/device_types.h>
#include <device/dev_hdr.h>
#include <kern/macros.h>
typedef struct io_req *io_req_t;
struct io_req {
struct io_req *	io_next;
struct io_req *	io_prev;
mach_device_t	io_device;
char *		io_dev_ptr;
int		io_unit;
int		io_op;
dev_mode_t	io_mode;
recnum_t	io_recnum;
union io_un {
io_buf_ptr_t    data;
} io_un;
#define	io_data		io_un.data
long		io_count;
vm_size_t	io_alloc_size;
long		io_residual;
io_return_t	io_error;
boolean_t	(*io_done)(io_req_t);
struct ipc_port	*io_reply_port;
mach_msg_type_name_t io_reply_port_type;
struct io_req *	io_link;
struct io_req *	io_rlink;
vm_map_copy_t	io_copy;
long		io_total;
decl_simple_lock_data(,io_req_lock)
long            io_physrec;
long            io_rectotal;
};
#define ior_lock(ior)	simple_lock(&(ior)->io_req_lock)
#define ior_unlock(ior)	simple_unlock(&(ior)->io_req_lock)
#define	IO_WRITE	0x00000000
#define	IO_READ		0x00000001
#define	IO_OPEN		0x00000002
#define	IO_DONE		0x00000100
#define	IO_ERROR	0x00000200
#define	IO_BUSY		0x00000400
#define	IO_WANTED	0x00000800
#define	IO_BAD		0x00001000
#define	IO_CALL		0x00002000
#define IO_INBAND	0x00004000
#define IO_INTERNAL	0x00008000
#define	IO_LOANED	0x00010000
#define	IO_SPARE_START	0x00020000
void	iodone(io_req_t);
#define	io_req_alloc(ior,size)					\
MACRO_BEGIN						\
(ior) = (io_req_t)kalloc(sizeof(struct io_req));	\
simple_lock_init(&(ior)->io_req_lock);			\
MACRO_END
#define	io_req_free(ior)					\
(kfree((vm_offset_t)(ior), sizeof(struct io_req)))
extern struct kmem_cache io_inband_cache;
#endif