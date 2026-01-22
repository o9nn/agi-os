#ifndef	_DEVICE_DEV_HDR_H_
#define	_DEVICE_DEV_HDR_H_
#include <ipc/ipc_types.h>
#include <mach/port.h>
#include <kern/lock.h>
#include <kern/queue.h>
typedef struct dev_ops *dev_ops_t;
struct device
{
struct device_emulation_ops *emul_ops;
void *emul_data;
};
typedef struct device *device_t;
#define DEVICE_NULL	((device_t) 0)
struct mach_device {
decl_simple_lock_data(,ref_lock)
int		ref_count;
decl_simple_lock_data(, lock)
short		state;
#define	DEV_STATE_INIT		0
#define	DEV_STATE_OPENING	1
#define	DEV_STATE_OPEN		2
#define	DEV_STATE_CLOSING	3
short		flag;
#define	D_EXCL_OPEN		0x0001
short		open_count;
short		io_in_progress;
boolean_t	io_wait;
struct ipc_port *port;
queue_chain_t	number_chain;
int		dev_number;
int		bsize;
struct dev_ops	*dev_ops;
struct device	dev;
};
typedef	struct mach_device *mach_device_t;
#define	MACH_DEVICE_NULL ((mach_device_t)0)
mach_device_t	device_lookup(const char *);
void		mach_device_reference(mach_device_t);
void		mach_device_deallocate(mach_device_t);
device_t	dev_port_lookup(ipc_port_t);
void		dev_port_enter(mach_device_t);
void		dev_port_remove(mach_device_t);
typedef boolean_t (*dev_map_fn)(mach_device_t, mach_port_t);
boolean_t	dev_map(dev_map_fn, mach_port_t);
#define	device_lock(device)	simple_lock(&(device)->lock)
#define	device_unlock(device)	simple_unlock(&(device)->lock)
extern boolean_t dev_name_lookup(
const char *      	name,
dev_ops_t   	*ops,
int     		*unit);
extern void dev_set_indirection(
const char	*name,
dev_ops_t   ops,
int     	unit);
extern boolean_t __attribute__ ((pure))
name_equal(
const char  *src,
int         len,
const char  *target);
#endif