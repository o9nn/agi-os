#ifndef _DEVICE_CONF_H_
#define _DEVICE_CONF_H_
#include <mach/machine/vm_types.h>
#include <sys/types.h>
#include <mach/port.h>
#include <mach/vm_prot.h>
#include <device/device_types.h>
#include <device/net_status.h>
struct io_req;
typedef struct io_req *io_req_t;
typedef int io_return_t;
struct dev_ops {
char * d_name;
int (*d_open)(dev_t, int, io_req_t);
void (*d_close)(dev_t, int);
int (*d_read)(dev_t, io_req_t);
int (*d_write)(dev_t, io_req_t);
int (*d_getstat)(dev_t, dev_flavor_t, dev_status_t, mach_msg_type_number_t *);
int (*d_setstat)(dev_t, dev_flavor_t, dev_status_t, mach_msg_type_number_t);
vm_offset_t (*d_mmap)(dev_t, vm_offset_t, vm_prot_t);
int (*d_async_in)(dev_t, const ipc_port_t, int, filter_t*, unsigned int);
int (*d_reset)(dev_t);
int (*d_port_death)(dev_t, mach_port_t);
int d_subdev;
int (*d_dev_info)(dev_t, int, int*);
};
typedef struct dev_ops *dev_ops_t;
extern int nulldev_reset(dev_t dev);
extern int nulldev_open(dev_t dev, int flag, io_req_t ior);
extern void nulldev_close(dev_t dev, int flags);
extern int nulldev_read(dev_t dev, io_req_t ior);
extern int nulldev_write(dev_t dev, io_req_t ior);
extern io_return_t nulldev_getstat(dev_t dev, dev_flavor_t flavor, dev_status_t data, mach_msg_type_number_t *count);
extern io_return_t nulldev_setstat(dev_t dev, dev_flavor_t flavor, dev_status_t data, mach_msg_type_number_t count);
extern io_return_t nulldev_portdeath(dev_t dev, mach_port_t port);
extern int nodev_async_in(dev_t, const ipc_port_t, int, filter_t*, unsigned int);
extern int nodev_info(dev_t, int, int*);
extern vm_offset_t nomap(dev_t dev, vm_offset_t off, int prot);
#define D_INFO_BLOCK_SIZE 1
extern struct dev_ops dev_name_list[];
extern int dev_name_count;
#define dev_search(dp) \
for (dp = dev_name_list; \
dp < &dev_name_list[dev_name_count]; \
dp++)
struct dev_indirect {
char * d_name;
dev_ops_t d_ops;
int d_unit;
};
typedef struct dev_indirect *dev_indirect_t;
extern struct dev_indirect dev_indirect_list[];
extern int dev_indirect_count;
#define dev_indirect_search(di) \
for (di = dev_indirect_list; \
di < &dev_indirect_list[dev_indirect_count]; \
di++)
#endif