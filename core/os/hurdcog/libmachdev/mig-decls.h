#ifndef __LIBMACHDEV_MIG_DECLS_H__
#define __LIBMACHDEV_MIG_DECLS_H__
#include <hurd/ports.h>
#include "machdev-dev_hdr.h"
#include "mach_device.h"
extern struct port_bucket *machdev_device_bucket;
extern struct port_class *machdev_device_class;
static inline struct mach_device * __attribute__ ((unused))
begin_using_device_port (mach_port_t port)
{
return ports_lookup_port (machdev_device_bucket, port, machdev_device_class);
}
static inline struct mach_device * __attribute__ ((unused))
begin_using_device_payload (uintptr_t payload)
{
return ports_lookup_payload (machdev_device_bucket, payload, machdev_device_class);
}
static inline void __attribute__ ((unused))
end_using_device (struct mach_device *p)
{
if (p)
ports_port_deref (p);
}
#endif