#ifndef __ETH_MULTIPLEXER_MIG_DECLS_H__
#define __ETH_MULTIPLEXER_MIG_DECLS_H__
#include <hurd/ports.h>
typedef struct vether_device *vether_device_t;
extern struct port_bucket *port_bucket;
extern struct port_class *vdev_portclass;
static inline struct vether_device * __attribute__ ((unused))
begin_using_device_port (mach_port_t port)
{
return ports_lookup_port (port_bucket, port, vdev_portclass);
}
static inline struct vether_device * __attribute__ ((unused))
begin_using_device_payload (uintptr_t payload)
{
return ports_lookup_payload (port_bucket, payload, vdev_portclass);
}
static inline void __attribute__ ((unused))
end_using_device (struct vether_device *p)
{
if (p)
ports_port_deref (p);
}
#endif