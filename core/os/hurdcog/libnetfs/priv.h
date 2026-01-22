#ifndef _LIBNETFS_PRIV_H
#define _LIBNETFS_PRIV_H
#include <hurd/hurd_types.h>
#include "netfs.h"
extern volatile struct mapped_time_value *netfs_mtime;
static inline struct protid * __attribute__ ((unused))
begin_using_protid_port (file_t port)
{
return ports_lookup_port (netfs_port_bucket, port, netfs_protid_class);
}
static inline struct protid * __attribute__ ((unused))
begin_using_protid_payload (uintptr_t payload)
{
return ports_lookup_payload (netfs_port_bucket, payload, netfs_protid_class);
}
static inline void __attribute__ ((unused))
end_using_protid_port (struct protid *cred)
{
if (cred)
ports_port_deref (cred);
}
static inline struct netfs_control * __attribute__ ((unused))
begin_using_control_port (fsys_t port)
{
return ports_lookup_port (netfs_port_bucket, port, netfs_control_class);
}
static inline struct netfs_control * __attribute__ ((unused))
begin_using_control_payload (uintptr_t payload)
{
return ports_lookup_payload (netfs_port_bucket, payload, netfs_control_class);
}
static inline void __attribute__ ((unused))
end_using_control_port (struct netfs_control *cred)
{
if (cred)
ports_port_deref (cred);
}
#endif