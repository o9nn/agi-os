#ifndef __TRIVFS_MIG_DECLS_H__
#define __TRIVFS_MIG_DECLS_H__
#include "priv.h"
extern struct port_class **trivfs_dynamic_protid_port_classes;
extern size_t trivfs_num_dynamic_protid_port_classes;
extern struct port_class **trivfs_dynamic_control_port_classes;
extern size_t trivfs_num_dynamic_control_port_classes;
extern struct port_bucket **trivfs_dynamic_port_buckets;
extern size_t trivfs_num_dynamic_port_buckets;
static inline struct trivfs_protid * __attribute__ ((unused))
trivfs_begin_using_protid (mach_port_t port)
{
struct port_info *pi = ports_lookup_port (0, port, 0);
if (pi)
{
size_t i;
for (i = 0; i < trivfs_num_dynamic_protid_port_classes; i++)
if (pi->class == trivfs_dynamic_protid_port_classes[i])
return (struct trivfs_protid *) pi;
ports_port_deref (pi);
}
return NULL;
}
static inline struct trivfs_protid * __attribute__ ((unused))
trivfs_begin_using_protid_payload (uintptr_t payload)
{
struct port_info *pi = ports_lookup_payload (NULL, payload, NULL);
if (pi)
{
size_t i;
for (i = 0; i < trivfs_num_dynamic_protid_port_classes; i++)
if (pi->class == trivfs_dynamic_protid_port_classes[i])
return (struct trivfs_protid *) pi;
ports_port_deref (pi);
}
return NULL;
}
static inline void __attribute__ ((unused))
trivfs_end_using_protid (struct trivfs_protid *cred)
{
if (cred)
ports_port_deref (cred);
}
static inline mach_port_t __attribute__ ((unused))
trivfs_convert_to_port(struct trivfs_protid *protid)
{
return protid->pi.port_right;
}
static inline struct trivfs_control * __attribute__ ((unused))
trivfs_begin_using_control (mach_port_t port)
{
struct port_info *pi = ports_lookup_port (0, port, 0);
if (pi)
{
size_t i;
for (i = 0; i < trivfs_num_dynamic_control_port_classes; i++)
if (pi->class == trivfs_dynamic_control_port_classes[i])
return (struct trivfs_control *) pi;
ports_port_deref (pi);
}
return NULL;
}
static inline struct trivfs_control * __attribute__ ((unused))
trivfs_begin_using_control_payload (uintptr_t payload)
{
struct port_info *pi = ports_lookup_payload (NULL, payload, NULL);
if (pi)
{
size_t i;
for (i = 0; i < trivfs_num_dynamic_control_port_classes; i++)
if (pi->class == trivfs_dynamic_control_port_classes[i])
return (struct trivfs_control *) pi;
ports_port_deref (pi);
}
return NULL;
}
static inline void __attribute__ ((unused))
trivfs_end_using_control (struct trivfs_control *cred)
{
if (cred)
ports_port_deref (cred);
}
#endif