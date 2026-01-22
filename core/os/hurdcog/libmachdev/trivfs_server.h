#ifndef _MACHDEV_TRIVFS_SERVER_H
#define _MACHDEV_TRIVFS_SERVER_H
#include <hurd/ports.h>
#include <hurd/trivfs.h>
#include <hurd.h>
extern struct port_bucket *port_bucket;
extern struct port_class *trivfs_protid_class;
extern struct port_class *trivfs_cntl_class;
extern struct port_class *machdev_shutdown_notify_class;
#endif