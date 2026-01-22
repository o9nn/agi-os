#ifndef __MACHDEV_H__
#define __MACHDEV_H__
#include <mach.h>
#include "machdev-device_emul.h"
#include "machdev-dev_hdr.h"
extern struct port_bucket *machdev_device_bucket;
void machdev_register (struct machdev_device_emulation_ops *ops);
void machdev_device_init(void);
void machdev_device_sync(void);
void * machdev_server(void *);
error_t machdev_create_device_port (size_t size, void *result);
int machdev_trivfs_init(int argc, char **argv, mach_port_t bootstrap_resume_task, const char *name, const char *path, mach_port_t *bootstrap);
int machdev_demuxer(mach_msg_header_t *inp, mach_msg_header_t *outp);
void machdev_trivfs_server_startup(mach_port_t bootstrap);
void * machdev_trivfs_server_loop(void *);
void * machdev_trivfs_server_loop_forever(void *);
boolean_t machdev_is_master_device (mach_port_t port);
#endif