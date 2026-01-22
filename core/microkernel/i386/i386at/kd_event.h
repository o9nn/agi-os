#ifndef _KD_EVENT_H_
#define _KD_EVENT_H_
#include <sys/types.h>
#include <device/io_req.h>
#include <i386at/kd.h>
extern void X_kdb_enter (void);
extern void X_kdb_exit (void);
extern int kbdopen(dev_t dev, int flags, io_req_t ior);
extern void kbdclose(dev_t dev, int flags);
extern int kbdread(dev_t dev, io_req_t ior);
extern io_return_t kbdgetstat(
dev_t dev,
dev_flavor_t flavor,
dev_status_t data,
mach_msg_type_number_t *count);
extern io_return_t kbdsetstat(
dev_t dev,
dev_flavor_t flavor,
dev_status_t data,
mach_msg_type_number_t count);
extern void kd_enqsc(Scancode sc);
void kbd_enqueue(kd_event *ev);
io_return_t X_kdb_enter_init(u_int *data, u_int count);
io_return_t X_kdb_exit_init(u_int *data, u_int count);
boolean_t kbd_read_done(io_req_t ior);
#endif