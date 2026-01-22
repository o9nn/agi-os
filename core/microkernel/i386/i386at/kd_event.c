#include <mach/boolean.h>
#include <sys/types.h>
#include <kern/printf.h>
#include <string.h>
#include <device/ds_routines.h>
#include <device/device_types.h>
#include <device/io_req.h>
#include <i386/spl.h>
#include <i386/pio.h>
#include <i386at/kd.h>
#include <i386at/kd_queue.h>
#ifdef APIC
# include <i386/apic.h>
#else
# include <i386/pic.h>
#endif
#include "kd_event.h"
kd_event_queue kbd_queue;
queue_head_t	kbd_read_queue = { &kbd_read_queue, &kbd_read_queue };
static boolean_t initialized = FALSE;
static void
kbdinit(void)
{
spl_t s = SPLKD();
if (!initialized) {
kdq_reset(&kbd_queue);
initialized = TRUE;
}
splx(s);
}
int
kbdopen(dev_t dev, int flags, io_req_t ior)
{
spl_t o_pri = spltty();
kdinit();
splx(o_pri);
kbdinit();
return(0);
}
void
kbdclose(
dev_t 	dev,
int 	flags)
{
spl_t s = SPLKD();
kb_mode = KB_ASCII;
kdq_reset(&kbd_queue);
splx(s);
}
io_return_t kbdgetstat(
dev_t		dev,
dev_flavor_t	flavor,
dev_status_t	data,
mach_msg_type_number_t	*count)
{
switch (flavor) {
case KDGKBDTYPE:
*data = KB_VANILLAKB;
*count = 1;
break;
case DEV_GET_SIZE:
data[DEV_GET_SIZE_DEVICE_SIZE] = 0;
data[DEV_GET_SIZE_RECORD_SIZE] = sizeof(kd_event);
*count = DEV_GET_SIZE_COUNT;
break;
default:
return (D_INVALID_OPERATION);
}
return (D_SUCCESS);
}
io_return_t kbdsetstat(
dev_t		dev,
dev_flavor_t	flavor,
dev_status_t	data,
mach_msg_type_number_t	count)
{
switch (flavor) {
case KDSKBDMODE:
kb_mode = *data;
break;
case KDSETLEDS:
if (count != 1)
return (D_INVALID_OPERATION);
kd_setleds1 (*data);
break;
case K_X_KDB_ENTER:
return X_kdb_enter_init((unsigned int *)data, count);
case K_X_KDB_EXIT:
return X_kdb_exit_init((unsigned int *)data, count);
default:
return (D_INVALID_OPERATION);
}
return (D_SUCCESS);
}
int
kbdread(
dev_t		dev,
io_req_t	ior)
{
int		err, count;
spl_t		s;
if (ior->io_count % sizeof(kd_event) != 0)
return D_INVALID_SIZE;
err = device_read_alloc(ior, (vm_size_t)ior->io_count);
if (err != KERN_SUCCESS)
return (err);
s = SPLKD();
if (kdq_empty(&kbd_queue)) {
if (ior->io_mode & D_NOWAIT) {
splx(s);
return (D_WOULD_BLOCK);
}
ior->io_done = kbd_read_done;
enqueue_tail(&kbd_read_queue, (queue_entry_t) ior);
splx(s);
return (D_IO_QUEUED);
}
count = 0;
while (!kdq_empty(&kbd_queue) && count < ior->io_count) {
kd_event *ev;
ev = kdq_get(&kbd_queue);
memcpy(&ior->io_data[count], ev, sizeof(kd_event));
count += sizeof(kd_event);
}
splx(s);
ior->io_residual = ior->io_count - count;
return (D_SUCCESS);
}
boolean_t kbd_read_done(io_req_t ior)
{
int		count;
spl_t		s;
s = SPLKD();
if (kdq_empty(&kbd_queue)) {
ior->io_done = kbd_read_done;
enqueue_tail(&kbd_read_queue, (queue_entry_t)ior);
splx(s);
return (FALSE);
}
count = 0;
while (!kdq_empty(&kbd_queue) && count < ior->io_count) {
kd_event *ev;
ev = kdq_get(&kbd_queue);
memcpy(&ior->io_data[count], ev, sizeof(kd_event));
count += sizeof(kd_event);
}
splx(s);
ior->io_residual = ior->io_count - count;
ds_read_done(ior);
return (TRUE);
}
void
kd_enqsc(Scancode sc)
{
kd_event ev;
ev.type = KEYBD_EVENT;
ev.unused_time.seconds = 0;
ev.unused_time.microseconds = 0;
ev.value.sc = sc;
kbd_enqueue(&ev);
}
void
kbd_enqueue(kd_event *ev)
{
if (kdq_full(&kbd_queue))
printf_once("kbd: queue full\n");
else
kdq_put(&kbd_queue, ev);
{
io_req_t	ior;
while ((ior = (io_req_t)dequeue_head(&kbd_read_queue)) != 0)
iodone(ior);
}
}
u_int X_kdb_enter_str[512], X_kdb_exit_str[512];
int   X_kdb_enter_len = 0,  X_kdb_exit_len = 0;
static void
kdb_in_out(const u_int *p)
{
int t = p[0];
switch (t & K_X_TYPE) {
case K_X_IN|K_X_BYTE:
inb(t & K_X_PORT);
break;
case K_X_IN|K_X_WORD:
inw(t & K_X_PORT);
break;
case K_X_IN|K_X_LONG:
inl(t & K_X_PORT);
break;
case K_X_OUT|K_X_BYTE:
outb(t & K_X_PORT, p[1]);
break;
case K_X_OUT|K_X_WORD:
outw(t & K_X_PORT, p[1]);
break;
case K_X_OUT|K_X_LONG:
outl(t & K_X_PORT, p[1]);
break;
}
}
void
X_kdb_enter(void)
{
u_int *u_ip, *endp;
for (u_ip = X_kdb_enter_str, endp = &X_kdb_enter_str[X_kdb_enter_len];
u_ip < endp;
u_ip += 2)
kdb_in_out(u_ip);
}
void
X_kdb_exit(void)
{
u_int *u_ip, *endp;
for (u_ip = X_kdb_exit_str, endp = &X_kdb_exit_str[X_kdb_exit_len];
u_ip < endp;
u_ip += 2)
kdb_in_out(u_ip);
}
io_return_t
X_kdb_enter_init(
u_int *data,
u_int count)
{
if (count * sizeof X_kdb_enter_str[0] > sizeof X_kdb_enter_str)
return D_INVALID_OPERATION;
memcpy(X_kdb_enter_str, data, count * sizeof X_kdb_enter_str[0]);
X_kdb_enter_len = count;
return D_SUCCESS;
}
io_return_t
X_kdb_exit_init(
u_int *data,
u_int count)
{
if (count * sizeof X_kdb_exit_str[0] > sizeof X_kdb_exit_str)
return D_INVALID_OPERATION;
memcpy(X_kdb_exit_str, data, count * sizeof X_kdb_exit_str[0]);
X_kdb_exit_len = count;
return D_SUCCESS;
}