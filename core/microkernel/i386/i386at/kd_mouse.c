#include <mach/boolean.h>
#include <sys/types.h>
#include <kern/printf.h>
#include <string.h>
#include <device/ds_routines.h>
#include <device/device_types.h>
#include <device/io_req.h>
#include <device/subrs.h>
#include <i386/ipl.h>
#include <i386/irq.h>
#include <i386/pio.h>
#include <chips/busses.h>
#include <i386at/com.h>
#include <i386at/kd.h>
#include <i386at/kd_queue.h>
#include <i386at/i8250.h>
#include "kd_mouse.h"
static interrupt_handler_fn oldvect;
static int oldunit;
extern struct bus_device *cominfo[];
kd_event_queue mouse_queue;
boolean_t mouse_in_use = FALSE;
queue_head_t mouse_read_queue = { &mouse_read_queue, &mouse_read_queue };
u_char lastbuttons;
#define MOUSE_UP 1
#define MOUSE_DOWN 0
#define MOUSE_ALL_UP 0x7
int mouse_baud = BCNT1200;
boolean_t mouse_char_cmd = FALSE;
boolean_t mouse_char_wanted = FALSE;
int mouse_char_index;
#define IBM_MOUSE_IRQ 12
static void
init_mouse_hw(dev_t unit, int mode)
{
unsigned short base_addr = cominfo[unit]->address;
outb(base_addr + RIE, 0);
outb(base_addr + RLC, LCDLAB);
outb(base_addr + RDLSB, mouse_baud & 0xff);
outb(base_addr + RDMSB, (mouse_baud >> 8) & 0xff);
outb(base_addr + RLC, mode);
outb(base_addr + RMC, MCDTR | MCRTS | MCOUT2);
outb(base_addr + RIE, IERD | IELS);
}
#define MOUSE_SYSTEM_MOUSE 0
#define MICROSOFT_MOUSE 1
#define IBM_MOUSE 2
#define NO_MOUSE 3
#define LOGITECH_TRACKMAN 4
#define MICROSOFT_MOUSE7 5
static int mouse_type;
static int mousebufsize;
static int mousebufindex = 0;
int track_man[10];
int
mouseopen(dev_t dev, int flags, io_req_t ior)
{
if (mouse_in_use)
return (D_ALREADY_OPEN);
mouse_in_use = TRUE;
kdq_reset(&mouse_queue);
lastbuttons = MOUSE_ALL_UP;
switch (mouse_type = ((minor(dev) & 0xf8) >> 3)) {
case MICROSOFT_MOUSE7:
mousebufsize = 3;
serial_mouse_open(dev);
init_mouse_hw(dev&7, LC7);
break;
case MICROSOFT_MOUSE:
mousebufsize = 3;
serial_mouse_open(dev);
init_mouse_hw(dev&7, LC8);
break;
case MOUSE_SYSTEM_MOUSE:
mousebufsize = 5;
serial_mouse_open(dev);
init_mouse_hw(dev&7, LC8);
break;
case LOGITECH_TRACKMAN:
mousebufsize = 3;
serial_mouse_open(dev);
init_mouse_hw(dev&7, LC7);
track_man[0] = comgetc(dev&7);
track_man[1] = comgetc(dev&7);
if (track_man[0] != 0x4d &&
track_man[1] != 0x33) {
printf("LOGITECH_TRACKMAN: NOT M3");
}
break;
case IBM_MOUSE:
mousebufsize = 3;
kd_mouse_open(dev, IBM_MOUSE_IRQ);
ibm_ps2_mouse_open(dev);
break;
case NO_MOUSE:
break;
}
mousebufindex = 0;
return(0);
}
void
serial_mouse_open(dev_t dev)
{
int unit = minor(dev) & 0x7;
int mouse_pic = cominfo[unit]->sysdep1;
spl_t s = splhi();
oldvect = ivect[mouse_pic];
ivect[mouse_pic] = mouseintr;
oldunit = iunit[mouse_pic];
iunit[mouse_pic] = unit;
splx(s);
}
int mouse_packets = 0;
void
kd_mouse_open(
dev_t dev,
int mouse_pic)
{
spl_t s = splhi();
oldvect = ivect[mouse_pic];
ivect[mouse_pic] = kdintr;
unmask_irq(mouse_pic);
splx(s);
}
void
mouseclose(
dev_t dev,
int flags)
{
switch (mouse_type) {
case MICROSOFT_MOUSE:
case MICROSOFT_MOUSE7:
case MOUSE_SYSTEM_MOUSE:
case LOGITECH_TRACKMAN:
serial_mouse_close(dev, flags);
break;
case IBM_MOUSE:
ibm_ps2_mouse_close(dev);
kd_mouse_close(dev, IBM_MOUSE_IRQ);
{int i = 20000; for (;i--;); }
kd_mouse_drain();
break;
case NO_MOUSE:
break;
}
kdq_reset(&mouse_queue);
mouse_in_use = FALSE;
}
void
serial_mouse_close(
dev_t dev,
int flags)
{
spl_t o_pri = splhi();
int unit = minor(dev) & 0x7;
int mouse_pic = cominfo[unit]->sysdep1;
unsigned short base_addr = cominfo[unit]->address;
assert(ivect[mouse_pic] == mouseintr);
outb(base_addr + RIE, 0);
outb(base_addr + RMC, 0);
ivect[mouse_pic] = oldvect;
iunit[mouse_pic] = oldunit;
(void)splx(o_pri);
}
void
kd_mouse_close(
dev_t dev,
int mouse_pic)
{
spl_t s = splhi();
mask_irq(mouse_pic);
ivect[mouse_pic] = oldvect;
splx(s);
}
io_return_t mousegetstat(
dev_t dev,
dev_flavor_t flavor,
dev_status_t data,
mach_msg_type_number_t *count)
{
switch (flavor) {
case DEV_GET_SIZE:
data[DEV_GET_SIZE_DEVICE_SIZE] = 0;
data[DEV_GET_SIZE_RECORD_SIZE] = sizeof(kd_event);
*count = DEV_GET_SIZE_COUNT;
break;
default:
return D_INVALID_OPERATION;
}
return D_SUCCESS;
}
int
mouseread(
dev_t dev,
io_req_t ior)
{
int err, count;
spl_t s;
if (ior->io_count % sizeof(kd_event) != 0)
return D_INVALID_SIZE;
err = device_read_alloc(ior, (vm_size_t)ior->io_count);
if (err != KERN_SUCCESS)
return (err);
s = SPLKD();
if (kdq_empty(&mouse_queue)) {
if (ior->io_mode & D_NOWAIT) {
splx(s);
return (D_WOULD_BLOCK);
}
ior->io_done = mouse_read_done;
enqueue_tail(&mouse_read_queue, (queue_entry_t)ior);
splx(s);
return (D_IO_QUEUED);
}
count = 0;
while (!kdq_empty(&mouse_queue) && count < ior->io_count) {
kd_event *ev;
ev = kdq_get(&mouse_queue);
memcpy(&ior->io_data[count], ev, sizeof(kd_event));
count += sizeof(kd_event);
}
splx(s);
ior->io_residual = ior->io_count - count;
return (D_SUCCESS);
}
boolean_t mouse_read_done(io_req_t ior)
{
int count;
spl_t s;
s = SPLKD();
if (kdq_empty(&mouse_queue)) {
ior->io_done = mouse_read_done;
enqueue_tail(&mouse_read_queue, (queue_entry_t)ior);
splx(s);
return (FALSE);
}
count = 0;
while (!kdq_empty(&mouse_queue) && count < ior->io_count) {
kd_event *ev;
ev = kdq_get(&mouse_queue);
memcpy(&ior->io_data[count], ev, sizeof(kd_event));
count += sizeof(kd_event);
}
splx(s);
ior->io_residual = ior->io_count - count;
ds_read_done(ior);
return (TRUE);
}
void
mouseintr(int unit)
{
unsigned short base_addr = cominfo[unit]->address;
unsigned char id, ls;
id = inb(base_addr + RID);
ls = inb(base_addr + RLS);
if (id == IDLS) {
if (ls & LSDR) {
inb(base_addr + RDAT);
}
return;
}
if (id & IDRD) {
mouse_handle_byte((u_char)(inb(base_addr + RDAT) & 0xff));
}
}
int show_mouse_byte = 0;
int lastgitech = 0x40;
int fourthgitech = 0;
int middlegitech = 0;
static u_char mousebuf[MOUSEBUFSIZE];
void
mouse_handle_byte(u_char ch)
{
if (show_mouse_byte) {
printf("%x(%c) ", ch, ch);
}
if (mouse_char_cmd) {
if (mousebufindex < mousebufsize)
mousebuf[mousebufindex++] = ch;
if (mouse_char_wanted) {
mouse_char_wanted = FALSE;
wakeup((vm_offset_t)&mousebuf);
}
return;
}
if (mousebufindex == 0) {
switch (mouse_type) {
case MICROSOFT_MOUSE7:
if ((ch & 0x40) != 0x40)
return;
break;
case MICROSOFT_MOUSE:
if ((ch & 0xc0) != 0xc0)
return;
break;
case MOUSE_SYSTEM_MOUSE:
if ((ch & 0xf8) != 0x80)
return;
break;
case LOGITECH_TRACKMAN:
if (fourthgitech == 1) {
fourthgitech = 0;
if (ch & 0xf0)
middlegitech = 0x4;
else
middlegitech = 0x0;
mouse_packet_microsoft_mouse(mousebuf);
return;
} else if ((ch & 0xc0) != 0x40)
return;
break;
case IBM_MOUSE:
break;
}
}
mousebuf[mousebufindex++] = ch;
if (mousebufindex < mousebufsize)
return;
mousebufindex = 0;
switch (mouse_type) {
case MICROSOFT_MOUSE7:
case MICROSOFT_MOUSE:
mouse_packet_microsoft_mouse(mousebuf);
break;
case MOUSE_SYSTEM_MOUSE:
mouse_packet_mouse_system_mouse(mousebuf);
break;
case LOGITECH_TRACKMAN:
if ( mousebuf[1] || mousebuf[2] ||
mousebuf[0] != lastgitech) {
mouse_packet_microsoft_mouse(mousebuf);
lastgitech = mousebuf[0] & 0xf0;
} else {
fourthgitech = 1;
}
break;
case IBM_MOUSE:
mouse_packet_ibm_ps2_mouse(mousebuf);
break;
}
}
void
mouse_packet_mouse_system_mouse(u_char mousebuf[MOUSEBUFSIZE])
{
u_char buttons, buttonchanges;
struct mouse_motion moved;
buttons = mousebuf[0] & 0x7;
buttonchanges = buttons ^ lastbuttons;
moved.mm_deltaX = (char)mousebuf[1] + (char)mousebuf[3];
moved.mm_deltaY = (char)mousebuf[2] + (char)mousebuf[4];
if (moved.mm_deltaX != 0 || moved.mm_deltaY != 0)
mouse_moved(moved);
if (buttonchanges != 0) {
lastbuttons = buttons;
if (buttonchanges & 1)
mouse_button(MOUSE_RIGHT, buttons & 1);
if (buttonchanges & 2)
mouse_button(MOUSE_MIDDLE, (buttons & 2) >> 1);
if (buttonchanges & 4)
mouse_button(MOUSE_LEFT, (buttons & 4) >> 2);
}
}
void
mouse_packet_microsoft_mouse(u_char mousebuf[MOUSEBUFSIZE])
{
u_char buttons, buttonchanges;
struct mouse_motion moved;
buttons = ((mousebuf[0] & 0x30) >> 4);
buttons |= middlegitech;
#ifdef gross_hack
if (buttons == 0x03)
buttons = 0x04;
#endif
buttons = (~buttons) & 0x07;
buttonchanges = buttons ^ lastbuttons;
moved.mm_deltaX = ((mousebuf[0] & 0x03) << 6) | (mousebuf[1] & 0x3F);
moved.mm_deltaY = ((mousebuf[0] & 0x0c) << 4) | (mousebuf[2] & 0x3F);
if (moved.mm_deltaX & 0x80)
moved.mm_deltaX = moved.mm_deltaX - 0x100;
if (moved.mm_deltaY & 0x80)
moved.mm_deltaY = moved.mm_deltaY - 0x100;
moved.mm_deltaY = -moved.mm_deltaY;
if (moved.mm_deltaX != 0 || moved.mm_deltaY != 0)
mouse_moved(moved);
if (buttonchanges != 0) {
lastbuttons = buttons;
if (buttonchanges & 1)
mouse_button(MOUSE_RIGHT, (buttons & 1) ?
MOUSE_UP : MOUSE_DOWN);
if (buttonchanges & 2)
mouse_button(MOUSE_LEFT, (buttons & 2) ?
MOUSE_UP : MOUSE_DOWN);
if (buttonchanges & 4)
mouse_button(MOUSE_MIDDLE, (buttons & 4) ?
MOUSE_UP : MOUSE_DOWN);
}
}
static void kd_mouse_write(
unsigned char ch)
{
while (inb(K_STATUS) & K_IBUF_FUL)
continue;
outb(K_CMD, 0xd4);
while (inb(K_STATUS) & K_IBUF_FUL)
continue;
outb(K_RDWR, ch);
}
static int kd_mouse_read(void)
{
int ch;
if (mouse_char_index >= mousebufsize)
return -1;
while (mousebufindex <= mouse_char_index) {
mouse_char_wanted = TRUE;
assert_wait((event_t) &mousebuf, FALSE);
thread_block((void (*)()) 0);
}
ch = mousebuf[mouse_char_index++];
return ch;
}
static void kd_mouse_read_reset(void)
{
mousebufindex = 0;
mouse_char_index = 0;
}
void
ibm_ps2_mouse_open(dev_t dev)
{
spl_t s = spltty();
lastbuttons = 0;
mouse_char_cmd = TRUE;
kd_sendcmd(0xa8);
kd_cmdreg_write(0x47);
kd_mouse_read_reset();
kd_mouse_write(0xff);
if (kd_mouse_read() != 0xfa) {
splx(s);
return;
}
(void) kd_mouse_read();
(void) kd_mouse_read();
kd_mouse_read_reset();
kd_mouse_write(0xea);
if (kd_mouse_read() != 0xfa) {
splx(s);
return;
}
kd_mouse_read_reset();
kd_mouse_write(0xf4);
if (kd_mouse_read() != 0xfa) {
splx(s);
return;
}
kd_mouse_read_reset();
mouse_char_cmd = FALSE;
splx(s);
}
void
ibm_ps2_mouse_close(dev_t dev)
{
spl_t s = spltty();
mouse_char_cmd = TRUE;
kd_mouse_read_reset();
kd_mouse_write(0xff);
if (kd_mouse_read() == 0xfa) {
(void) kd_mouse_read();
(void) kd_mouse_read();
}
kd_sendcmd(0xa7);
kd_cmdreg_write(0x65);
splx(s);
}
void
mouse_packet_ibm_ps2_mouse(u_char mousebuf[MOUSEBUFSIZE])
{
u_char buttons, buttonchanges;
struct mouse_motion moved;
buttons = mousebuf[0] & 0x7;
buttonchanges = buttons ^ lastbuttons;
moved.mm_deltaX = ((mousebuf[0]&0x10) ? 0xffffff00 : 0 ) | (u_char)mousebuf[1];
moved.mm_deltaY = ((mousebuf[0]&0x20) ? 0xffffff00 : 0 ) | (u_char)mousebuf[2];
if (mouse_packets) {
printf("(%x:%x:%x)", mousebuf[0], mousebuf[1], mousebuf[2]);
return;
}
if (moved.mm_deltaX != 0 || moved.mm_deltaY != 0)
mouse_moved(moved);
if (buttonchanges != 0) {
lastbuttons = buttons;
if (buttonchanges & 1)
mouse_button(MOUSE_LEFT, !(buttons & 1));
if (buttonchanges & 2)
mouse_button(MOUSE_RIGHT, !((buttons & 2) >> 1));
if (buttonchanges & 4)
mouse_button(MOUSE_MIDDLE, !((buttons & 4) >> 2));
}
}
void
mouse_moved(struct mouse_motion where)
{
kd_event ev;
ev.type = MOUSE_MOTION;
ev.unused_time.seconds = 0;
ev.unused_time.microseconds = 0;
ev.value.mmotion = where;
mouse_enqueue(&ev);
}
void
mouse_button(
kev_type which,
u_char direction)
{
kd_event ev;
ev.type = which;
ev.value.up = (direction == MOUSE_UP) ? TRUE : FALSE;
ev.unused_time.seconds = 0;
ev.unused_time.microseconds = 0;
mouse_enqueue(&ev);
}
void
mouse_enqueue(kd_event *ev)
{
if (kdq_full(&mouse_queue))
printf_once("mouse: queue full\n");
else
kdq_put(&mouse_queue, ev);
{
io_req_t ior;
while ((ior = (io_req_t)dequeue_head(&mouse_read_queue)) != 0)
iodone(ior);
}
}