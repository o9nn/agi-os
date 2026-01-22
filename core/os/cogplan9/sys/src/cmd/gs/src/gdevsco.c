#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "gdevpcfb.h"
#include <signal.h>
#ifdef M_XENIX
#include <sys/console.h>
#ifndef CONSIO
#include <sys/machdep.h>
#endif
#else
#include <sys/kd.h>
#endif
#if defined(__STDC__)
#include <stdlib.h>
#else
extern char *getenv(const char *);
#endif
#if defined(M_XENIX)
#include <prototypes.h>
#include <fcntl.h>
#else
extern int ioctl(int, int,...);
extern int open(const char *, int,...);
#endif
private int console_fd = -1;
fb_ptr fb_addr;
private int cur_mode = -1;
private void open_console(void);
private void
open_console()
{
const char *dev;
if (console_fd != -1)
return;
dev = getenv("GSDEVICE");
if (dev == NULL || *dev == '\0')
dev = "/dev/tty";
console_fd = open(dev, 0);
if (console_fd == -1) {
ega_close((gx_device *) NULL);
eprintf1("unable to map display '%s'\n", dev);
perror("open_console");
exit(1);
}
}
#if defined(__GNUC__)
#else
void
outportb(uint port, byte data)
{
int i;
struct port_io_arg pio;
if (console_fd == -1)
open_console();
pio.args[0].dir = OUT_ON_PORT;
pio.args[0].port = port;
pio.args[0].data = data;
pio.args[1].port = 0;
pio.args[2].port = 0;
pio.args[3].port = 0;
i = ioctl(console_fd, CONSIO, (long)(&pio));
if (i == -1) {
ega_close((gx_device *) NULL);
eprintf("error setting device register\n");
perror("outportb");
exit(1);
}
}
void
outport2(uint port, byte index, byte data)
{
int i;
struct port_io_arg pio;
if (console_fd == -1)
open_console();
pio.args[0].dir = OUT_ON_PORT;
pio.args[0].port = port;
pio.args[0].data = index;
pio.args[1].dir = OUT_ON_PORT;
pio.args[1].port = port + 1;
pio.args[1].data = data;
pio.args[2].port = 0;
pio.args[3].port = 0;
i = ioctl(console_fd, CONSIO, (long)(&pio));
if (i == -1) {
ega_close((gx_device *) NULL);
eprintf("error setting device register\n");
perror("outport2");
exit(1);
}
}
#endif
private void
ega_int_handler(int sig)
{
ega_close((gx_device *) NULL);
eprintf("GS exiting...\n");
exit(1);
}
#ifdef	SIGTSTP
private void
ega_tstp_handler(int sig)
{
#if 1
ega_int_handler(sig);
#else
signal(SIGTSTP, ega_tstp_handler);
ega_close((gx_device *) NULL);
eprintf("GS stopping...\n");
signal(SIGSTOP, SIG_DFL);
kill(getpid(), SIGSTOP);
#endif
}
#endif
#ifdef	SIGCONT
private void
ega_cont_handler(int sig)
{
#if 1
ega_int_handler(sig);
#else
signal(SIGCONT, ega_cont_handler);
ega_set_mode(cur_mode);
#endif
}
#endif
void
pcfb_set_signals(gx_device * dev)
{
signal(SIGINT, ega_int_handler);
signal(SIGTERM, ega_int_handler);
#ifdef	SIGTSTP
signal(SIGTSTP, ega_tstp_handler);
#endif
#ifdef	SIGCONT
signal(SIGCONT, ega_cont_handler);
#endif
}
void
pcfb_get_state(pcfb_bios_state * pbs)
{
int mode;
open_console();
mode = ioctl(console_fd, CONS_CURRENT, 0L);
if (mode == -1) {
#ifdef __linux__
mode = M_ENH_C80x25;
#else
ega_close((gx_device *) NULL);
eprintf("unable to get current console mode\n");
perror("pcfb_get_state");
exit(1);
#endif
}
pbs->display_mode =
(mode == M_ENH_CG640 || mode == M_CG640x350 ? 0x10 :
#ifdef M_VGA12
mode == M_VGA12 ? 0x12 :
#endif
0x03);
}
void
pcfb_set_mode(int mode)
{
int i, mode1;
open_console();
cur_mode = mode;
mode1 = -1;
if (mode == 0x10)
mode = SW_ENH_CG640;
#ifdef SW_VGA12
else if (mode == 0x12)
mode = SW_VGA12;
#endif
else if (mode == 0x03) {
#ifdef SW_VGA80x25
mode = SW_VGA80x25;
mode1 = SW_ENHC80x25;
#else
mode = SW_ENHC80x25;
#endif
} else {
eprintf1("can not set to video mode %d\n", mode);
exit(1);
}
i = ioctl(console_fd, mode, 0L);
if (i == -1 && mode1 != -1)
i = ioctl(console_fd, mode1, 0L);
if (i == -1) {
ega_close((gx_device *) NULL);
eprintf("unable to set console mode\n");
perror("pcfb_set_mode");
exit(1);
}
#ifdef VGA_IOPRIVL
if (ioctl(console_fd, VGA_IOPRIVL, 1) == -1) {
ega_close((gx_device *) NULL);
eprintf("unable to get I/O privilege\n");
perror("pcfb_set_mode");
exit(1);
}
#endif
i = ioctl(console_fd, MAPCONS, 0L);
if (i == -1) {
ega_close((gx_device *) NULL);
eprintf("unable to map console adaptor's display memory\n");
perror("pcfb_set_mode");
exit(1);
}
fb_addr = (fb_ptr) (i);
}
void
pcfb_set_state(const pcfb_bios_state * pbs)
{
pcfb_set_mode(pbs->display_mode);
}