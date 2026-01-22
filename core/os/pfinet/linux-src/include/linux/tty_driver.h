#ifndef _LINUX_TTY_DRIVER_H
#define _LINUX_TTY_DRIVER_H
#include <linux/fs.h>
struct tty_driver {
int	magic;
const char	*driver_name;
const char	*name;
int	name_base;
short	major;
short	minor_start;
short	num;
short	type;
short	subtype;
struct termios init_termios;
int	flags;
int	*refcount;
struct proc_dir_entry *proc_entry;
struct tty_driver *other;
struct tty_struct **table;
struct termios **termios;
struct termios **termios_locked;
void *driver_state;
int  (*open)(struct tty_struct * tty, struct file * filp);
void (*close)(struct tty_struct * tty, struct file * filp);
int  (*write)(struct tty_struct * tty, int from_user,
const unsigned char *buf, int count);
void (*put_char)(struct tty_struct *tty, unsigned char ch);
void (*flush_chars)(struct tty_struct *tty);
int  (*write_room)(struct tty_struct *tty);
int  (*chars_in_buffer)(struct tty_struct *tty);
int  (*ioctl)(struct tty_struct *tty, struct file * file,
unsigned int cmd, unsigned long arg);
void (*set_termios)(struct tty_struct *tty, struct termios * old);
void (*throttle)(struct tty_struct * tty);
void (*unthrottle)(struct tty_struct * tty);
void (*stop)(struct tty_struct *tty);
void (*start)(struct tty_struct *tty);
void (*hangup)(struct tty_struct *tty);
void (*break_ctl)(struct tty_struct *tty, int state);
void (*flush_buffer)(struct tty_struct *tty);
void (*set_ldisc)(struct tty_struct *tty);
void (*wait_until_sent)(struct tty_struct *tty, int timeout);
void (*send_xchar)(struct tty_struct *tty, char ch);
int (*read_proc)(char *page, char **start, off_t off,
int count, int *eof, void *data);
int (*write_proc)(struct file *file, const char *buffer,
unsigned long count, void *data);
struct tty_driver *next;
struct tty_driver *prev;
};
#define TTY_DRIVER_MAGIC		0x5402
#define TTY_DRIVER_INSTALLED		0x0001
#define TTY_DRIVER_RESET_TERMIOS	0x0002
#define TTY_DRIVER_REAL_RAW		0x0004
#define TTY_DRIVER_TYPE_SYSTEM		0x0001
#define TTY_DRIVER_TYPE_CONSOLE		0x0002
#define TTY_DRIVER_TYPE_SERIAL		0x0003
#define TTY_DRIVER_TYPE_PTY		0x0004
#define TTY_DRIVER_TYPE_SCC		0x0005
#define TTY_DRIVER_TYPE_SYSCONS		0x0006
#define SYSTEM_TYPE_TTY			0x0001
#define SYSTEM_TYPE_CONSOLE		0x0002
#define SYSTEM_TYPE_SYSCONS		0x0003
#define SYSTEM_TYPE_SYSPTMX		0x0004
#define PTY_TYPE_MASTER			0x0001
#define PTY_TYPE_SLAVE			0x0002
#define SERIAL_TYPE_NORMAL	1
#define SERIAL_TYPE_CALLOUT	2
#endif