#ifndef _LINUX_SERIALP_H
#define _LINUX_SERIALP_H
#include <linux/termios.h>
#include <linux/tqueue.h>
struct async_icount {
__u32	cts, dsr, rng, dcd, tx, rx;
__u32	frame, parity, overrun, brk;
__u32	buf_overrun;
};
struct serial_state {
int	magic;
int	baud_base;
int	port;
int	irq;
int	flags;
int	hub6;
int	type;
int	line;
int	xmit_fifo_size;
int	custom_divisor;
int	count;
unsigned short	close_delay;
unsigned short	closing_wait;
struct async_icount	icount;
struct termios		normal_termios;
struct termios		callout_termios;
struct async_struct *info;
};
struct async_struct {
int			magic;
int			port;
int			hub6;
int			flags;
int			xmit_fifo_size;
struct serial_state	*state;
struct tty_struct 	*tty;
int			read_status_mask;
int			ignore_status_mask;
int			timeout;
int			quot;
int			x_char;
int			close_delay;
unsigned short		closing_wait;
unsigned short		closing_wait2;
int			IER;
int			MCR;
unsigned long		event;
unsigned long		last_active;
int			line;
int			blocked_open;
long			session;
long			pgrp;
unsigned char 		*xmit_buf;
int			xmit_head;
int			xmit_tail;
int			xmit_cnt;
struct tq_struct	tqueue;
struct wait_queue	*open_wait;
struct wait_queue	*close_wait;
struct wait_queue	*delta_msr_wait;
struct async_struct	*next_port;
struct async_struct	*prev_port;
};
#define SERIAL_MAGIC 0x5301
#define SSTATE_MAGIC 0x5302
#define SERIAL_XMIT_SIZE 4096
#define RS_EVENT_WRITE_WAKEUP	0
struct rs_multiport_struct {
int		port1;
unsigned char	mask1, match1;
int		port2;
unsigned char	mask2, match2;
int		port3;
unsigned char	mask3, match3;
int		port4;
unsigned char	mask4, match4;
int		port_monitor;
};
#endif