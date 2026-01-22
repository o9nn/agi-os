#ifndef _LINUX_SERIAL_H
#define _LINUX_SERIAL_H
struct serial_struct {
int	type;
int	line;
int	port;
int	irq;
int	flags;
int	xmit_fifo_size;
int	custom_divisor;
int	baud_base;
unsigned short	close_delay;
char	reserved_char[2];
int	hub6;
unsigned short	closing_wait;
unsigned short	closing_wait2;
int	reserved[4];
};
#define ASYNC_CLOSING_WAIT_INF	0
#define ASYNC_CLOSING_WAIT_NONE	65535
#define PORT_UNKNOWN	0
#define PORT_8250	1
#define PORT_16450	2
#define PORT_16550	3
#define PORT_16550A	4
#define PORT_CIRRUS     5
#define PORT_16650	6
#define PORT_16650V2	7
#define PORT_16750	8
#define PORT_STARTECH	9
#define PORT_MAX	9
struct serial_uart_config {
char	*name;
int	dfl_xmit_fifo_size;
int	flags;
};
#define UART_CLEAR_FIFO		0x01
#define UART_USE_FIFO		0x02
#define UART_STARTECH		0x04
#define ASYNC_HUP_NOTIFY 0x0001
#define ASYNC_FOURPORT  0x0002
#define ASYNC_SAK	0x0004
#define ASYNC_SPLIT_TERMIOS 0x0008
#define ASYNC_SPD_MASK	0x1030
#define ASYNC_SPD_HI	0x0010
#define ASYNC_SPD_VHI	0x0020
#define ASYNC_SPD_CUST	0x0030
#define ASYNC_SKIP_TEST	0x0040
#define ASYNC_AUTO_IRQ  0x0080
#define ASYNC_SESSION_LOCKOUT 0x0100
#define ASYNC_PGRP_LOCKOUT    0x0200
#define ASYNC_CALLOUT_NOHUP   0x0400
#define ASYNC_HARDPPS_CD	0x0800
#define ASYNC_SPD_SHI	0x1000
#define ASYNC_SPD_WARP	0x1010
#define ASYNC_LOW_LATENCY 0x2000
#define ASYNC_FLAGS	0x3FFF
#define ASYNC_USR_MASK	0x3430
#define ASYNC_INITIALIZED	0x80000000
#define ASYNC_CALLOUT_ACTIVE	0x40000000
#define ASYNC_NORMAL_ACTIVE	0x20000000
#define ASYNC_BOOT_AUTOCONF	0x10000000
#define ASYNC_CLOSING		0x08000000
#define ASYNC_CTS_FLOW		0x04000000
#define ASYNC_CHECK_CD		0x02000000
#define ASYNC_SHARE_IRQ		0x01000000
#define ASYNC_INTERNAL_FLAGS	0xFF000000
struct serial_multiport_struct {
int		irq;
int		port1;
unsigned char	mask1, match1;
int		port2;
unsigned char	mask2, match2;
int		port3;
unsigned char	mask3, match3;
int		port4;
unsigned char	mask4, match4;
int		port_monitor;
int	reserved[32];
};
struct serial_icounter_struct {
int cts, dsr, rng, dcd;
int rx, tx;
int frame, overrun, parity, brk;
int buf_overrun;
int reserved[9];
};
#ifdef __KERNEL__
extern int register_serial(struct serial_struct *req);
extern void unregister_serial(int line);
#endif
#endif