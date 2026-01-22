#ifndef _DEVICE_TTY_STATUS_H_
#define _DEVICE_TTY_STATUS_H_
struct tty_status {
int	tt_ispeed;
int	tt_ospeed;
int	tt_breakc;
int	tt_flags;
};
#define	TTY_STATUS_COUNT	(sizeof(struct tty_status)/sizeof(int))
#define	TTY_STATUS		(dev_flavor_t)(('t'<<16) + 1)
#define B0	0
#define B50	1
#define B75	2
#define B110	3
#define B134	4
#define B150	5
#define B200	6
#define B300	7
#define B600	8
#define B1200	9
#define	B1800	10
#define B2400	11
#define B4800	12
#define B9600	13
#define EXTA	14
#define EXTB	15
#define B19200	EXTA
#define B38400  EXTB
#define B57600	16
#define B115200	17
#define	NSPEEDS	18
#define	TF_TANDEM	0x00000001
#define	TF_ODDP		0x00000002
#define	TF_EVENP	0x00000004
#define	TF_ANYP		(TF_ODDP|TF_EVENP)
#define	TF_LITOUT	0x00000008
#define	TF_MDMBUF	0x00000010
#define	TF_NOHANG	0x00000020
#define	TF_HUPCLS	0x00000040
#define	TF_ECHO		0x00000080
#define	TF_CRMOD	0x00000100
#define	TF_XTABS	0x00000200
#define	TTY_MODEM_COUNT		(1)
#define	TTY_MODEM		(dev_flavor_t)(('t'<<16) + 2)
#define	TM_LE		0x0001
#define	TM_DTR		0x0002
#define	TM_RTS		0x0004
#define	TM_ST		0x0008
#define	TM_SR		0x0010
#define	TM_CTS		0x0020
#define	TM_CAR		0x0040
#define	TM_RNG		0x0080
#define	TM_DSR		0x0100
#define	TM_BRK		0x0200
#define	TM_HUP		0x0000
#define	TTY_FLUSH_COUNT		(1)
#define	TTY_FLUSH		(dev_flavor_t)(('t'<<16) + 3)
#define	TTY_STOP		(dev_flavor_t)(('t'<<16) + 4)
#define	TTY_START		(dev_flavor_t)(('t'<<16) + 5)
#define	TTY_SET_BREAK		(dev_flavor_t)(('t'<<16) + 6)
#define	TTY_CLEAR_BREAK		(dev_flavor_t)(('t'<<16) + 7)
#define TTY_SET_TRANSLATION	(dev_flavor_t)(('t'<<16) + 8)
#endif