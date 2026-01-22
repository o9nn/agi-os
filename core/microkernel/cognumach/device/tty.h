#ifndef	_DEVICE_TTY_H_
#define	_DEVICE_TTY_H_
#include <kern/lock.h>
#include <kern/queue.h>
#include <mach/port.h>
#include <device/device_types.h>
#include <device/tty_status.h>
#include <device/cirbuf.h>
#include <device/io_req.h>
struct tty {
decl_simple_lock_irq_data(,t_lock)
struct cirbuf	t_inq;
struct cirbuf	t_outq;
char *		t_addr;
int		t_dev;
void		(*t_start)(struct tty *);
#define	t_oproc	t_start
void		(*t_stop)(struct tty *, int);
int		(*t_mctl)(struct tty *, int, int);
unsigned char	t_ispeed;
unsigned char	t_ospeed;
char		t_breakc;
int		t_flags;
int		t_state;
int		t_line;
queue_head_t	t_delayed_read;
queue_head_t	t_delayed_write;
queue_head_t	t_delayed_open;
io_return_t	(*t_getstat)(dev_t, dev_flavor_t, dev_status_t, mach_msg_type_number_t *);
io_return_t	(*t_setstat)(dev_t, dev_flavor_t, dev_status_t, mach_msg_type_number_t);
dev_ops_t	t_tops;
};
typedef struct tty	*tty_t;
extern io_return_t char_open(
int		dev,
struct tty *	tp,
dev_mode_t	mode,
io_req_t	ior);
extern io_return_t char_read(
struct tty *	tp,
io_req_t	ior);
extern io_return_t char_write(
struct tty *	tp,
io_req_t	ior);
extern void ttyinput(
unsigned int	c,
struct tty *	tp);
extern void ttyinput_many(
struct tty *	tp,
char *		chars,
int		count);
extern boolean_t ttymodem(
struct tty *	tp,
boolean_t	carrier_up);
extern void tty_cts(
struct tty *	tp,
boolean_t	cts_up);
extern void tty_queue_completion(
queue_t		queue);
#define	tt_open_wakeup(tp) \
(tty_queue_completion(&(tp)->t_delayed_open))
#define	tt_write_wakeup(tp) \
(tty_queue_completion(&(tp)->t_delayed_write))
extern void ttychars(
struct tty *	tp);
#define	TTMINBUF	90
extern short	tthiwat[NSPEEDS], ttlowat[NSPEEDS];
#define	TTHIWAT(tp)	tthiwat[(tp)->t_ospeed]
#define	TTLOWAT(tp)	ttlowat[(tp)->t_ospeed]
extern io_return_t tty_get_status(
struct tty *	tp,
dev_flavor_t	flavor,
int *		data,
natural_t *	count);
extern io_return_t tty_set_status(
struct tty *	tp,
dev_flavor_t	flavor,
int *		data,
natural_t	count);
extern void tty_flush(
struct tty *	tp,
int		rw);
extern void ttrstrt(
struct tty *	tp);
extern void ttstart(
struct tty *	tp);
extern void ttyclose(
struct tty *	tp);
extern boolean_t tty_portdeath(
struct tty *	tp,
ipc_port_t	port);
#define	TS_INIT		0x00000001
#define	TS_TIMEOUT	0x00000002
#define	TS_WOPEN	0x00000004
#define	TS_ISOPEN	0x00000008
#define	TS_FLUSH	0x00000010
#define	TS_CARR_ON	0x00000020
#define	TS_BUSY		0x00000040
#define	TS_ASLEEP	0x00000080
#define	TS_TTSTOP	0x00000100
#define	TS_HUPCLS	0x00000200
#define	TS_TBLOCK	0x00000400
#define	TS_NBIO		0x00001000
#define	TS_ONDELAY	0x00002000
#define	TS_MIN		0x00004000
#define	TS_MIN_TO	0x00008000
#define TS_OUT          0x00010000
#define	TS_RTS_DOWN	0x00020000
#define TS_TRANSLATE	0x00100000
#define TS_KDB		0x00200000
#define	TS_MIN_TO_RCV	0x00400000
#define	TANDEM		TF_TANDEM
#define	ODDP		TF_ODDP
#define	EVENP		TF_EVENP
#define	ANYP		(ODDP|EVENP)
#define	MDMBUF		TF_MDMBUF
#define	LITOUT		TF_LITOUT
#define	NOHANG		TF_NOHANG
#define	ECHO		TF_ECHO
#define	CRMOD		TF_CRMOD
#define	XTABS		TF_XTABS
#define	RAW		LITOUT
#define	PASS8		LITOUT
#define	DONE	0200
#define	IENABLE	0100
#define	DMSET		0
#define	DMBIS		1
#define	DMBIC		2
#define	DMGET		3
struct ldisc_switch {
int	(*l_read) (struct tty *, io_req_t);
int	(*l_write)(struct tty *, io_req_t);
void	(*l_rint) (unsigned int, struct tty *);
boolean_t (*l_modem)(struct tty *, boolean_t);
void	(*l_start)(struct tty *);
};
extern struct ldisc_switch	linesw[];
#endif