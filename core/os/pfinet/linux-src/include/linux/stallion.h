#ifndef	_STALLION_H
#define	_STALLION_H
#define	STL_MAXBRDS		4
#define	STL_MAXPANELS		4
#define	STL_MAXBANKS		8
#define	STL_PORTSPERPANEL	16
#define	STL_MAXPORTS		64
#define	STL_MAXDEVS		(STL_MAXBRDS * STL_MAXPORTS)
typedef struct {
char	*buf;
char	*head;
char	*tail;
} stlrq_t;
typedef struct stlport {
unsigned long		magic;
int			portnr;
int			panelnr;
int			brdnr;
int			ioaddr;
int			uartaddr;
int			pagenr;
int			istate;
int			flags;
int			baud_base;
int			custom_divisor;
int			close_delay;
int			closing_wait;
int			refcount;
int			openwaitcnt;
int			brklen;
long			session;
long			pgrp;
unsigned int		sigs;
unsigned int		rxignoremsk;
unsigned int		rxmarkmsk;
unsigned int		imr;
unsigned int		crenable;
unsigned long		clk;
unsigned long		hwid;
void			*uartp;
struct tty_struct	*tty;
struct wait_queue	*open_wait;
struct wait_queue	*close_wait;
struct termios		normaltermios;
struct termios		callouttermios;
struct tq_struct	tqueue;
comstats_t		stats;
stlrq_t			tx;
} stlport_t;
typedef struct stlpanel {
unsigned long	magic;
int		panelnr;
int		brdnr;
int		pagenr;
int		nrports;
int		iobase;
void		*uartp;
void		(*isr)(struct stlpanel *panelp, unsigned int iobase);
unsigned int	hwid;
unsigned int	ackmask;
stlport_t	*ports[STL_PORTSPERPANEL];
} stlpanel_t;
typedef struct stlbrd {
unsigned long	magic;
int		brdnr;
int		brdtype;
int		state;
int		nrpanels;
int		nrports;
int		nrbnks;
int		irq;
int		irqtype;
void		(*isr)(struct stlbrd *brdp);
unsigned int	ioaddr1;
unsigned int	ioaddr2;
unsigned int	iosize1;
unsigned int	iosize2;
unsigned int	iostatus;
unsigned int	ioctrl;
unsigned int	ioctrlval;
unsigned int	hwid;
unsigned long	clk;
unsigned int	bnkpageaddr[STL_MAXBANKS];
unsigned int	bnkstataddr[STL_MAXBANKS];
stlpanel_t	*bnk2panel[STL_MAXBANKS];
stlpanel_t	*panels[STL_MAXPANELS];
} stlbrd_t;
#define	STL_PORTMAGIC	0x5a7182c9
#define	STL_PANELMAGIC	0x7ef621a1
#define	STL_BOARDMAGIC	0xa2267f52
#endif