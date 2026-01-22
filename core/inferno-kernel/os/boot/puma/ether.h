typedef struct Card Card;
typedef struct RingBuf RingBuf;
typedef struct Type Type;
typedef struct Ctlr Ctlr;
struct Card {
ISAConf;
int	(*reset)(Ctlr*);
void	(*attach)(Ctlr*);
void	*(*read)(Ctlr*, void*, ulong, ulong);
void	*(*write)(Ctlr*, ulong, void*, ulong);
void	(*receive)(Ctlr*);
void	(*transmit)(Ctlr*);
void	(*intr)(Ureg*, Ctlr*);
void	(*overflow)(Ctlr*);
uchar	bit16;
uchar	ram;
ulong	dp8390;
ulong	data;
uchar	nxtpkt;
uchar	tstart;
uchar	pstart;
uchar	pstop;
uchar	dummyrr;
};
struct RingBuf {
uchar	owner;
uchar	busy;
ushort	len;
uchar	pkt[sizeof(Etherpkt)];
};
enum {
Host		= 0,
Interface	= 1,
Nrb		= 16,
Ntb		= 2,
};
struct Ctlr {
Card	card;
int	ctlrno;
int	present;
Queue*	iq;
Queue*	oq;
int	inpackets;
int	outpackets;
int	crcs;
int	oerrs;
int	frames;
int	overflows;
int	buffs;
};
#define NEXT(x, l)	(((x)+1)%(l))
#define	HOWMANY(x, y)	(((x)+((y)-1))/(y))
#define ROUNDUP(x, y)	(HOWMANY((x), (y))*(y))
extern int cs8900reset(Ctlr*);
extern int	etheriq(Ctlr*, Block*, int);