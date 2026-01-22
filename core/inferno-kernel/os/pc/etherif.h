enum {
MaxEther	= 24,
Ntypes		= 8,
};
typedef struct Ether Ether;
struct Ether {
ISAConf;
int	ctlrno;
int	tbdf;
int	minmtu;
int 	maxmtu;
uchar	ea[Eaddrlen];
void	(*attach)(Ether*);
void	(*detach)(Ether*);
void	(*transmit)(Ether*);
void	(*interrupt)(Ureg*, void*);
long	(*ifstat)(Ether*, void*, long, ulong);
long 	(*ctl)(Ether*, void*, long);
void	(*power)(Ether*, int);
void	(*shutdown)(Ether*);
void	*ctlr;
Queue*	oq;
Netif;
};
extern Block* etheriq(Ether*, Block*, int);
extern void addethercard(char*, int(*)(Ether*));
extern ulong ethercrc(uchar*, int);
extern int parseether(uchar*, char*);
#define NEXT(x, l)	(((x)+1)%(l))
#define PREV(x, l)	(((x) == 0) ? (l)-1: (x)-1)
#define	HOWMANY(x, y)	(((x)+((y)-1))/(y))
#define ROUNDUP(x, y)	(HOWMANY((x), (y))*(y))