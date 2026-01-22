enum
{
MaxEther = 2,
Ntypes = 8,
};
typedef struct Ether Ether;
struct Ether {
RWlock;
ISAConf;
int ctlrno;
int minmtu;
int maxmtu;
uchar ea[Eaddrlen];
void *address;
int tbusy;
void (*attach)(Ether*);
void (*closed)(Ether*);
void (*detach)(Ether*);
void (*transmit)(Ether*);
void (*interrupt)(Ureg*, void*);
long (*ifstat)(Ether*, void*, long, ulong);
long (*ctl)(Ether*, void*, long);
void (*power)(Ether*, int);
void (*shutdown)(Ether*);
void *ctlr;
int pcmslot;
int fullduplex;
int linkchg;
uvlong starttime;
Queue* oq;
ulong interrupts;
ulong dmarxintr;
ulong dmatxintr;
ulong promisc;
ulong pktsdropped;
ulong pktsmisaligned;
ulong resets;
ulong bcasts;
ulong mcasts;
Netif;
};
extern Block* etheriq(Ether*, Block*, int);
extern void addethercard(char*, int(*)(Ether*));
extern ulong ethercrc(uchar*, int);
extern int parseether(uchar*, char*);
#define NEXT(x, l) (((x)+1)%(l))
#define PREV(x, l) (((x) == 0) ? (l)-1: (x)-1)