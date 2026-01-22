enum {
MaxEther = 2,
Ntypes = 8,
};
typedef struct Ether Ether;
struct Ether {
int ctlrno;
int port;
int irq;
int tbdf;
void *ctlr;
Queue* oq;
uchar ea[Eaddrlen];
long (*ifstat)(Ether*, void*, long, ulong);
#ifdef MULTIETHERTYPES
void (*attach)(Ether*);
void (*detach)(Ether*);
void (*transmit)(Ether*);
void (*interrupt)(Ureg*, void*);
long (*ctl)(Ether*, void*, long);
void (*power)(Ether*, int);
void (*shutdown)(Ether*);
#endif
Netif;
};
extern Block* etheriq(Ether*, Block*, int);
extern void addethercard(char*, int(*)(Ether*));
extern ulong ethercrc(uchar*, int);
extern int parseether(uchar*, char*);
#define NEXT(x, l) (((x)+1)%(l))
#define PREV(x, l) (((x) == 0) ? (l)-1: (x)-1)