typedef struct RingBuf {
uchar owner;
uchar unused;
ushort len;
uchar pkt[sizeof(Etherpkt)];
} RingBuf;
enum {
Host = 0,
Interface = 1,
Nrb = 32,
Ntb = 8,
};
typedef struct Ether Ether;
struct Ether {
ISAConf;
int ctlrno;
int state;
int tbdf;
void (*attach)(Ether*);
void (*transmit)(Ether*);
void (*interrupt)(Ureg*, void*);
void (*detach)(Ether*);
void *ctlr;
ushort nrb;
ushort ntb;
RingBuf *rb;
RingBuf *tb;
ushort rh;
ushort ri;
ushort th;
ushort ti;
int tbusy;
int mbps;
};
extern void etherrloop(Ether*, Etherpkt*, long);
extern void addethercard(char*, int(*)(Ether*));
#define NEXT(x, l) (((x)+1)%(l))
#define PREV(x, l) (((x) == 0) ? (l)-1: (x)-1)