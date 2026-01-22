typedef struct {
Lock;
ulong	port;
ulong	data;
uchar	width;
uchar	ram;
uchar	dummyrr;
uchar	nxtpkt;
uchar	pstart;
uchar	pstop;
int	txbusy;
uchar	tstart;
} Dp8390;
#define Dp8390BufSz	256
extern int dp8390reset(Ether*);
extern void *dp8390read(Dp8390*, void*, ulong, ulong);
extern void dp8390getea(Ether*, uchar*);
extern void dp8390setea(Ether*);
#define regr(c, r)	inb((c)->port+(r))
#define regw(c, r, v)	outb((c)->port+(r), (v))
static void
rdread(Dp8390* ctlr, void* to, int len)
{
switch(ctlr->width){
default:
panic("dp8390 rdread: width %d\n", ctlr->width);
break;
case 2:
inss(ctlr->data, to, len/2);
break;
case 1:
insb(ctlr->data, to, len);
break;
}
}
static void
rdwrite(Dp8390* ctlr, void* from, int len)
{
switch(ctlr->width){
default:
panic("dp8390 rdwrite: width %d\n", ctlr->width);
break;
case 2:
outss(ctlr->data, from, len/2);
break;
case 1:
outsb(ctlr->data, from, len);
break;
}
}