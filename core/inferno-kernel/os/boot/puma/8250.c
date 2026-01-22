#include "boot.h"
enum
{
Data=	0,
Iena=	1,
Ircv=	(1<<0),
Ixmt=	(1<<1),
Irstat=(1<<2),
Imstat=(1<<3),
Istat=	2,
Tctl=	2,
Format=	3,
Bits8=	(3<<0),
Stop2=	(1<<2),
Pena=	(1<<3),
Peven=	(1<<4),
Pforce=(1<<5),
Break=	(1<<6),
Dra=	(1<<7),
Mctl=	4,
Dtr=	(1<<0),
Rts=	(1<<1),
Ri=	(1<<2),
Inton=	(1<<3),
Loop=	(1<<4),
Lstat=	5,
Inready=(1<<0),
Oerror=(1<<1),
Perror=(1<<2),
Ferror=(1<<3),
Outready=(1<<5),
Mstat=	6,
Ctsc=	(1<<0),
Dsrc=	(1<<1),
Rire=	(1<<2),
Dcdc=	(1<<3),
Cts=	(1<<4),
Dsr=	(1<<5),
Ring=	(1<<6),
Dcd=	(1<<7),
Scratch=7,
Dlsb=	0,
Dmsb=	1,
Serial=	0,
Modem=	1,
};
typedef struct Uart	Uart;
struct Uart
{
int	port;
int	setup;
uchar	sticky[8];
uchar	txbusy;
Queue	*iq;
Queue	*oq;
void	(*rx)(Queue *, int);
ulong	frame;
ulong	overrun;
};
Uart	uart[1];
static	void	uartkick(void*);
#define UartFREQ 1843200
#define uartwrreg(u,r,v)	outb((u)->port + r, (u)->sticky[r] | (v))
#define uartrdreg(u,r)		inb((u)->port + r)
static void
uartsetbaud(Uart *up, int rate)
{
ulong brconst;
brconst = (UartFREQ+8*rate-1)/(16*rate);
uartwrreg(up, Format, Dra);
outb(up->port+Dmsb, (brconst>>8) & 0xff);
outb(up->port+Dlsb, brconst & 0xff);
uartwrreg(up, Format, 0);
}
static void
uartdtr(Uart *up, int n)
{
if(n)
up->sticky[Mctl] |= Dtr;
else
up->sticky[Mctl] &= ~Dtr;
uartwrreg(up, Mctl, 0);
}
static void
uartrts(Uart *up, int n)
{
if(n)
up->sticky[Mctl] |= Rts;
else
up->sticky[Mctl] &= ~Rts;
uartwrreg(up, Mctl, 0);
}
static void
uartintr(Ureg*, void *arg)
{
Uart *up;
int ch;
int s, l, loops;
up = arg;
for(loops = 0; loops < 1024; loops++){
s = uartrdreg(up, Istat);
switch(s){
case 6:
l = uartrdreg(up, Lstat);
if(l & Ferror)
up->frame++;
if(l & Oerror)
up->overrun++;
break;
case 4:
case 12:
ch = inb(up->port+Data);
if(up->iq)
if(up->rx)
(*up->rx)(up->iq, ch);
else
qbputc(up->iq, ch);
break;
case 2:
ch = -1;
if(up->oq)
ch = qbgetc(up->oq);
if(ch != -1)
outb(up->port+Data, ch);
else
up->txbusy = 0;
break;
case 0:
uartrdreg(up, Mstat);
break;
default:
if(s&1)
return;
print("weird modem interrupt #%2.2ux\n", s);
break;
}
}
panic("uartintr: 0x%2.2ux\n", uartrdreg(up, Istat));
}
static void
uartenable(Uart *up)
{
up->sticky[Iena] = 0;
if(up->oq)
up->sticky[Iena] |= Ixmt;
if(up->iq)
up->sticky[Iena] |= Ircv|Irstat;
uartwrreg(up, Iena, 0);
uartdtr(up, 1);
uartrts(up, 1);
}
void
uartspecial(int port, int baud, Queue **iq, Queue **oq, void (*rx)(Queue *, int))
{
Uart *up = &uart[0];
if(up->setup)
return;
up->setup = 1;
*iq = up->iq = qopen(4*1024, 0, 0, 0);
*oq = up->oq = qopen(16*1024, 0, uartkick, up);
switch(port){
case 0:
up->port = 0x3F8;
setvec(V_COM1, uartintr, up);
break;
case 1:
up->port = 0x2F8;
setvec(V_COM2, uartintr, up);
break;
default:
return;
}
uartsetbaud(up, 9600);
up->sticky[Format] = Bits8;
uartwrreg(up, Format, 0);
up->sticky[Mctl] |= Inton;
uartwrreg(up, Mctl, 0x0);
up->rx = rx;
uartenable(up);
if(baud)
uartsetbaud(up, baud);
}
static void
uartputc(int c)
{
Uart *up = &uart[0];
int i;
for(i = 0; i < 100; i++){
if(uartrdreg(up, Lstat) & Outready)
break;
delay(1);
}
outb(up->port+Data, c);
}
void
uartputs(char *s, int n)
{
Uart *up = &uart[0];
Block *b;
int nl;
char *p;
nl = 0;
for(p = s; p < s+n; p++)
if(*p == '\n')
nl++;
b = iallocb(n+nl);
while(n--){
if(*s == '\n')
*b->wp++ = '\r';
*b->wp++ = *s++;
}
qbwrite(up->oq, b);
}
static void
uartkick(void *arg)
{
Uart *up = arg;
int x, n, c;
x = splhi();
while(up->txbusy == 0 && (c = qbgetc(up->oq)) != -1) {
n = 0;
while((uartrdreg(up, Lstat) & Outready) == 0){
if(++n > 100000){
print("stuck serial line\n");
break;
}
}
outb(up->port + Data, c);
}
splx(x);
}
void
uartwait(void)
{
Uart *up = &uart[0];
while(up->txbusy)
;
}