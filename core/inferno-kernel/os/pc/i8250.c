#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
enum
{
Data=	0,
Iena=	1,
Ircv=	(1<<0),
Ixmt=	(1<<1),
Irstat=(1<<2),
Imstat=(1<<3),
Istat=	2,
Fenabd=(3<<6),
Fifoctl=2,
Fena=	(1<<0),
Ftrig=	(1<<6),
Fclear=(3<<1),
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
uchar	sticky[8];
int	nofifo;
void	(*rx)(int);
int	(*tx)(void);
ulong	frame;
ulong	overrun;
};
static Uart i8250uart[1];
#define UartFREQ 1843200
#define i8250regw(u, r, v)	outb((u)->port+(r), (u)->sticky[(r)]|(v))
#define i8250regr(u, r)		inb((u)->port+(r))
static void
i8250setbaud(Uart* uart, int rate)
{
ulong brconst;
brconst = (UartFREQ+8*rate-1)/(16*rate);
i8250regw(uart, Format, Dra);
outb(uart->port+Dmsb, (brconst>>8) & 0xff);
outb(uart->port+Dlsb, brconst & 0xff);
i8250regw(uart, Format, 0);
}
static void
i8250dtr(Uart* uart, int n)
{
if(n)
uart->sticky[Mctl] |= Dtr;
else
uart->sticky[Mctl] &= ~Dtr;
i8250regw(uart, Mctl, 0);
}
static void
i8250rts(Uart* uart, int n)
{
if(n)
uart->sticky[Mctl] |= Rts;
else
uart->sticky[Mctl] &= ~Rts;
i8250regw(uart, Mctl, 0);
}
static void
i8250fifo(Uart* uart, int n)
{
int i, s;
if(uart->nofifo)
return;
s = splhi();
i8250regw(uart, Fifoctl, Fclear);
for(i = 0; i < 16; i++){
if(i8250regr(uart, Istat))
{}
if(i8250regr(uart, Data))
{}
}
if(n){
i8250regw(uart, Fifoctl, Fena|Ftrig);
if((i8250regr(uart, Istat) & Fenabd) == 0){
uart->nofifo = 1;
}
}
splx(s);
}
#ifdef notdef
static void
i8250intr(Ureg*, void* arg)
{
Uart *uart;
int ch;
int s, l, loops;
uart = arg;
for(loops = 0; loops < 1024; loops++){
s = i8250regr(uart, Istat);
switch(s & 0x3F){
case 6:
l = i8250regr(uart, Lstat);
if(l & Ferror)
uart->frame++;
if(l & Oerror)
uart->overrun++;
break;
case 4:
case 12:
ch = inb(uart->port+Data);
if(uart->rx)
(*uart->rx)(ch & 0x7F);
break;
case 2:
ch = -1;
if(uart->tx)
ch = (*uart->tx)();
if(ch != -1)
outb(uart->port+Data, ch);
break;
case 0:
i8250regr(uart, Mstat);
break;
default:
if(s&1)
return;
print("weird modem interrupt #%2.2ux\n", s);
break;
}
}
panic("i8250intr: 0x%2.2ux\n", i8250regr(uart, Istat));
}
#endif
static void
i8250enable(Uart* uart)
{
uart->sticky[Iena] = 0;
#ifdef notdef
if(uart->tx)
uart->sticky[Iena] |= Ixmt;
if(uart->rx)
uart->sticky[Iena] |= Ircv|Irstat;
#endif
i8250dtr(uart, 1);
i8250rts(uart, 1);
i8250fifo(uart, 1);
i8250regw(uart, Iena, 0);
}
void
i8250special(int port, void (*rx)(int), int (*tx)(void), int baud)
{
Uart *uart = &i8250uart[0];
if(uart->port)
return;
switch(port){
case 0:
uart->port = 0x3F8;
#ifdef notdef
intrenable(VectorUART0, i8250intr, uart, BUSUNKNOWN);
#endif
break;
case 1:
uart->port = 0x2F8;
#ifdef notdef
intrenable(VectorUART1, i8250intr, uart, BUSUNKNOWN);
#endif
break;
default:
return;
}
i8250setbaud(uart, 9600);
uart->sticky[Format] = Bits8;
i8250regw(uart, Format, 0);
uart->sticky[Mctl] |= Inton;
i8250regw(uart, Mctl, 0x0);
uart->rx = rx;
uart->tx = tx;
i8250enable(uart);
if(baud)
i8250setbaud(uart, baud);
}
int
i8250getc(void)
{
Uart *uart = &i8250uart[0];
if(i8250regr(uart, Lstat) & Inready)
return inb(uart->port+Data);
return 0;
}
void
i8250putc(int c)
{
Uart *uart = &i8250uart[0];
int i;
for(i = 0; i < 100; i++){
if(i8250regr(uart, Lstat) & Outready)
break;
delay(1);
}
outb(uart->port+Data, c);
}
void
i8250puts(char* s, int n)
{
int x;
x = splhi();
while(n--){
if(*s == '\n')
i8250putc('\r');
i8250putc(*s++);
}
splx(x);
}