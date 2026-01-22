#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
enum
{
Data= 0,
Iena= 1,
Ircv= (1<<0),
Ixmt= (1<<1),
Irstat=(1<<2),
Imstat=(1<<3),
Istat= 2,
Tctl= 2,
Format= 3,
Bits8= (3<<0),
Stop2= (1<<2),
Pena= (1<<3),
Peven= (1<<4),
Pforce=(1<<5),
Break= (1<<6),
Dra= (1<<7),
Mctl= 4,
Dtr= (1<<0),
Rts= (1<<1),
Ri= (1<<2),
Inton= (1<<3),
Loop= (1<<4),
Lstat= 5,
Inready=(1<<0),
Oerror=(1<<1),
Perror=(1<<2),
Ferror=(1<<3),
Outready=(1<<5),
Mstat= 6,
Ctsc= (1<<0),
Dsrc= (1<<1),
Rire= (1<<2),
Dcdc= (1<<3),
Cts= (1<<4),
Dsr= (1<<5),
Ring= (1<<6),
Dcd= (1<<7),
Scratch=7,
Dlsb= 0,
Dmsb= 1,
Serial= 0,
Modem= 1,
};
typedef struct Uart Uart;
struct Uart
{
int port;
uchar sticky[8];
uchar txbusy;
void (*rx)(int);
int (*tx)(void);
ulong frame;
ulong overrun;
};
static Uart com[2];
static Uart* uart;
#define UartFREQ 1843200
#define uartwrreg(u,r,v) outb((u)->port + r, (u)->sticky[r] | (v))
#define uartrdreg(u,r) inb((u)->port + r)
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
switch(s & 0x3F){
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
if(up->rx)
(*up->rx)(ch);
break;
case 2:
ch = -1;
if(up->tx)
ch = (*up->tx)();
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
if(up->tx)
up->sticky[Iena] |= Ixmt;
if(up->rx)
up->sticky[Iena] |= Ircv|Irstat;
uartwrreg(up, Iena, 0);
uartdtr(up, 1);
uartrts(up, 1);
}
static void
uartdisable(Uart* up)
{
up->sticky[Iena] = 0;
uartwrreg(up, Iena, 0);
uartdtr(up, 0);
uartrts(up, 0);
}
void
uartspecial(int port, void (*rx)(int), int (*tx)(void), int baud)
{
Uart *up;
int vector;
switch(port){
case 0:
port = 0x3F8;
vector = VectorUART0;
up = &com[0];
break;
case 1:
port = 0x2F8;
vector = VectorUART1;
up = &com[1];
break;
default:
return;
}
if(uart != nil && uart != up)
uartdisable(uart);
uart = up;
if(up->port == 0){
up->port = port;
setvec(vector, uartintr, up);
}
uartsetbaud(up, 9600);
up->sticky[Format] = Bits8;
uartwrreg(up, Format, 0);
up->sticky[Mctl] |= Inton;
uartwrreg(up, Mctl, 0x0);
up->rx = rx;
up->tx = tx;
uartenable(up);
if(baud)
uartsetbaud(up, baud);
}
void
uartputc(int c)
{
int i;
Uart *up;
if((up = uart) == nil)
return;
for(i = 0; i < 100; i++){
if(uartrdreg(up, Lstat) & Outready)
break;
delay(1);
}
outb(up->port+Data, c);
}
void
uartputs(IOQ *q, char *s, int n)
{
Uart *up;
int c, x;
if((up = uart) == nil)
return;
while(n--){
if(*s == '\n')
q->putc(q, '\r');
q->putc(q, *s++);
}
x = splhi();
if(up->txbusy == 0 && (c = q->getc(q)) != -1){
uartputc(c & 0xFF);
up->txbusy = 1;
}
splx(x);
}
void
uartdrain(void)
{
Uart *up;
int timeo;
if((up = uart) == nil)
return;
for(timeo = 0; timeo < 10000 && up->txbusy; timeo++)
delay(1);
}