#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
enum {
Rbr		= 0,
Thr		= 0,
Ier		= 1,
Iir		= 2,
Fcr		= 2,
Lcr		= 3,
Mcr		= 4,
Lsr		= 5,
Msr		= 6,
Scr		= 7,
Mdr		= 8,
Dll		= 0,
Dlm		= 1,
};
enum {
Busy		= 0x01,
};
enum {
Erda		= 0x01,
Ethre		= 0x02,
Erls		= 0x04,
Ems		= 0x08,
};
enum {
Ims		= 0x00,
Ip		= 0x01,
Ithre		= 0x02,
Irda		= 0x04,
Irls		= 0x06,
Ictoi		= 0x0C,
IirMASK		= 0x3F,
Ifena		= 0xC0,
};
enum {
FIFOena		= 0x01,
FIFOrclr	= 0x02,
FIFOtclr	= 0x04,
FIFO1		= 0x00,
FIFO4		= 0x40,
FIFO8		= 0x80,
FIFO14		= 0xC0,
};
enum {
Wls5		= 0x00,
Wls6		= 0x01,
Wls7		= 0x02,
Wls8		= 0x03,
WlsMASK		= 0x03,
Stb		= 0x04,
Pen		= 0x08,
Eps		= 0x10,
Stp		= 0x20,
Brk		= 0x40,
Dlab		= 0x80,
};
enum {
Dtr		= 0x01,
Rts		= 0x02,
Out1		= 0x04,
Dm		= 0x10,
};
enum {
Dr		= 0x01,
Oe		= 0x02,
Pe		= 0x04,
Fe		= 0x08,
Bi		= 0x10,
Thre		= 0x20,
Temt		= 0x40,
FIFOerr		= 0x80,
};
enum {
Dcts		= 0x01,
Ddsr		= 0x02,
Teri		= 0x04,
Ddcd		= 0x08,
Cts		= 0x10,
Dsr		= 0x20,
Ri		= 0x40,
Dcd		= 0x80,
};
enum {
Modemask	= 7,
Modeuart	= 0,
};
typedef struct Ctlr {
u32int*	io;
int	irq;
int	tbdf;
int	iena;
int	poll;
uchar	sticky[Scr+1];
Lock;
int	hasfifo;
int	checkfifo;
int	fena;
} Ctlr;
extern PhysUart i8250physuart;
static Ctlr i8250ctlr[] = {
{	.io	= (u32int*)PHYSCONS,
.irq	= Uartirq,
.tbdf	= -1,
.poll	= 0, },
};
static Uart i8250uart[] = {
{	.regs	= &i8250ctlr[0],
.name	= "COM3",
.freq	= 3686000,
.phys	= &i8250physuart,
.console= 1,
.next	= nil, },
};
#define csr8r(c, r)	((c)->io[r])
#define csr8w(c, r, v)	((c)->io[r] = (c)->sticky[r] | (v), coherence())
#define csr8o(c, r, v)	((c)->io[r] = (v), coherence())
static long
i8250status(Uart* uart, void* buf, long n, long offset)
{
char *p;
Ctlr *ctlr;
uchar ier, lcr, mcr, msr;
ctlr = uart->regs;
p = malloc(READSTR);
mcr = ctlr->sticky[Mcr];
msr = csr8r(ctlr, Msr);
ier = ctlr->sticky[Ier];
lcr = ctlr->sticky[Lcr];
snprint(p, READSTR,
"b%d c%d d%d e%d l%d m%d p%c r%d s%d i%d\n"
"dev(%d) type(%d) framing(%d) overruns(%d) "
"berr(%d) serr(%d)%s%s%s%s\n",
uart->baud,
uart->hup_dcd,
(msr & Dsr) != 0,
uart->hup_dsr,
(lcr & WlsMASK) + 5,
(ier & Ems) != 0,
(lcr & Pen) ? ((lcr & Eps) ? 'e': 'o'): 'n',
(mcr & Rts) != 0,
(lcr & Stb) ? 2: 1,
ctlr->fena,
uart->dev,
uart->type,
uart->ferr,
uart->oerr,
uart->berr,
uart->serr,
(msr & Cts) ? " cts": "",
(msr & Dsr) ? " dsr": "",
(msr & Dcd) ? " dcd": "",
(msr & Ri) ? " ring": ""
);
n = readstr(offset, buf, n, p);
free(p);
return n;
}
static void
i8250fifo(Uart* uart, int level)
{
Ctlr *ctlr;
ctlr = uart->regs;
if(ctlr->hasfifo == 0)
return;
ilock(ctlr);
while(!(csr8r(ctlr, Lsr) & Temt))
;
ctlr->fena = level;
switch(level){
case 0:
break;
case 1:
level = FIFO1|FIFOena;
break;
case 4:
level = FIFO4|FIFOena;
break;
case 8:
level = FIFO8|FIFOena;
break;
default:
level = FIFO14|FIFOena;
break;
}
csr8w(ctlr, Fcr, level);
csr8w(ctlr, Fcr, level);
iunlock(ctlr);
}
static void
i8250dtr(Uart* uart, int on)
{
Ctlr *ctlr;
ctlr = uart->regs;
if(on)
ctlr->sticky[Mcr] |= Dtr;
else
ctlr->sticky[Mcr] &= ~Dtr;
csr8w(ctlr, Mcr, 0);
}
static void
i8250rts(Uart* uart, int on)
{
Ctlr *ctlr;
ctlr = uart->regs;
if(on)
ctlr->sticky[Mcr] |= Rts;
else
ctlr->sticky[Mcr] &= ~Rts;
csr8w(ctlr, Mcr, 0);
}
static void
i8250modemctl(Uart* uart, int on)
{
Ctlr *ctlr;
ctlr = uart->regs;
ilock(&uart->tlock);
if(on){
ctlr->sticky[Ier] |= Ems;
csr8w(ctlr, Ier, 0);
uart->modem = 1;
uart->cts = csr8r(ctlr, Msr) & Cts;
}
else{
ctlr->sticky[Ier] &= ~Ems;
csr8w(ctlr, Ier, 0);
uart->modem = 0;
uart->cts = 1;
}
iunlock(&uart->tlock);
(*uart->phys->fifo)(uart, on);
}
static int
i8250parity(Uart* uart, int parity)
{
int lcr;
Ctlr *ctlr;
ctlr = uart->regs;
lcr = ctlr->sticky[Lcr] & ~(Eps|Pen);
switch(parity){
case 'e':
lcr |= Eps|Pen;
break;
case 'o':
lcr |= Pen;
break;
case 'n':
break;
default:
return -1;
}
ctlr->sticky[Lcr] = lcr;
csr8w(ctlr, Lcr, 0);
uart->parity = parity;
return 0;
}
static int
i8250stop(Uart* uart, int stop)
{
int lcr;
Ctlr *ctlr;
ctlr = uart->regs;
lcr = ctlr->sticky[Lcr] & ~Stb;
switch(stop){
case 1:
break;
case 2:
lcr |= Stb;
break;
default:
return -1;
}
ctlr->sticky[Lcr] = lcr;
csr8w(ctlr, Lcr, 0);
uart->stop = stop;
return 0;
}
static int
i8250bits(Uart* uart, int bits)
{
int lcr;
Ctlr *ctlr;
ctlr = uart->regs;
lcr = ctlr->sticky[Lcr] & ~WlsMASK;
switch(bits){
case 5:
lcr |= Wls5;
break;
case 6:
lcr |= Wls6;
break;
case 7:
lcr |= Wls7;
break;
case 8:
lcr |= Wls8;
break;
default:
return -1;
}
ctlr->sticky[Lcr] = lcr;
csr8w(ctlr, Lcr, 0);
uart->bits = bits;
return 0;
}
static int
i8250baud(Uart* uart, int baud)
{
#ifdef notdef
ulong bgc;
Ctlr *ctlr;
extern int i8250freq;
if(i8250freq == 0 || baud <= 0)
return -1;
bgc = (i8250freq+8*baud-1)/(16*baud);
ctlr = uart->regs;
while(csr8r(ctlr, Usr) & Busy)
delay(1);
csr8w(ctlr, Lcr, Dlab);
csr8o(ctlr, Dlm, bgc>>8);
csr8o(ctlr, Dll, bgc);
csr8w(ctlr, Lcr, 0);
#endif
uart->baud = baud;
return 0;
}
static void
i8250break(Uart* uart, int ms)
{
Ctlr *ctlr;
if (up == nil)
panic("i8250break: nil up");
if(ms <= 0)
ms = 200;
ctlr = uart->regs;
csr8w(ctlr, Lcr, Brk);
tsleep(&up->sleep, return0, 0, ms);
csr8w(ctlr, Lcr, 0);
}
static void
emptyoutstage(Uart *uart, int n)
{
_uartputs((char *)uart->op, n);
uart->op = uart->oe = uart->ostage;
}
static void
i8250kick(Uart* uart)
{
int i;
Ctlr *ctlr;
if( uart->blocked)
return;
if(!normalprint) {
if (uart->op < uart->oe)
emptyoutstage(uart, uart->oe - uart->op);
while ((i = uartstageoutput(uart)) > 0)
emptyoutstage(uart, i);
return;
}
ctlr = uart->regs;
if (uart->op >= uart->oe && qlen(uart->oq) == 0 &&
csr8r(ctlr, Lsr) & Temt) {
ctlr->sticky[Ier] &= ~Ethre;
csr8w(ctlr, Ier, 0);
return;
}
for(i = 0; i < 128; i++){
if(!(csr8r(ctlr, Lsr) & Thre))
break;
if(uart->op >= uart->oe && uartstageoutput(uart) == 0)
break;
csr8o(ctlr, Thr, *uart->op++);
ctlr->sticky[Ier] |= Ethre;
csr8w(ctlr, Ier, 0);
}
}
void
serialkick(void)
{
uartkick(&i8250uart[CONSOLE]);
}
static void
i8250interrupt(Ureg*, void* arg)
{
Ctlr *ctlr;
Uart *uart;
int iir, lsr, old, r;
uart = arg;
ctlr = uart->regs;
for(iir = csr8r(ctlr, Iir); !(iir & Ip); iir = csr8r(ctlr, Iir)){
switch(iir & IirMASK){
case Ims:
r = csr8r(ctlr, Msr);
if(r & Dcts){
ilock(&uart->tlock);
old = uart->cts;
uart->cts = r & Cts;
if(old == 0 && uart->cts)
uart->ctsbackoff = 2;
iunlock(&uart->tlock);
}
if(r & Ddsr){
old = r & Dsr;
if(uart->hup_dsr && uart->dsr && !old)
uart->dohup = 1;
uart->dsr = old;
}
if(r & Ddcd){
old = r & Dcd;
if(uart->hup_dcd && uart->dcd && !old)
uart->dohup = 1;
uart->dcd = old;
}
break;
case Ithre:
uartkick(uart);
break;
case Irda:
case Irls:
case Ictoi:
while((lsr = csr8r(ctlr, Lsr)) & Dr){
if(lsr & (FIFOerr|Oe))
uart->oerr++;
if(lsr & Pe)
uart->perr++;
if(lsr & Fe)
uart->ferr++;
r = csr8r(ctlr, Rbr);
if(!(lsr & (Bi|Fe|Pe)))
uartrecv(uart, r);
}
break;
default:
iprint("weird uart interrupt type %#2.2uX\n", iir);
break;
}
}
}
static void
i8250disable(Uart* uart)
{
Ctlr *ctlr;
(*uart->phys->dtr)(uart, 0);
(*uart->phys->rts)(uart, 0);
(*uart->phys->fifo)(uart, 0);
ctlr = uart->regs;
ctlr->sticky[Ier] = 0;
csr8w(ctlr, Ier, 0);
if(ctlr->iena != 0){
if(irqdisable(ctlr->irq, i8250interrupt, uart, uart->name) == 0)
ctlr->iena = 0;
}
}
static void
i8250enable(Uart* uart, int ie)
{
int mode;
Ctlr *ctlr;
if (up == nil)
return;
ctlr = uart->regs;
mode = csr8r(ctlr, Mdr);
csr8o(ctlr, Mdr, (mode & ~Modemask) | Modeuart);
ctlr->sticky[Lcr] = Wls8;
csr8w(ctlr, Lcr, 0);
ilock(ctlr);
if(!ctlr->checkfifo){
while(!(csr8r(ctlr, Lsr) & Temt))
;
csr8w(ctlr, Fcr, FIFOena);
if(csr8r(ctlr, Iir) & Ifena)
ctlr->hasfifo = 1;
csr8w(ctlr, Fcr, 0);
ctlr->checkfifo = 1;
}
iunlock(ctlr);
if(ie){
if(ctlr->iena == 0 && !ctlr->poll){
irqenable(ctlr->irq, i8250interrupt, uart, uart->name);
ctlr->iena = 1;
}
ctlr->sticky[Ier] = Erda;
ctlr->sticky[Mcr] = 0;
}
else{
ctlr->sticky[Ier] = 0;
ctlr->sticky[Mcr] = 0;
}
csr8w(ctlr, Ier, 0);
csr8w(ctlr, Mcr, 0);
(*uart->phys->dtr)(uart, 1);
(*uart->phys->rts)(uart, 1);
if(ie)
i8250interrupt(nil, uart);
}
static Uart*
i8250pnp(void)
{
return i8250uart;
}
static int
i8250getc(Uart* uart)
{
Ctlr *ctlr;
ctlr = uart->regs;
while(!(csr8r(ctlr, Lsr) & Dr))
delay(1);
return csr8r(ctlr, Rbr);
}
static void
i8250putc(Uart* uart, int c)
{
int i;
Ctlr *ctlr;
if (!normalprint) {
int s = splhi();
while (!(((ulong *)PHYSCONS)[Lsr] & Thre))
;
((ulong *)PHYSCONS)[Thr] = c;
coherence();
splx(s);
return;
}
ctlr = uart->regs;
for(i = 0; !(csr8r(ctlr, Lsr) & Thre) && i < 128; i++)
delay(1);
csr8o(ctlr, Thr, (uchar)c);
for(i = 0; !(csr8r(ctlr, Lsr) & Thre) && i < 128; i++)
delay(1);
}
void
serialputc(int c)
{
i8250putc(&i8250uart[CONSOLE], c);
}
void
serialputs(char* s, int n)
{
_uartputs(s, n);
}
#ifdef notdef
static void
i8250poll(Uart* uart)
{
Ctlr *ctlr;
ctlr = uart->regs;
if(ctlr->iena || !ctlr->poll)
return;
i8250interrupt(nil, uart);
}
#endif
PhysUart i8250physuart = {
.name		= "i8250",
.pnp		= i8250pnp,
.enable		= i8250enable,
.disable	= i8250disable,
.kick		= i8250kick,
.dobreak	= i8250break,
.baud		= i8250baud,
.bits		= i8250bits,
.stop		= i8250stop,
.parity		= i8250parity,
.modemctl	= i8250modemctl,
.rts		= i8250rts,
.dtr		= i8250dtr,
.status		= i8250status,
.fifo		= i8250fifo,
.getc		= i8250getc,
.putc		= i8250putc,
};
static void
i8250dumpregs(Ctlr* ctlr)
{
int dlm, dll;
int _uartprint(char*, ...);
csr8w(ctlr, Lcr, Dlab);
dlm = csr8r(ctlr, Dlm);
dll = csr8r(ctlr, Dll);
csr8w(ctlr, Lcr, 0);
_uartprint("dlm %#ux dll %#ux\n", dlm, dll);
}
Uart*	uartenable(Uart *p);
int
i8250console(void)
{
Uart *uart = &i8250uart[CONSOLE];
if (up == nil)
return -1;
if(uartenable(uart) != nil ){
kbdq = uart->iq;
serialoq = uart->oq;
uart->putc = kbdcr2nl;
uart->opens++;
consuart = uart;
}
uartctl(uart, "b115200 l8 pn r1 s1 i1");
return 0;
}
void
_uartputs(char* s, int n)
{
char *e;
for(e = s+n; s < e; s++){
if(*s == '\n')
i8250putc(&i8250uart[CONSOLE], '\r');
i8250putc(&i8250uart[CONSOLE], *s);
}
}
int
_uartprint(char* fmt, ...)
{
int n;
va_list arg;
char buf[PRINTSIZE];
va_start(arg, fmt);
n = vseprint(buf, buf+sizeof(buf), fmt, arg) - buf;
va_end(arg);
_uartputs(buf, n);
return n;
}