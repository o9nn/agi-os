#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
typedef struct Uartregs Uartregs;
struct Uartregs {
uchar	rbr;
#define	thr	rbr
#define	dll	rbr
uchar	ier;
#define	dlm	ier
uchar	fcr;
#define	iir	fcr
uchar	lcr;
uchar	mcr;
uchar	lsr;
uchar	msr;
uchar	scr;
};
#define	UARTREGS(n)	((Uartregs*)(PHYSUART0+(n)*0x100))
enum {
Edssi=	1<<3,
Elsi=		1<<2,
Etbei=	1<<1,
Erbfi=	1<<0,
Fci0=	0<<4,
Fci3=	3<<4,
Ipl=		7<<1,
Ip=		1<<0,
Rftl1=	0<<6,
Rftl16=	1<<6,
Rftl32=	2<<6,
Rftl56=	3<<6,
Dms=	1<<3,
Tfr=		1<<2,
Rfr=		1<<1,
Fifoe=	1<<0,
Dlab=	1<<7,
Sb=		1<<6,
Sp=		1<<5,
Eps=		1<<4,
Pen=		1<<3,
Sbs=		1<<2,
Wls=		3<<0,
Afc=		1<<5,
Loop=	1<<4,
Out2=	1<<3,
Out1=	1<<2,
Rts=		1<<1,
Dtr=		1<<0,
Rfe=		1<<7,
Temt=	1<<6,
Thre=	1<<5,
Be=		1<<4,
Fe=		1<<3,
Pe=		1<<2,
Oe=		1<<1,
Dr=		1<<0,
Dcd=	1<<7,
Ri=		1<<6,
Dsr=		1<<5,
Cts=		1<<4,
Ddcd=	1<<3,
Teri=	1<<2,
Ddsr=	1<<1,
Dcts=	1<<0,
};
void (*serwrite)(char*, int) = uartputs;
void
uartinstall(void)
{
}
void
uartspecial(int, int, Queue**, Queue**, int (*)(Queue*, int))
{
}
void
uartputc(int c)
{
Uartregs *r;
if(c == 0)
return;
r = UARTREGS(0);
while((r->lsr & Thre) == 0)
{}
r->thr = c;
if(c == '\n')
while((r->lsr & Thre) == 0)
{}
}
void
uartputs(char *data, int len)
{
int s;
s = splhi();
while(--len >= 0){
if(*data == '\n')
uartputc('\r');
uartputc(*data++);
}
splx(s);
}
void
uartwait(void)
{
}