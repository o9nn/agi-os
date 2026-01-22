#include	"u.h"
#include	"lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
enum
{
Parity=		1<<0,
Even=		1<<1,
Stop2=		1<<2,
Bits8=		1<<3,
SCE=		1<<4,
RCE=		1<<5,
TCE=		1<<6,
Rena=		1<<0,
Tena=		1<<1,
Break=		1<<2,
Rintena=	1<<3,
Tintena=	1<<4,
Loopback=	1<<5,
DEparity=	1<<8,
DEframe=	1<<9,
DEoverrun=	1<<10,
Tint=		1<<0,
Rint0=		1<<1,
Rint1=		1<<2,
Breakstart=	1<<3,
Breakend=	1<<4,
Fifoerror=	1<<5,
Tbusy=		1<<0,
Rnotempty=	1<<1,
Tnotfull=	1<<2,
ParityError=	1<<3,
FrameError=	1<<4,
Overrun=	1<<5,
};
Uartregs *uart3regs = UART3REGS;
void
serialputs(char *str, int n)
{
Uartregs *ur;
ur = uart3regs;
while(n-- > 0){
while((ur->status[1] & Tnotfull) == 0)
;
ur->data = *str++;
}
while((ur->status[1] & Tbusy))
;
}