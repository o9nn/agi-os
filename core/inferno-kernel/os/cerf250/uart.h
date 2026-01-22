#define	UR(p,r)	((ulong*)(p))[r]
#define uartwr(u,r,v)	(UR(u->regs,r) = (v))
#define uartwrreg(u,r,v)	(UR(u->regs,r)= (u)->sticky[r] | (v))
#define uartrdreg(u,r)		UR(u->regs,r)
extern void	uartsetup(ulong, void*, ulong, char*);
extern void	uartclock(void);
static void
uartportpower(Uart *p, int on)
{
if(on)
p->sticky[Iena] |= Uue;
else
p->sticky[Iena] &= ~Uue;
uartwrreg(p, Iena, 0);
}
static void
uartintrx(Ureg*, void* arg)
{
uartintr(arg);
}
void
uartinstall(void)
{
static int already;
if(already)
return;
already = 1;
uartsetup(0, (void*)PHYSUART0, 0, "eia0");
intrenable(IRQ, IRQffuart, uartintrx, uart[0], "uart0");
uartsetup(2, (void*)PHYSUART2, 0, "eia2");
intrenable(IRQ, IRQhwuart, uartintrx, uart[1], "uart2");
addclock0link(uartclock, 22);
}
char
uartdmarcv(int dev)
{
USED(dev);
return -1;
}
void
uartdebuginit(void)
{
ulong *p;
p = (ulong*)PHYSUART0;
p[Iena] = Uue;
p[Format] = Dra;
p[Dmsb] = 0;
p[Dlsb] = 24;
p[Format] = Bits8;
}
void
uartputc(int c)
{
ulong *p;
if(c == 0)
return;
p = (ulong*)PHYSUART0;
while((UR(p,Lstat) & Outready) == 0){
;
}
UR(p,Data) = c;
if(c == '\n')
while((UR(p,Lstat) & Outready) == 0){
;
}
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