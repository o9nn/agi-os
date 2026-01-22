#define UR(p,r) ((uchar*)(p))[r]
#define uartwr(u,r,v) (UR(u->regs,r) = (v))
#define uartwrreg(u,r,v) (UR(u->regs,r)= (u)->sticky[r] | (v))
#define uartrdreg(u,r) UR(u->regs,r)
extern void uartsetup(ulong, void*, ulong, char*);
extern void uartclock(void);
static void
uartportpower(Uart*, int)
{
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
intrenable(VectorUART0, uartintrx, uart[0], BUSUNKNOWN, "uart0");
uartsetup(1, (void*)PHYSUART1, 0, "eia1");
intrenable(VectorUART1, uartintrx, uart[1], BUSUNKNOWN, "uart1");
addclock0link(uartclock, 22);
}
char
uartdmarcv(int dev)
{
USED(dev);
return -1;
}
void
uartputc(int c)
{
uchar *p;
if(c == 0)
return;
p = (uchar*)PHYSUART0;
while((UR(p,Lstat) & Outready) == 0){
;
}
UR(p,Data) = c;
eieio();
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