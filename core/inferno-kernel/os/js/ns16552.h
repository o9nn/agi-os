enum
{
UartFREQ= 1843200,
TTYABase = 0x2F8
};
#define uartwrreg(u,r,v) outb((u)->port + r, (u)->sticky[r] | (v))
#define uartrdreg(u,r) inb((u)->port + r)
void ns16552setup(ulong, ulong, char*);
static void
uartpower(int, int)
{
}
static void
ns16552intrx(Ureg *ur, void *arg)
{
USED(ur);
ns16552intr((ulong)arg);
}
void
ns16552install(void)
{
static int already;
void uartclock(void);
if(already)
return;
already = 1;
ns16552setup(superiova()+TTYABase, UartFREQ, "eia0");
ns16552special(0, 38400, &kbdq, &printq, kbdputc);
addclock0link(uartclock, 22);
}
char
ns16552dmarcv(int dev)
{
USED(dev);
return -1;
}
long
dmasetup(int,void*,long,int)
{
return 0;
}
void
dmaend(int)
{
}
int
dmacount(int)
{
return 0;
}