#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
enum
{
IntelVendID=	0x8086,
Piix4PMID=	0x7113,
SMBbase=	0x90,
SMBconfig=	0xd2,
SMBintrselect=	(7<<1),
SMIenable=	(0<<1),
IRQ9enable=	(4<<1),
SMBenable=	(1<<0),
Hoststatus=	0x0,
Failed=		(1<<4),
Bus_error=	(1<<3),
Dev_error=	(1<<2),
Host_complete=	(1<<1),
Host_busy=	(1<<0),
Slavestatus=	0x1,
Alert_sts=	(1<<5),
Shdw2_sts=	(1<<4),
Shdw1_sts=	(1<<3),
Slv_sts=	(1<<2),
Slv_bsy=	(1<<0),
Hostcontrol=	0x2,
Start=		(1<<6),
Cmd_prot=	(7<<2),
Quick=		(0<<2),
Byte=		(1<<2),
ByteData=	(2<<2),
WordData=	(3<<2),
Kill=		(1<<1),
Ienable=	(1<<0),
Hostcommand=	0x3,
Hostaddress=	0x4,
AddressMask=	(0x7f<<1),
Read=		(1<<0),
Hostdata0=	0x5,
Hostdata1=	0x6,
Blockdata=	0x7,
Slavecontrol=	0x8,
Alert_en=	(1<<3),
Shdw2_en=	(1<<2),
Shdw1_en=	(1<<1),
Slv_en=		(1<<0),
Shadowcommand=	0x9,
Slaveevent=	0xa,
Slavedata=	0xc,
};
static struct
{
int	rw;
int	cmd;
int	len;
int	proto;
} proto[] =
{
[SMBquick]	{ 0,	0,	0,	Quick },
[SMBsend]	{ 0,	1,	0,	Byte },
[SMBbytewrite]	{ 0,	1,	1,	ByteData },
[SMBwordwrite]	{ 0,	1,	2,	WordData },
[SMBrecv]	{ Read,	0,	1, 	Byte },
[SMBbyteread]	{ Read,	1,	1,	ByteData },
[SMBwordread]	{ Read,	1,	2,	WordData },
};
static void
transact(SMBus *s, int type, int addr, int cmd, uchar *data)
{
int tries, status;
char err[256];
if(type < 0 || type > nelem(proto))
panic("piix4smbus: illegal transaction type %d", type);
if(waserror()){
qunlock(s);
nexterror();
}
qlock(s);
for(tries = 0; tries < 1000000; tries++){
if((inb(s->base+Hoststatus) & Host_busy) == 0)
break;
sched();
}
if(tries >= 1000000){
outb(s->base+Hostcontrol, Kill);
for(tries = 0; tries < 1000000; tries++){
if((inb(s->base+Hoststatus) & Host_busy) == 0)
break;
sched();
}
if(tries >= 1000000){
snprint(err, sizeof(err), "SMBus jammed: %2.2ux", inb(s->base+Hoststatus));
error(err);
}
}
outb(s->base+Hostaddress, (addr<<1)|proto[type].rw);
if(proto[type].cmd)
outb(s->base+Hostcommand, cmd);
if(proto[type].rw != Read){
switch(proto[type].len){
case 2:
outb(s->base+Hostdata1, data[1]);
case 1:
outb(s->base+Hostdata0, data[0]);
break;
}
}
outb(s->base+Hoststatus, Failed|Bus_error|Dev_error|Host_complete);
outb(s->base+Hostcontrol, Start|proto[type].proto);
status = 0;
for(tries = 0; tries < 1000000; tries++){
status = inb(s->base+Hoststatus);
if(status & (Failed|Bus_error|Dev_error|Host_complete))
break;
sched();
}
if((status & Host_complete) == 0){
snprint(err, sizeof(err), "SMBus request failed: %2.2ux", status);
error(err);
}
if(proto[type].rw == Read){
switch(proto[type].len){
case 2:
data[1] = inb(s->base+Hostdata1);
case 1:
data[0] = inb(s->base+Hostdata0);
break;
}
}
qunlock(s);
poperror();
}
static SMBus smbusproto =
{
.transact = transact,
};
SMBus*
piix4smbus(void)
{
Pcidev *p;
static SMBus *s;
if(s != nil)
return s;
p = pcimatch(nil, IntelVendID, Piix4PMID);
if(p == nil)
return nil;
s = smalloc(sizeof(*s));
memmove(s, &smbusproto, sizeof(*s));
s->arg = p;
pcicfgw8(p, SMBconfig, IRQ9enable|0);
s->base = pcicfgr32(p, SMBbase) & ~1;
print("SMB base from bios is 0x%lux\n", s->base);
if(ioalloc(s->base, 0xd, 0, "piix4smbus") < 0){
s->base = ioalloc(-1, 0xd, 2, "piix4smbus");
if(s->base < 0){
free(s);
print("piix4smbus: can't allocate io port\n");
return nil;
}
print("SMB base ialloc is 0x%lux\n", s->base);
pcicfgw32(p, SMBbase, s->base|1);
}
outb(s->base+Hostcontrol, Kill);
outb(s->base+Slavecontrol, 0);
pcicfgw8(p, SMBconfig, IRQ9enable|SMBenable);
return s;
}