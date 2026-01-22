#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#include "ether8390.h"
enum {
Data		= 0x10,
Reset		= 0x1F,
};
typedef struct Ec2t {
char*	name;
int	iochecksum;
} Ec2t;
static Ec2t ec2tpcmcia[] = {
{ "EC2T", 0, },
{ "PCMPC100", 1, },
{ "PCM100", 1, },
{ "EN2216", 0, },
{ "FA410TX", 1, },
{ "Network Everywhere", 0, },
{ "10/100 Port Attached", 1, },
{ "8041TX-10/100-PC-Card-V2", 0 },
{ "FA411", 0 },
{ nil, 0, },
};
static int
reset(Ether* ether)
{
ushort buf[16];
ulong port;
Dp8390 *ctlr;
int i, slot;
uchar ea[Eaddrlen], sum, x;
Ec2t *ec2t, tmpec2t;
if(ether->port == 0)
ether->port = 0x300;
if(ether->irq == 0)
ether->irq = 9;
if(ether->mem == 0)
ether->mem = 0x4000;
if(ether->size == 0)
ether->size = 16*1024;
port = ether->port;
if(ioalloc(ether->port, 0x20, 0, "ec2t") < 0)
return -1;
slot = -1;
for(ec2t = ec2tpcmcia; ec2t->name != nil; ec2t++){
if((slot = pcmspecial(ec2t->name, ether)) >= 0)
break;
}
if(ec2t->name == nil){
ec2t = &tmpec2t;
ec2t->name = nil;
ec2t->iochecksum = 0;
for(i = 0; i < ether->nopt; i++){
if(cistrncmp(ether->opt[i], "id=", 3) == 0){
ec2t->name = &ether->opt[i][3];
slot = pcmspecial(ec2t->name, ether);
}
else if(cistrncmp(ether->opt[i], "iochecksum", 10) == 0)
ec2t->iochecksum = 1;
}
}
if(slot < 0){
iofree(port);
return -1;
}
ether->ctlr = malloc(sizeof(Dp8390));
ctlr = ether->ctlr;
ctlr->width = 2;
ctlr->ram = 0;
ctlr->port = port;
ctlr->data = port+Data;
ctlr->tstart = HOWMANY(ether->mem, Dp8390BufSz);
ctlr->pstart = ctlr->tstart + HOWMANY(sizeof(Etherpkt), Dp8390BufSz);
ctlr->pstop = ctlr->tstart + HOWMANY(ether->size, Dp8390BufSz);
ctlr->dummyrr = 0;
for(i = 0; i < ether->nopt; i++){
if(cistrcmp(ether->opt[i], "nodummyrr") == 0)
ctlr->dummyrr = 0;
else if(cistrncmp(ether->opt[i], "dummyrr=", 8) == 0)
ctlr->dummyrr = strtol(&ether->opt[i][8], nil, 0);
}
buf[0] = inb(port+Reset);
delay(2);
outb(port+Reset, buf[0]);
delay(2);
dp8390reset(ether);
sum = 0;
if(ec2t->iochecksum){
for(i = 0; i < 8; i++){
x = inb(port+0x14+i);
sum += x;
buf[i] = (x<<8)|x;
}
}
else{
memset(buf, 0, sizeof(buf));
dp8390read(ctlr, buf, 0, sizeof(buf));
if((buf[0x0E] & 0xFF) == 0x57 && (buf[0x0F] & 0xFF) == 0x57)
sum = 0xFF;
}
if(sum != 0xFF){
pcmspecialclose(slot);
iofree(ether->port);
free(ether->ctlr);
return -1;
}
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0){
for(i = 0; i < sizeof(ether->ea); i++)
ether->ea[i] = buf[i];
}
dp8390setea(ether);
return 0;
}
void
etherec2tlink(void)
{
addethercard("EC2T", reset);
}