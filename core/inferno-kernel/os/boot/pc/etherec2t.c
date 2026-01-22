#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
#include "ether8390.h"
enum {
Data		= 0x10,
Reset		= 0x1F,
};
static char* ec2tpcmcia[] = {
"EC2T",
"PCMPC100",
"EN2216",
"FA410TX",
"Network Everywhere",
nil,
};
int
ec2treset(Ether* ether)
{
ushort buf[16];
ulong port;
Dp8390 *ctlr;
int i, slot;
uchar ea[Eaddrlen], sum, x;
char *type;
if(ether->port == 0)
ether->port = 0x300;
if(ether->irq == 0)
ether->irq = 9;
if(ether->mem == 0)
ether->mem = 0x4000;
if(ether->size == 0)
ether->size = 16*1024;
port = ether->port;
slot = -1;
type = nil;
for(i = 0; ec2tpcmcia[i] != nil; i++){
type = ec2tpcmcia[i];
if((slot = pcmspecial(type, ether)) >= 0)
break;
}
if(ec2tpcmcia[i] == nil){
for(i = 0; i < ether->nopt; i++){
if(cistrncmp(ether->opt[i], "id=", 3))
continue;
type = &ether->opt[i][3];
if((slot = pcmspecial(type, ether)) >= 0)
break;
}
}
if(slot < 0){
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
if(cistrcmp(type, "PCMPC100") == 0 || cistrcmp(type, "FA410TX") == 0){
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