#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
#include "ether8390.h"
enum {
Data = 0x10,
Reset = 0x1F,
};
int
ne2000reset(Ether* ether)
{
ushort buf[16];
ulong port;
Dp8390 *ctlr;
int i;
uchar ea[Eaddrlen];
if(ether->port == 0)
ether->port = 0x300;
if(ether->irq == 0)
ether->irq = 2;
if(ether->mem == 0)
ether->mem = 0x4000;
if(ether->size == 0)
ether->size = 16*1024;
port = ether->port;
ether->ctlr = malloc(sizeof(Dp8390));
ctlr = ether->ctlr;
ctlr->width = 2;
ctlr->ram = 0;
ctlr->port = port;
ctlr->data = port+Data;
ctlr->tstart = HOWMANY(ether->mem, Dp8390BufSz);
ctlr->pstart = ctlr->tstart + HOWMANY(sizeof(Etherpkt), Dp8390BufSz);
ctlr->pstop = ctlr->tstart + HOWMANY(ether->size, Dp8390BufSz);
ctlr->dummyrr = 1;
for(i = 0; i < ether->nopt; i++){
if(strcmp(ether->opt[i], "nodummyrr"))
continue;
ctlr->dummyrr = 0;
break;
}
buf[0] = inb(port+Reset);
delay(2);
outb(port+Reset, buf[0]);
delay(2);
dp8390reset(ether);
memset(buf, 0, sizeof(buf));
dp8390read(ctlr, buf, 0, sizeof(buf));
if((buf[0x0E] & 0xFF) != 0x57 || (buf[0x0F] & 0xFF) != 0x57){
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