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
Data = 0x10,
Reset = 0x1F,
};
typedef struct Ctlr Ctlr;
typedef struct Ctlr {
Pcidev* pcidev;
Ctlr* next;
int active;
} Ctlr;
static Ctlr* ctlrhead;
static Ctlr* ctlrtail;
static struct {
char* name;
int id;
} ne2000pci[] = {
{ "Realtek 8029", (0x8029<<16)|0x10EC, },
{ "Winbond 89C940", (0x0940<<16)|0x1050, },
{ nil },
};
static Ctlr*
ne2000match(Ether* edev, int id)
{
int port;
Pcidev *p;
Ctlr *ctlr;
for(ctlr = ctlrhead; ctlr != nil; ctlr = ctlr->next){
if(ctlr->active)
continue;
p = ctlr->pcidev;
if(((p->did<<16)|p->vid) != id)
continue;
port = p->mem[0].bar & ~0x01;
if(edev->port != 0 && edev->port != port)
continue;
edev->port = port;
edev->irq = p->intl;
ctlr->active = 1;
return ctlr;
}
return nil;
}
static void
ne2000pnp(Ether* edev)
{
int i, id;
Pcidev *p;
Ctlr *ctlr;
if(ctlrhead == nil){
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
ctlr = malloc(sizeof(Ctlr));
if(ctlr == nil)
error(Enomem);
ctlr->pcidev = p;
if(ctlrhead != nil)
ctlrtail->next = ctlr;
else
ctlrhead = ctlr;
ctlrtail = ctlr;
}
}
id = 0;
for(i = 0; i < edev->nopt; i++){
if(cistrncmp(edev->opt[i], "id=", 3) == 0)
id = strtol(&edev->opt[i][3], nil, 0);
}
if(id != 0)
ne2000match(edev, id);
else for(i = 0; ne2000pci[i].name; i++){
if(ne2000match(edev, ne2000pci[i].id) != nil)
break;
}
}
static int
ne2000reset(Ether* edev)
{
ushort buf[16];
ulong port;
Dp8390 *dp8390;
int i;
uchar ea[Eaddrlen];
if(edev->port == 0)
ne2000pnp(edev);
if(edev->port == 0)
return -1;
if(edev->irq == 0)
edev->irq = 2;
if(edev->mem == 0)
edev->mem = 0x4000;
if(edev->size == 0)
edev->size = 16*1024;
port = edev->port;
if(ioalloc(edev->port, 0x20, 0, "ne2000") < 0)
return -1;
edev->ctlr = malloc(sizeof(Dp8390));
dp8390 = edev->ctlr;
if(dp8390 == nil)
error(Enomem);
dp8390->width = 2;
dp8390->ram = 0;
dp8390->port = port;
dp8390->data = port+Data;
dp8390->tstart = HOWMANY(edev->mem, Dp8390BufSz);
dp8390->pstart = dp8390->tstart + HOWMANY(sizeof(Etherpkt), Dp8390BufSz);
dp8390->pstop = dp8390->tstart + HOWMANY(edev->size, Dp8390BufSz);
dp8390->dummyrr = 1;
for(i = 0; i < edev->nopt; i++){
if(strcmp(edev->opt[i], "nodummyrr"))
continue;
dp8390->dummyrr = 0;
break;
}
buf[0] = inb(port+Reset);
delay(2);
outb(port+Reset, buf[0]);
delay(2);
dp8390reset(edev);
memset(buf, 0, sizeof(buf));
dp8390read(dp8390, buf, 0, sizeof(buf));
i = buf[0x0E] & 0xFF;
if((i != 0x00 && i != 0x57) || (buf[0x0F] & 0xFF) != 0x57){
iofree(edev->port);
free(edev->ctlr);
return -1;
}
memset(ea, 0, Eaddrlen);
if(memcmp(ea, edev->ea, Eaddrlen) == 0){
for(i = 0; i < sizeof(edev->ea); i++)
edev->ea[i] = buf[i];
}
dp8390setea(edev);
return 0;
}
void
ether2000link(void)
{
addethercard("NE2000", ne2000reset);
}