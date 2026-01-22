#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
extern PhysUart i8250physuart;
extern PhysUart pciphysuart;
extern void* i8250alloc(int, int, int);
static Uart *perlehead, *perletail;
static Uart*
uartpci(int ctlrno, Pcidev* p, int barno, int n, int freq, char* name,
int iosize)
{
int i, io;
void *ctlr;
char buf[64];
Uart *head, *uart;
io = p->mem[barno].bar & ~0x01;
snprint(buf, sizeof(buf), "%s%d", pciphysuart.name, ctlrno);
if(ioalloc(io, p->mem[barno].size, 0, buf) < 0){
print("uartpci: I/O 0x%uX in use\n", io);
return nil;
}
head = uart = malloc(sizeof(Uart)*n);
if(uart == nil)
error(Enomem);
for(i = 0; i < n; i++){
ctlr = i8250alloc(io, p->intl, p->tbdf);
io += iosize;
if(ctlr == nil)
continue;
uart->regs = ctlr;
snprint(buf, sizeof(buf), "%s.%8.8uX", name, p->tbdf);
kstrdup(&uart->name, buf);
uart->freq = freq;
uart->phys = &i8250physuart;
if(uart != head)
(uart-1)->next = uart;
uart++;
}
if (head) {
if(perlehead != nil)
perletail->next = head;
else
perlehead = head;
for(perletail = head; perletail->next != nil;
perletail = perletail->next)
;
}
return head;
}
static Uart *
ultraport16si(int ctlrno, Pcidev *p, ulong freq)
{
int io, i;
char *name;
Uart *uart;
name = "Ultraport16si";
io = p->mem[4].bar & ~1;
if (ioalloc(io, p->mem[4].size, 0, name) < 0) {
print("uartpci: can't get IO space to set %s to rs-232\n", name);
return nil;
}
for (i = 0; i < 16; i++) {
outb(io, i << 4);
outb(io, (i << 4) + 1);
}
uart = uartpci(ctlrno, p, 2, 8, freq, name, 16);
if(uart)
uart = uartpci(ctlrno, p, 3, 8, freq, name, 16);
return uart;
}
static Uart*
uartpcipnp(void)
{
Pcidev *p;
char *name;
int ctlrno, subid;
ulong freq;
Uart *uart;
perlehead = perletail = nil;
ctlrno = 0;
for(p = pcimatch(nil, 0, 0); p != nil; p = pcimatch(p, 0, 0)){
if(p->ccrb != Pcibccomm || p->ccru > 2 && p->ccru != 0x80)
continue;
switch(p->did<<16 | p->vid){
default:
continue;
case (0x9835<<16)|0x9710:
uart = uartpci(ctlrno, p, 0, 1, 1843200, "PCI2S550-0", 8);
if(uart == nil)
continue;
uart->next = uartpci(ctlrno, p, 1, 1, 1843200,
"PCI2S550-1", 8);
if(uart->next == nil)
continue;
break;
case (0x950A<<16)|0x1415:
case (0x9501<<16)|0x1415:
case (0x9521<<16)|0x1415:
subid = pcicfgr16(p, PciSVID);
subid |= pcicfgr16(p, PciSID)<<16;
switch(subid){
default:
print("oxsemi uart %.8#ux of vid %#ux did %#ux unknown\n",
subid, p->vid, p->did);
continue;
case (0<<16)|0x1415:
uart = uartpci(ctlrno, p, 0, 4, 1843200,
"starport-pex4s", 8);
break;
case (1<<16)|0x1415:
uart = uartpci(ctlrno, p, 0, 2, 14745600,
"starport-pex2s", 8);
break;
case (0x2000<<16)|0x131F:
uart = uartpci(ctlrno, p, 0, 1, 18432000,
"CyberSerial-1S", 8);
break;
}
break;
case (0x9505<<16)|0x1415:
name = "SATAGear-IOI-102";
if (uartpci(ctlrno, p, 0, 1, 14745600, name, 8) != nil)
ctlrno++;
if (uartpci(ctlrno, p, 1, 1, 14745600, name, 8) != nil)
ctlrno++;
uart = nil;
break;
case (0x9050<<16)|0x10B5:
case (0x9030<<16)|0x10B5:
subid = pcicfgr16(p, PciSVID);
subid |= pcicfgr16(p, PciSID)<<16;
freq = 7372800;
switch(subid){
default:
print("uartpci: unknown perle subid %#ux\n",
subid);
continue;
case (0x1588<<16)|0x10B5:
name = "P588UG";
freq = 1843200;
uart = uartpci(ctlrno, p, 2, 8, freq, name, 8);
break;
case (0x0011<<16)|0x12E0:
name = "PCI-Fast16";
uart = uartpci(ctlrno, p, 2, 16, freq, name, 8);
break;
case (0x0021<<16)|0x12E0:
name = "PCI-Fast8";
uart = uartpci(ctlrno, p, 2, 8, freq, name, 8);
break;
case (0x0031<<16)|0x12E0:
name = "PCI-Fast4";
uart = uartpci(ctlrno, p, 2, 4, freq, name, 8);
break;
case (0x0021<<16)|0x155F:
name = "Ultraport8";
uart = uartpci(ctlrno, p, 2, 8, freq, name, 8);
break;
case (0x0041<<16)|0x155F:
name = "Ultraport16";
uart = uartpci(ctlrno, p, 2, 16, 2 * freq,
name, 8);
break;
case (0x0241<<16)|0x155F:
uart = ultraport16si(ctlrno, p, 4 * freq);
break;
}
break;
}
if(uart)
ctlrno++;
}
return perlehead;
}
PhysUart pciphysuart = {
.name		= "UartPCI",
.pnp		= uartpcipnp,
.enable		= nil,
.disable	= nil,
.kick		= nil,
.dobreak	= nil,
.baud		= nil,
.bits		= nil,
.stop		= nil,
.parity		= nil,
.modemctl	= nil,
.rts		= nil,
.dtr		= nil,
.status		= nil,
.fifo		= nil,
};