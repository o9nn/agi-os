#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "error.h"
#include "sd.h"
#include "fs.h"
long	biosread(Fs *, void *, long);
vlong	biosseek(Fs *fs, vlong off);
extern SDifc sdbiosifc;
extern int onlybios0, biosinited;
int
biosverify(SDunit* )
{
if (onlybios0 || !biosinited)
return 0;
return 1;
}
int
biosonline(SDunit* unit)
{
if (onlybios0 || !biosinited || !unit)
return 0;
unit->sectors = 1UL << 30;
unit->secsize = 512;
return 1;
}
static int
biosrio(SDreq* r)
{
int nb;
long got;
vlong len, off;
uchar *p;
Fs fs;
if (onlybios0 || !biosinited)
return SDeio;
r->rlen = 0;
r->status = SDok;
switch(r->cmd[0]){
case 0x08:
case 0x28:
if (r->cmd[0] == 0x08)
panic("biosrio: 0x08 read op\n");
off = r->cmd[2]<<24 | r->cmd[3]<<16 | r->cmd[4]<<8 | r->cmd[5];
nb = r->cmd[7]<<8 | r->cmd[8];
USED(nb);
memset(&fs, 0, sizeof fs);
biosseek(&fs, off*512);
got = biosread(&fs, r->data, r->dlen);
if (got < 0)
r->status = SDeio;
else
r->rlen = got;
break;
case 0x0A:
case 0x2A:
r->status = SDeio;
break;
case 0x25:
len = r->unit->sectors - 1;
p = r->data;
*p++ = len>>24;
*p++ = len>>16;
*p++ = len>>8;
*p++ = len;
len = r->unit->secsize;
*p++ = len>>24;
*p++ = len>>16;
*p++ = len>>8;
*p = len;
r->data = (char *)r->data + 8;
return SDok;
case 0x9E:
len = r->unit->sectors - 1;
p = r->data;
*p++ = len>>56;
*p++ = len>>48;
*p++ = len>>40;
*p++ = len>>32;
*p++ = len>>24;
*p++ = len>>16;
*p++ = len>>8;
*p++ = len;
len = r->unit->secsize;
*p++ = len>>24;
*p++ = len>>16;
*p++ = len>>8;
*p = len;
r->data = (char *)r->data + 8;
return SDok;
}
return r->status;
}
SDev*
biosid(SDev* sdev)
{
for (; sdev; sdev = sdev->next)
if (sdev->ifc == &sdbiosifc)
sdev->idno = 'B';
return sdev;
}
static SDev*
biospnp(void)
{
SDev *sdev;
if (pxe || getconf("*nobiosload") != nil || onlybios0 || !biosinited)
return nil;
if((sdev = malloc(sizeof(SDev))) != nil) {
sdev->ifc = &sdbiosifc;
sdev->index = -1;
sdev->nunit = 1;
}
return sdev;
}
SDifc sdbiosifc = {
"bios",
biospnp,
nil,
biosid,
nil,
nil,
biosverify,
biosonline,
biosrio,
nil,
nil,
scsibio,
};