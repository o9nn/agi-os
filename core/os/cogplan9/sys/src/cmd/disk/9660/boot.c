#include <u.h>
#include <libc.h>
#include <bio.h>
#include <libsec.h>
#include "iso9660.h"
void
Cputbootvol(Cdimg *cd)
{
Cputc(cd, 0x00);
Cputs(cd, "CD001", 5);
Cputc(cd, 0x01);
Cputs(cd, "EL TORITO SPECIFICATION", 2+1+6+1+13);
Crepeat(cd, 0, 2+16+16+7);
cd->bootcatptr = Cwoffset(cd);
Cpadblock(cd);
}
void
Cupdatebootvol(Cdimg *cd)
{
uvlong o;
o = Cwoffset(cd);
Cwseek(cd, cd->bootcatptr);
Cputnl(cd, cd->bootcatblock, 4);
Cwseek(cd, o);
}
void
Cputbootcat(Cdimg *cd)
{
cd->bootcatblock = Cwoffset(cd) / Blocksize;
Cputc(cd, 0x01);
Cputc(cd, 0x00);
Cputc(cd, 0x00);
Cputc(cd, 0x00);
Crepeat(cd, 0, 12+12);
Cputc(cd, 0xAA);
Cputc(cd, 0x55);
Cputc(cd, 0x55);
Cputc(cd, 0xAA);
cd->bootimageptr = Cwoffset(cd);
Cpadblock(cd);
}
enum {
Emusectsz	= 512,
};
void
Cupdatebootcat(Cdimg *cd)
{
uvlong o;
int n;
if(cd->bootdirec == nil)
return;
o = Cwoffset(cd);
Cwseek(cd, cd->bootimageptr);
Cputc(cd, 0x88);
if(cd->flags & CDbootnoemu)
Cputc(cd, 0);
else
switch(cd->bootdirec->length){
default:
fprint(2, "warning: boot image is not 1.44MB or 2.88MB; "
"pretending 1.44MB\n");
case 1440*1024:
Cputc(cd, 0x02);
break;
case 2880*1024:
Cputc(cd, 0x03);
break;
}
Cputnl(cd, 0, 2);
Cputc(cd, 0);
Cputc(cd, 0);
n = 1;
if(cd->flags & CDbootnoemu){
n = (cd->bootdirec->length + Emusectsz - 1) / Emusectsz;
if(n > 4){
fprint(2, "warning: boot image too big; "
"will only load the first 2K\n");
n = 4;
}
}
Cputnl(cd, n, 2);
Cputnl(cd, cd->bootdirec->block, 4);
Cwseek(cd, o);
}
void
Cfillpbs(Cdimg *cd)
{
uvlong o;
int n;
if(cd->bootdirec == nil || cd->loaderdirec == nil)
return;
o = Cwoffset(cd);
Cwseek(cd, 3 + cd->bootdirec->block * Blocksize);
Cputnl(cd, cd->loaderdirec->block, 4);
n = (cd->loaderdirec->length + Blocksize - 1) / Blocksize;
Cputnl(cd, n, 4);
Cputnl(cd, Blocksize, 4);
Cwseek(cd, o);
}
void
findbootimage(Cdimg *cd, Direc *root)
{
Direc *d;
d = walkdirec(root, cd->bootimage);
if(d == nil){
fprint(2, "warning: did not encounter boot image\n");
return;
}
cd->bootdirec = d;
}
void
findloader(Cdimg *cd, Direc *root)
{
Direc *d;
d = walkdirec(root, cd->loader);
if(d == nil){
fprint(2, "warning: did not encounter boot loader\n");
return;
}
cd->loaderdirec = d;
}