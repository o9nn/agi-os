#include <u.h>
#include <libc.h>
#include <bio.h>
#include <libsec.h>
#include "iso9660.h"
char*
jolietstring(uchar *buf, int len)
{
char *p, *q;
int i;
Rune *rp;
rp = emalloc(sizeof(Rune)*(len/2+1));
p = emalloc(UTFmax*(len/2+1));
for(i=0; i<len/2; i++)
rp[i] = (buf[2*i]<<8) | buf[2*i+1];
rp[i] = (Rune)'\0';
snprint(p, UTFmax*(len/2+1), "%S", rp);
q = atom(p);
free(p);
return q;
}
int
isjolietfrog(Rune r)
{
return r==L'*' || r==L'/' || r==L':'
|| r==';' || r=='?' || r=='\\';
}
int
isbadjoliet(char *s)
{
Rune r[256], *p;
if(utflen(s) > 64)
return 1;
strtorune(r, s);
for(p=r; *p; p++)
if(isjolietfrog(*p))
return 1;
return 0;
}
static Rune emptystring[] = { (Rune)0 };
int
jolietcmp(const void *va, const void *vb)
{
int i;
Rune s1[256], s2[256], *b1, *b2, *e1, *e2;
const Direc *a, *b;
a = va;
b = vb;
b1 = strtorune(s1, a->confname);
b2 = strtorune(s2, b->confname);
if((e1 = runechr(b1, (Rune)'.')) != nil)
*e1++ = '\0';
else
e1 = emptystring;
if((e2 = runechr(b2, (Rune)'.')) != nil)
*e2++ = '\0';
else
e2 = emptystring;
if((i = runecmp(b1, b2)) != 0)
return i;
return runecmp(e1, e2);
}
void
Cputjolietsvd(Cdimg *cd, Cdinfo info)
{
Cputc(cd, 2);
Cputs(cd, "CD001", 5);
Cputc(cd, 1);
Cputc(cd, 0);
Cputrscvt(cd, "Joliet Plan 9", 32);
Cputrscvt(cd, info.volumename, 32);
Crepeat(cd, 0, 8);
Cputn(cd, 0, 4);
Cputc(cd, 0x25);
Cputc(cd, 0x2F);
Cputc(cd, 0x43);
Crepeat(cd, 0, 29);
Cputn(cd, 1, 2);
Cputn(cd, 1, 2);
Cputn(cd, Blocksize, 2);
Cputn(cd, 0, 4);
Cputnl(cd, 0, 4);
Cputnl(cd, 0, 4);
Cputnm(cd, 0, 4);
Cputnm(cd, 0, 4);
Cputjolietdir(cd, nil, DTroot, 1, Cwoffset(cd));
Cputrscvt(cd, info.volumeset, 128);
Cputrscvt(cd, info.publisher, 128);
Cputrscvt(cd, info.preparer, 128);
Cputrscvt(cd, info.application, 128);
Cputrscvt(cd, "", 37);
Cputrscvt(cd, "", 37);
Cputrscvt(cd, "", 37);
Cputdate1(cd, now);
Cputdate1(cd, now);
Cputdate1(cd, 0);
Cputdate1(cd, 0);
Cputc(cd, 1);
Cpadblock(cd);
}