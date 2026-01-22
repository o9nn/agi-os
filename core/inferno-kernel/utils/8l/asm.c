#include	"l.h"
#define	Dbufslop	100
long
entryvalue(void)
{
char *a;
Sym *s;
a = INITENTRY;
if(*a >= '0' && *a <= '9')
return atolwhex(a);
s = lookup(a, 0);
if(s->type == 0)
return INITTEXT;
switch(s->type) {
case STEXT:
break;
case SDATA:
if(dlm)
return s->value+INITDAT;
default:
diag("entry not text: %s", s->name);
}
return s->value;
}
void
wputl(long w)
{
cput(w);
cput(w>>8);
}
void
wput(long w)
{
cput(w>>8);
cput(w);
}
void
lput(long l)
{
cput(l>>24);
cput(l>>16);
cput(l>>8);
cput(l);
}
void
lputl(long l)
{
cput(l);
cput(l>>8);
cput(l>>16);
cput(l>>24);
}
void
llput(vlong v)
{
lput(v>>32);
lput(v);
}
void
llputl(vlong v)
{
lputl(v);
lputl(v>>32);
}
void
strnput(char *s, int n)
{
for(; *s && n > 0; s++){
cput(*s);
n--;
}
while(n > 0){
cput(0);
n--;
}
}
void
asmb(void)
{
Prog *p;
long v, magic;
int a;
uchar *op1;
if(debug['v'])
Bprint(&bso, "%5.2f asmb\n", cputime());
Bflush(&bso);
seek(cout, HEADR, 0);
pc = INITTEXT;
curp = firstp;
for(p = firstp; p != P; p = p->link) {
if(p->as == ATEXT)
curtext = p;
if(p->pc != pc) {
if(!debug['a'])
print("%P\n", curp);
diag("phase error %lux sb %lux in %s", p->pc, pc, TNAME);
pc = p->pc;
}
curp = p;
asmins(p);
if(cbc < sizeof(and))
cflush();
a = (andptr - and);
if(debug['a']) {
Bprint(&bso, pcstr, pc);
for(op1 = and; op1 < andptr; op1++)
Bprint(&bso, "%.2ux", *op1 & 0xff);
Bprint(&bso, "\t%P\n", curp);
}
if(dlm) {
if(p->as == ATEXT)
reloca = nil;
else if(reloca != nil)
diag("reloc failure: %P", curp);
}
memmove(cbp, and, a);
cbp += a;
pc += a;
cbc -= a;
}
cflush();
switch(HEADTYPE) {
default:
diag("unknown header type %ld", HEADTYPE);
case 0:
seek(cout, rnd(HEADR+textsize, 8192), 0);
break;
case 1:
textsize = rnd(HEADR+textsize, 4096)-HEADR;
seek(cout, textsize+HEADR, 0);
break;
case 2:
case 5:
seek(cout, HEADR+textsize, 0);
break;
case 3:
case 4:
seek(cout, HEADR+rnd(textsize, INITRND), 0);
break;
}
if(debug['v'])
Bprint(&bso, "%5.2f datblk\n", cputime());
Bflush(&bso);
if(dlm){
char buf[8];
write(cout, buf, INITDAT-textsize);
textsize = INITDAT;
}
for(v = 0; v < datsize; v += sizeof(buf)-Dbufslop) {
if(datsize-v > sizeof(buf)-Dbufslop)
datblk(v, sizeof(buf)-Dbufslop);
else
datblk(v, datsize-v);
}
symsize = 0;
spsize = 0;
lcsize = 0;
if(!debug['s']) {
if(debug['v'])
Bprint(&bso, "%5.2f sym\n", cputime());
Bflush(&bso);
switch(HEADTYPE) {
default:
case 0:
seek(cout, rnd(HEADR+textsize, 8192)+datsize, 0);
break;
case 1:
seek(cout, rnd(HEADR+textsize, INITRND)+datsize, 0);
break;
case 2:
case 5:
seek(cout, HEADR+textsize+datsize, 0);
break;
case 3:
case 4:
debug['s'] = 1;
break;
}
if(!debug['s'])
asmsym();
if(debug['v'])
Bprint(&bso, "%5.2f sp\n", cputime());
Bflush(&bso);
if(debug['v'])
Bprint(&bso, "%5.2f pc\n", cputime());
Bflush(&bso);
if(!debug['s'])
asmlc();
if(dlm)
asmdyn();
cflush();
}
else if(dlm){
seek(cout, HEADR+textsize+datsize, 0);
asmdyn();
cflush();
}
if(debug['v'])
Bprint(&bso, "%5.2f headr\n", cputime());
Bflush(&bso);
seek(cout, 0L, 0);
switch(HEADTYPE) {
default:
case 0:
lput(0x160L<<16);
lput(0L);
lput(rnd(HEADR+textsize, 4096)+datsize);
lput(symsize);
lput((0x38L<<16)|7L);
lput((0413<<16)|0437L);
lput(rnd(HEADR+textsize, 4096));
lput(datsize);
lput(bsssize);
lput(entryvalue());
lput(INITTEXT-HEADR);
lput(INITDAT);
lput(INITDAT+datsize);
lput(~0L);
lput(0L);
lput(0L);
lput(0L);
lput(0L);
lput(~0L);
break;
case 1:
lputl(0x0004014c);
lputl(0);
lputl(0);
lputl(0);
lputl(0x0003001c);
lputl(0x10b);
lputl(rnd(textsize, INITRND));
lputl(datsize);
lputl(bsssize);
lput(entryvalue());
lputl(INITTEXT);
lputl(INITDAT);
strnput(".text", 8);
lputl(HEADR);
lputl(HEADR);
lputl(textsize);
lputl(HEADR);
lputl(0);
lputl(0);
lputl(0);
lputl(0x20);
strnput(".data", 8);
lputl(INITDAT);
lputl(INITDAT);
lputl(datsize);
lputl(HEADR+textsize);
lputl(0);
lputl(0);
lputl(0);
lputl(0x40);
strnput(".bss", 8);
lputl(INITDAT+datsize);
lputl(INITDAT+datsize);
lputl(bsssize);
lputl(0);
lputl(0);
lputl(0);
lputl(0);
lputl(0x80);
strnput(".comment", 8);
lputl(0);
lputl(0);
lputl(symsize+lcsize);
lputl(HEADR+textsize+datsize);
lputl(HEADR+textsize+datsize);
lputl(HEADR+textsize+datsize+symsize);
lputl(0);
lputl(0x200);
break;
case 2:
magic = 4*11*11+7;
if(dlm)
magic |= 0x80000000;
lput(magic);
lput(textsize);
lput(datsize);
lput(bsssize);
lput(symsize);
lput(entryvalue());
lput(spsize);
lput(lcsize);
break;
case 3:
break;
case 4:
v = rnd(HEADR+textsize, INITRND)+datsize;
wputl(0x5A4D);
wputl(v % 512);
wputl(rnd(v, 512)/512);
wputl(0x0000);
v = rnd(HEADR-(INITTEXT & 0xFFFF), 16);
wputl(v/16);
wputl(0x0000);
wputl(0xFFFF);
wputl(0x0000);
wputl(0x0100);
wputl(0x0000);
v = entryvalue();
wputl(v);
wputl(0x0000);
wputl(0x0000);
wputl(0x0000);
wputl(0x003E);
wputl(0x0000);
break;
case 5:
elf32(I386, ELFDATA2LSB, 0, nil);
break;
}
cflush();
}
void
cflush(void)
{
int n;
n = sizeof(buf.cbuf) - cbc;
if(n)
write(cout, buf.cbuf, n);
cbp = buf.cbuf;
cbc = sizeof(buf.cbuf);
}
void
datblk(long s, long n)
{
Prog *p;
char *cast;
long l, fl, j;
int i, c;
memset(buf.dbuf, 0, n+Dbufslop);
for(p = datap; p != P; p = p->link) {
curp = p;
l = p->from.sym->value + p->from.offset - s;
c = p->from.scale;
i = 0;
if(l < 0) {
if(l+c <= 0)
continue;
while(l < 0) {
l++;
i++;
}
}
if(l >= n)
continue;
if(p->as != AINIT && p->as != ADYNT) {
for(j=l+(c-i)-1; j>=l; j--)
if(buf.dbuf[j]) {
print("%P\n", p);
diag("multiple initialization");
break;
}
}
switch(p->to.type) {
case D_FCONST:
switch(c) {
default:
case 4:
fl = ieeedtof(&p->to.ieee);
cast = (char*)&fl;
if(debug['a'] && i == 0) {
Bprint(&bso, pcstr, l+s+INITDAT);
for(j=0; j<c; j++)
Bprint(&bso, "%.2ux", cast[fnuxi4[j]] & 0xff);
Bprint(&bso, "\t%P\n", curp);
}
for(; i<c; i++) {
buf.dbuf[l] = cast[fnuxi4[i]];
l++;
}
break;
case 8:
cast = (char*)&p->to.ieee;
if(debug['a'] && i == 0) {
Bprint(&bso, pcstr, l+s+INITDAT);
for(j=0; j<c; j++)
Bprint(&bso, "%.2ux", cast[fnuxi8[j]] & 0xff);
Bprint(&bso, "\t%P\n", curp);
}
for(; i<c; i++) {
buf.dbuf[l] = cast[fnuxi8[i]];
l++;
}
break;
}
break;
case D_SCONST:
if(debug['a'] && i == 0) {
Bprint(&bso, pcstr, l+s+INITDAT);
for(j=0; j<c; j++)
Bprint(&bso, "%.2ux", p->to.scon[j] & 0xff);
Bprint(&bso, "\t%P\n", curp);
}
for(; i<c; i++) {
buf.dbuf[l] = p->to.scon[i];
l++;
}
break;
default:
fl = p->to.offset;
if(p->to.type == D_ADDR) {
if(p->to.index != D_STATIC && p->to.index != D_EXTERN)
diag("DADDR type%P", p);
if(p->to.sym) {
if(p->to.sym->type == SUNDEF)
ckoff(p->to.sym, fl);
fl += p->to.sym->value;
if(p->to.sym->type != STEXT && p->to.sym->type != SUNDEF)
fl += INITDAT;
if(dlm)
dynreloc(p->to.sym, l+s+INITDAT, 1);
}
}
cast = (char*)&fl;
switch(c) {
default:
diag("bad nuxi %d %d\n%P", c, i, curp);
break;
case 1:
if(debug['a'] && i == 0) {
Bprint(&bso, pcstr, l+s+INITDAT);
for(j=0; j<c; j++)
Bprint(&bso, "%.2ux", cast[inuxi1[j]] & 0xff);
Bprint(&bso, "\t%P\n", curp);
}
for(; i<c; i++) {
buf.dbuf[l] = cast[inuxi1[i]];
l++;
}
break;
case 2:
if(debug['a'] && i == 0) {
Bprint(&bso, pcstr, l+s+INITDAT);
for(j=0; j<c; j++)
Bprint(&bso, "%.2ux", cast[inuxi2[j]] & 0xff);
Bprint(&bso, "\t%P\n", curp);
}
for(; i<c; i++) {
buf.dbuf[l] = cast[inuxi2[i]];
l++;
}
break;
case 4:
if(debug['a'] && i == 0) {
Bprint(&bso, pcstr, l+s+INITDAT);
for(j=0; j<c; j++)
Bprint(&bso, "%.2ux", cast[inuxi4[j]] & 0xff);
Bprint(&bso, "\t%P\n", curp);
}
for(; i<c; i++) {
buf.dbuf[l] = cast[inuxi4[i]];
l++;
}
break;
}
break;
}
}
write(cout, buf.dbuf, n);
}
long
rnd(long v, long r)
{
long c;
if(r <= 0)
return v;
v += r - 1;
c = v % r;
if(c < 0)
c += r;
v -= c;
return v;
}