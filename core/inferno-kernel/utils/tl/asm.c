#include "l.h"
long OFFSET;
static Prog *PP;
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
case SLEAF:
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
asmb(void)
{
Prog *p;
long t, etext;
Optab *o;
if(debug['v'])
Bprint(&bso, "%5.2f asm\n", cputime());
Bflush(&bso);
OFFSET = HEADR;
seek(cout, OFFSET, 0);
pc = INITTEXT;
for(p = firstp; p != P; p = p->link) {
setarch(p);
if(p->as == ATEXT) {
curtext = p;
autosize = p->to.offset + 4;
}
if(p->pc != pc) {
diag("phase error %lux sb %lux",
p->pc, pc);
if(!debug['a'])
prasm(curp);
pc = p->pc;
}
curp = p;
o = oplook(p);
if(thumb)
thumbasmout(p, o);
else
asmout(p, o);
pc += o->size;
}
while (pc < INITRODAT) {
cput(0); pc++;
}
Bflush(&bso);
cflush();
etext = INITTEXT + textsize;
for(t = pc; t < etext; t += sizeof(buf)-100) {
if(etext-t > sizeof(buf)-100)
datblk(t, sizeof(buf)-100, 1);
else
datblk(t, etext-t, 1);
}
pc = t;
while(pc-INITTEXT < textsize) {
cput(0);
pc++;
}
if(debug['a'])
Bprint(&bso, "\n");
Bflush(&bso);
cflush();
curtext = P;
switch(HEADTYPE) {
case 0:
case 1:
case 2:
case 5:
OFFSET = HEADR+textsize;
seek(cout, OFFSET, 0);
break;
case 3:
OFFSET = rnd(HEADR+textsize, 4096);
seek(cout, OFFSET, 0);
break;
}
if(dlm){
char buf[8];
write(cout, buf, INITDAT-textsize);
textsize = INITDAT;
}
for(t = 0; t < datsize; t += sizeof(buf)-100) {
if(datsize-t > sizeof(buf)-100)
datblk(t, sizeof(buf)-100, 0);
else
datblk(t, datsize-t, 0);
}
cflush();
symsize = 0;
lcsize = 0;
if(!debug['s']) {
if(debug['v'])
Bprint(&bso, "%5.2f sym\n", cputime());
Bflush(&bso);
switch(HEADTYPE) {
case 0:
case 1:
case 4:
case 5:
debug['s'] = 1;
break;
case 2:
OFFSET = HEADR+textsize+datsize;
seek(cout, OFFSET, 0);
break;
case 3:
OFFSET += rnd(datsize, 4096);
seek(cout, OFFSET, 0);
break;
}
if(!debug['s'])
asmsym();
if(debug['v'])
Bprint(&bso, "%5.2f pc\n", cputime());
Bflush(&bso);
if(!debug['s'])
asmlc();
if(!debug['s'])
asmthumbmap();
if(dlm)
asmdyn();
cflush();
}
else if(dlm){
seek(cout, HEADR+textsize+datsize, 0);
asmdyn();
cflush();
}
curtext = P;
if(debug['v'])
Bprint(&bso, "%5.2f header\n", cputime());
Bflush(&bso);
OFFSET = 0;
seek(cout, OFFSET, 0);
switch(HEADTYPE) {
case 0:
break;
case 1:
lputl(0xe1a00000);
lputl(0xe1a00000);
lputl(0xeb000000 + 12);
lputl(0xeb000000 +
(entryvalue()
- INITTEXT
+ HEADR
- 12
- 8) / 4);
lputl(0xef000011);
lputl(textsize+HEADR);
lputl(datsize);
lputl(0);
lputl(bsssize);
lputl(0);
lputl(INITTEXT-HEADR);
lputl(0);
lputl(32);
lputl(0);
for(t=0; t<2; t++)
lputl(0);
for(t=0; t<15; t++)
lputl(0xe1a00000);
lputl(0xe1a0f00e);
break;
case 2:
if(dlm)
lput(0x80000000|0x647);
else
lput(0x647);
lput(textsize);
lput(datsize);
lput(bsssize);
lput(symsize);
lput(entryvalue());
lput(0L);
lput(lcsize);
break;
case 3:
lput((143<<16)|0413);
lputl(rnd(HEADR+textsize, 4096));
lputl(rnd(datsize, 4096));
lputl(bsssize);
lputl(symsize);
lputl(entryvalue());
lputl(0L);
lputl(0L);
break;
case 4:
break;
case 5:
lputl(0xe3300000);
lputl(0xe3300000);
lputl(0xe3300000);
lputl(0xe3300000);
break;
}
cflush();
if(debug['c']){
print("textsize=%ld\n", textsize);
print("datsize=%ld\n", datsize);
print("bsssize=%ld\n", bsssize);
print("symsize=%ld\n", symsize);
print("lcsize=%ld\n", lcsize);
print("total=%ld\n", textsize+datsize+bsssize+symsize+lcsize);
}
}
void
strnput(char *s, int n)
{
for(; *s; s++){
cput(*s);
n--;
}
for(; n > 0; n--)
cput(0);
}
void
cput(int c)
{
cbp[0] = c;
cbp++;
cbc--;
if(cbc <= 0)
cflush();
}
void
wput(long l)
{
cbp[0] = l>>8;
cbp[1] = l;
cbp += 2;
cbc -= 2;
if(cbc <= 0)
cflush();
}
void
hput(long l)
{
cbp[0] = l>>8;
cbp[1] = l;
cbp += 2;
cbc -= 2;
if(cbc <= 0)
cflush();
}
void
lput(long l)
{
cbp[0] = l>>24;
cbp[1] = l>>16;
cbp[2] = l>>8;
cbp[3] = l;
cbp += 4;
cbc -= 4;
if(cbc <= 0)
cflush();
}
void
lputl(long l)
{
cbp[3] = l>>24;
cbp[2] = l>>16;
cbp[1] = l>>8;
cbp[0] = l;
cbp += 4;
cbc -= 4;
if(cbc <= 0)
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
nopstat(char *f, Count *c)
{
if(c->outof)
Bprint(&bso, "%s delay %ld/%ld (%.2f)\n", f,
c->outof - c->count, c->outof,
(double)(c->outof - c->count)/c->outof);
}
void
asmsym(void)
{
Prog *p;
Auto *a;
Sym *s;
int h;
s = lookup("etext", 0);
if(s->type == STEXT)
putsymb(s->name, 'T', s->value, s->version);
for(h=0; h<NHASH; h++)
for(s=hash[h]; s!=S; s=s->link)
switch(s->type) {
case SCONST:
putsymb(s->name, 'D', s->value, s->version);
continue;
case SDATA:
putsymb(s->name, 'D', s->value+INITDAT, s->version);
continue;
case SBSS:
putsymb(s->name, 'B', s->value+INITDAT, s->version);
continue;
case SSTRING:
putsymb(s->name, 'T', s->value, s->version);
continue;
case SFILE:
putsymb(s->name, 'f', s->value, s->version);
continue;
}
for(p=textp; p!=P; p=p->cond) {
s = p->from.sym;
if(s->type != STEXT && s->type != SLEAF)
continue;
for(a=p->to.autom; a; a=a->link)
if(a->type == D_FILE)
putsymb(a->asym->name, 'z', a->aoffset, 0);
else
if(a->type == D_FILE1)
putsymb(a->asym->name, 'Z', a->aoffset, 0);
if(s->type == STEXT)
putsymb(s->name, 'T', s->value, s->version);
else
putsymb(s->name, 'L', s->value, s->version);
putsymb(".frame", 'm', p->to.offset+4, 0);
for(a=p->to.autom; a; a=a->link)
if(a->type == D_AUTO)
putsymb(a->asym->name, 'a', -a->aoffset, 0);
else
if(a->type == D_PARAM)
putsymb(a->asym->name, 'p', a->aoffset, 0);
}
if(debug['v'] || debug['n'])
Bprint(&bso, "symsize = %lud\n", symsize);
Bflush(&bso);
}
void
putsymb(char *s, int t, long v, int ver)
{
int i, f;
if(t == 'f')
s++;
lput(v);
if(ver)
t += 'a' - 'A';
cput(t+0x80);
if(t == 'Z' || t == 'z') {
cput(s[0]);
for(i=1; s[i] != 0 || s[i+1] != 0; i += 2) {
cput(s[i]);
cput(s[i+1]);
}
cput(0);
cput(0);
i++;
}
else {
for(i=0; s[i]; i++)
cput(s[i]);
cput(0);
}
symsize += 4 + 1 + i + 1;
if(debug['n']) {
if(t == 'z' || t == 'Z') {
Bprint(&bso, "%c %.8lux ", t, v);
for(i=1; s[i] != 0 || s[i+1] != 0; i+=2) {
f = ((s[i]&0xff) << 8) | (s[i+1]&0xff);
Bprint(&bso, "/%x", f);
}
Bprint(&bso, "\n");
return;
}
if(ver)
Bprint(&bso, "%c %.8lux %s<%d>\n", t, v, s, ver);
else
Bprint(&bso, "%c %.8lux %s\n", t, v, s);
}
}
#define MINLC 4
void
asmlc(void)
{
long oldpc, oldlc;
Prog *p;
long v, s;
oldpc = INITTEXT;
oldlc = 0;
for(p = firstp; p != P; p = p->link) {
setarch(p);
if(p->line == oldlc || p->as == ATEXT || p->as == ANOP) {
if(p->as == ATEXT)
curtext = p;
if(debug['L'])
Bprint(&bso, "%6lux %P\n",
p->pc, p);
continue;
}
if(debug['L'])
Bprint(&bso, "\t\t%6ld", lcsize);
v = (p->pc - oldpc) / MINLC;
while(v) {
s = 127;
if(v < 127)
s = v;
cput(s+128);
if(debug['L'])
Bprint(&bso, " pc+%ld*%d(%ld)", s, MINLC, s+128);
v -= s;
lcsize++;
}
s = p->line - oldlc;
oldlc = p->line;
oldpc = p->pc + MINLC;
if(s > 64 || s < -64) {
cput(0);
cput(s>>24);
cput(s>>16);
cput(s>>8);
cput(s);
if(debug['L']) {
if(s > 0)
Bprint(&bso, " lc+%ld(%d,%ld)\n",
s, 0, s);
else
Bprint(&bso, " lc%ld(%d,%ld)\n",
s, 0, s);
Bprint(&bso, "%6lux %P\n",
p->pc, p);
}
lcsize += 5;
continue;
}
if(s > 0) {
cput(0+s);
if(debug['L']) {
Bprint(&bso, " lc+%ld(%ld)\n", s, 0+s);
Bprint(&bso, "%6lux %P\n",
p->pc, p);
}
} else {
cput(64-s);
if(debug['L']) {
Bprint(&bso, " lc%ld(%ld)\n", s, 64-s);
Bprint(&bso, "%6lux %P\n",
p->pc, p);
}
}
lcsize++;
}
while(lcsize & 1) {
s = 129;
cput(s);
lcsize++;
}
if(debug['v'] || debug['L'])
Bprint(&bso, "lcsize = %ld\n", lcsize);
Bflush(&bso);
}
static void
outt(long f, long l)
{
if(debug['L'])
Bprint(&bso, "tmap: %lux-%lux\n", f, l);
lput(f);
lput(l);
}
void
asmthumbmap(void)
{
long pc, lastt;
Prog *p;
if(!seenthumb)
return;
pc = 0;
lastt = -1;
for(p = firstp; p != P; p = p->link){
pc = p->pc - INITTEXT;
if(p->as == ATEXT){
setarch(p);
if(thumb){
if(p->from.sym->foreign){
if(lastt >= 0){
outt(lastt, pc-1);
lastt = -1;
}
pc += 8;
}
if(lastt < 0)
lastt = pc;
}
else{
if(p->from.sym->foreign){
if(lastt < 0)
lastt = pc;
pc += 4;
}
if(lastt >= 0){
outt(lastt, pc-1);
lastt = -1;
}
}
}
}
if(lastt >= 0)
outt(lastt, pc+1);
}
void
datblk(long s, long n, int str)
{
Sym *v;
Prog *p;
char *cast;
long a, l, fl, j, d;
int i, c;
memset(buf.dbuf, 0, n+100);
for(p = datap; p != P; p = p->link) {
if(str != (p->from.sym->type == SSTRING))
continue;
curp = p;
a = p->from.sym->value + p->from.offset;
l = a - s;
c = p->reg;
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
default:
diag("unknown mode in initialization%P", p);
break;
case D_FCONST:
switch(c) {
default:
case 4:
fl = ieeedtof(p->to.ieee);
cast = (char*)&fl;
for(; i<c; i++) {
buf.dbuf[l] = cast[fnuxi4[i]];
l++;
}
break;
case 8:
cast = (char*)p->to.ieee;
for(; i<c; i++) {
buf.dbuf[l] = cast[fnuxi8[i]];
l++;
}
break;
}
break;
case D_SCONST:
for(; i<c; i++) {
buf.dbuf[l] = p->to.sval[i];
l++;
}
break;
case D_CONST:
d = p->to.offset;
v = p->to.sym;
if(v) {
switch(v->type) {
case SUNDEF:
ckoff(v, d);
d += v->value;
break;
case STEXT:
case SLEAF:
d += v->value;
#ifdef CALLEEBX
d += fnpinc(v);
#else
if(v->thumb)
d++;
#endif
break;
case SSTRING:
d += v->value;
break;
case SDATA:
case SBSS:
d += v->value + INITDAT;
break;
}
if(dlm)
dynreloc(v, a+INITDAT, 1);
}
cast = (char*)&d;
switch(c) {
default:
diag("bad nuxi %d %d%P", c, i, curp);
break;
case 1:
for(; i<c; i++) {
buf.dbuf[l] = cast[inuxi1[i]];
l++;
}
break;
case 2:
for(; i<c; i++) {
buf.dbuf[l] = cast[inuxi2[i]];
l++;
}
break;
case 4:
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
void
asmout(Prog *p, Optab *o)
{
long o1, o2, o3, o4, o5, o6, v;
int r, rf, rt, rt2;
Sym *s;
PP = p;
o1 = 0;
o2 = 0;
o3 = 0;
o4 = 0;
o5 = 0;
o6 = 0;
armsize += o->size;
if(debug['P']) print("%ulx: %P	type %d\n", (ulong)(p->pc), p, o->type);
switch(o->type) {
default:
diag("unknown asm %d", o->type);
prasm(p);
break;
case 0:
if(debug['G']) print("%ulx: %s: arm %d %d %d %d\n", (ulong)(p->pc), p->from.sym->name, p->from.sym->thumb, p->from.sym->foreign, p->from.sym->fnptr, p->from.sym->used);
break;
case 1:
o1 = oprrr(p->as, p->scond);
rf = p->from.reg;
rt = p->to.reg;
r = p->reg;
if(p->to.type == D_NONE)
rt = 0;
if(p->as == AMOVW || p->as == AMVN)
r = 0;
else if(r == NREG)
r = rt;
o1 |= rf | (r<<16) | (rt<<12);
break;
case 2:
aclass(&p->from);
o1 = oprrr(p->as, p->scond);
o1 |= immrot(instoffset);
rt = p->to.reg;
r = p->reg;
if(p->to.type == D_NONE)
rt = 0;
if(p->as == AMOVW || p->as == AMVN)
r = 0;
else if(r == NREG)
r = rt;
o1 |= (r<<16) | (rt<<12);
break;
case 3:
mov:
aclass(&p->from);
o1 = oprrr(p->as, p->scond);
o1 |= p->from.offset;
rt = p->to.reg;
r = p->reg;
if(p->to.type == D_NONE)
rt = 0;
if(p->as == AMOVW || p->as == AMVN)
r = 0;
else if(r == NREG)
r = rt;
o1 |= (r<<16) | (rt<<12);
break;
case 4:
aclass(&p->from);
o1 = oprrr(AADD, p->scond);
o1 |= immrot(instoffset);
r = p->from.reg;
if(r == NREG)
r = o->param;
o1 |= r << 16;
o1 |= p->to.reg << 12;
break;
case 5:
v = -8;
if(p->cond == UP) {
s = p->to.sym;
if(s->type != SUNDEF)
diag("bad branch sym type");
v = (ulong)s->value >> (Roffset-2);
dynreloc(s, p->pc, 0);
}
else if(p->cond != P)
v = (p->cond->pc - pc) - 8;
#ifdef CALLEEBX
if(p->as == ABL)
v += fninc(p->to.sym);
#endif
o1 = opbra(p->as, p->scond);
o1 |= (v >> 2) & 0xffffff;
break;
case 6:
aclass(&p->to);
o1 = oprrr(AADD, p->scond);
o1 |= immrot(instoffset);
o1 |= p->to.reg << 16;
o1 |= REGPC << 12;
break;
case 7:
aclass(&p->to);
o1 = oprrr(AADD, p->scond);
o1 |= immrot(0);
o1 |= REGPC << 16;
o1 |= REGLINK << 12;
o2 = oprrr(AADD, p->scond);
o2 |= immrot(instoffset);
o2 |= p->to.reg << 16;
o2 |= REGPC << 12;
break;
case 8:
aclass(&p->from);
o1 = oprrr(p->as, p->scond);
r = p->reg;
if(r == NREG)
r = p->to.reg;
o1 |= r;
o1 |= (instoffset&31) << 7;
o1 |= p->to.reg << 12;
break;
case 9:
o1 = oprrr(p->as, p->scond);
r = p->reg;
if(r == NREG)
r = p->to.reg;
o1 |= r;
o1 |= (p->from.reg << 8) | (1<<4);
o1 |= p->to.reg << 12;
break;
case 10:
o1 = oprrr(p->as, p->scond);
if(p->to.type != D_NONE) {
aclass(&p->to);
o1 |= instoffset & 0xffffff;
}
break;
case 11:
switch(aclass(&p->to)) {
case C_LCON:
if(!dlm)
break;
if(p->to.name != D_EXTERN && p->to.name != D_STATIC)
break;
case C_ADDR:
if(p->to.sym->type == SUNDEF)
ckoff(p->to.sym, p->to.offset);
dynreloc(p->to.sym, p->pc, 1);
}
o1 = instoffset;
break;
case 12:
o1 = omvl(p, &p->from, p->to.reg);
break;
case 13:
o1 = omvl(p, &p->from, REGTMP);
if(!o1)
break;
o2 = oprrr(p->as, p->scond);
o2 |= REGTMP;
r = p->reg;
if(p->as == AMOVW || p->as == AMVN)
r = 0;
else if(r == NREG)
r = p->to.reg;
o2 |= r << 16;
if(p->to.type != D_NONE)
o2 |= p->to.reg << 12;
break;
case 14:
o1 = oprrr(ASLL, p->scond);
if(p->as == AMOVBU || p->as == AMOVHU)
o2 = oprrr(ASRL, p->scond);
else
o2 = oprrr(ASRA, p->scond);
r = p->to.reg;
o1 |= (p->from.reg)|(r<<12);
o2 |= (r)|(r<<12);
if(p->as == AMOVB || p->as == AMOVBU) {
o1 |= (24<<7);
o2 |= (24<<7);
} else {
o1 |= (16<<7);
o2 |= (16<<7);
}
break;
case 15:
o1 = oprrr(p->as, p->scond);
rf = p->from.reg;
rt = p->to.reg;
r = p->reg;
if(r == NREG)
r = rt;
if(rt == r) {
r = rf;
rf = rt;
}
if(0)
if(rt == r || rf == REGPC || r == REGPC || rt == REGPC) {
diag("bad registers in MUL");
prasm(p);
}
o1 |= (rf<<8) | r | (rt<<16);
break;
case 16:
o1 = 0xf << 28;
o2 = 0;
break;
case 17:
o1 = oprrr(p->as, p->scond);
rf = p->from.reg;
rt = p->to.reg;
rt2 = p->to.offset;
r = p->reg;
o1 |= (rf<<8) | r | (rt<<16) | (rt2<<12);
break;
case 20:
aclass(&p->to);
r = p->to.reg;
if(r == NREG)
r = o->param;
o1 = osr(p->as, p->from.reg, instoffset, r, p->scond);
break;
case 21:
aclass(&p->from);
r = p->from.reg;
if(r == NREG)
r = o->param;
o1 = olr(instoffset, r, p->to.reg, p->scond);
if(p->as != AMOVW)
o1 |= 1<<22;
break;
case 22:
aclass(&p->from);
r = p->from.reg;
if(r == NREG)
r = o->param;
o1 = olr(instoffset, r, p->to.reg, p->scond);
o2 = oprrr(ASLL, p->scond);
o3 = oprrr(ASRA, p->scond);
r = p->to.reg;
if(p->as == AMOVB) {
o2 |= (24<<7)|(r)|(r<<12);
o3 |= (24<<7)|(r)|(r<<12);
} else {
o2 |= (16<<7)|(r)|(r<<12);
if(p->as == AMOVHU)
o3 = oprrr(ASRL, p->scond);
o3 |= (16<<7)|(r)|(r<<12);
}
break;
case 23:
aclass(&p->to);
r = p->to.reg;
if(r == NREG)
r = o->param;
o1 = osr(AMOVH, p->from.reg, instoffset, r, p->scond);
o2 = oprrr(ASRL, p->scond);
o2 |= (8<<7)|(p->from.reg)|(REGTMP<<12);
o3 = osr(AMOVH, REGTMP, instoffset+1, r, p->scond);
break;
case 30:
o1 = omvl(p, &p->to, REGTMP);
if(!o1)
break;
r = p->to.reg;
if(r == NREG)
r = o->param;
o2 = osrr(p->from.reg, REGTMP,r, p->scond);
if(p->as != AMOVW)
o2 |= 1<<22;
break;
case 31:
case 32:
o1 = omvl(p, &p->from, REGTMP);
if(!o1)
break;
r = p->from.reg;
if(r == NREG)
r = o->param;
o2 = olrr(REGTMP,r, p->to.reg, p->scond);
if(p->as == AMOVBU || p->as == AMOVB)
o2 |= 1<<22;
if(o->type == 31)
break;
o3 = oprrr(ASLL, p->scond);
if(p->as == AMOVBU || p->as == AMOVHU)
o4 = oprrr(ASRL, p->scond);
else
o4 = oprrr(ASRA, p->scond);
r = p->to.reg;
o3 |= (r)|(r<<12);
o4 |= (r)|(r<<12);
if(p->as == AMOVB || p->as == AMOVBU) {
o3 |= (24<<7);
o4 |= (24<<7);
} else {
o3 |= (16<<7);
o4 |= (16<<7);
}
break;
case 33:
o1 = omvl(p, &p->to, REGTMP);
if(!o1)
break;
r = p->to.reg;
if(r == NREG)
r = o->param;
o2 = osrr(p->from.reg, REGTMP, r, p->scond);
o2 |= (1<<22) ;
o3 = oprrr(ASRL, p->scond);
o3 |= (8<<7)|(p->from.reg)|(p->from.reg<<12);
o3 |= (1<<6);
o4 = oprrr(AADD, p->scond);
o4 |= (REGTMP << 12) | (REGTMP << 16);
o4 |= immrot(1);
o5 = osrr(p->from.reg, REGTMP,r,p->scond);
o5 |= (1<<22);
o6 = oprrr(ASRL, p->scond);
o6 |= (24<<7)|(p->from.reg)|(p->from.reg<<12);
o6 |= (1<<6);
break;
case 34:
o1 = omvl(p, &p->from, REGTMP);
if(!o1)
break;
o2 = oprrr(AADD, p->scond);
o2 |= REGTMP;
r = p->from.reg;
if(r == NREG)
r = o->param;
o2 |= r << 16;
if(p->to.type != D_NONE)
o2 |= p->to.reg << 12;
break;
case 35:
o1 = (2<<23) | (0xf<<16) | (0<<0);
o1 |= (p->scond & C_SCOND) << 28;
o1 |= (p->from.reg & 1) << 22;
o1 |= p->to.reg << 12;
break;
case 36:
o1 = (2<<23) | (0x29f<<12) | (0<<4);
if(p->scond & C_FBIT)
o1 ^= 0x010 << 12;
o1 |= (p->scond & C_SCOND) << 28;
o1 |= (p->to.reg & 1) << 22;
o1 |= p->from.reg << 0;
break;
case 37:
aclass(&p->from);
o1 = (2<<23) | (0x29f<<12) | (0<<4);
if(p->scond & C_FBIT)
o1 ^= 0x010 << 12;
o1 |= (p->scond & C_SCOND) << 28;
o1 |= immrot(instoffset);
o1 |= (p->to.reg & 1) << 22;
o1 |= p->from.reg << 0;
break;
case 38:
o1 = (0x4 << 25);
o1 |= p->from.offset & 0xffff;
o1 |= p->to.reg << 16;
aclass(&p->to);
goto movm;
case 39:
o1 = (0x4 << 25) | (1 << 20);
o1 |= p->to.offset & 0xffff;
o1 |= p->from.reg << 16;
aclass(&p->from);
movm:
if(instoffset != 0)
diag("offset must be zero in MOVM");
o1 |= (p->scond & C_SCOND) << 28;
if(p->scond & C_PBIT)
o1 |= 1 << 24;
if(p->scond & C_UBIT)
o1 |= 1 << 23;
if(p->scond & C_SBIT)
o1 |= 1 << 22;
if(p->scond & C_WBIT)
o1 |= 1 << 21;
break;
case 40:
aclass(&p->from);
if(instoffset != 0)
diag("offset must be zero in SWP");
o1 = (0x2<<23) | (0x9<<4);
if(p->as != ASWPW)
o1 |= 1 << 22;
o1 |= p->from.reg << 16;
o1 |= p->reg << 0;
o1 |= p->to.reg << 12;
o1 |= (p->scond & C_SCOND) << 28;
break;
case 41:
o1 = 0xe8fd8000;
break;
case 50:
v = regoff(&p->to);
r = p->to.reg;
if(r == NREG)
r = o->param;
o1 = ofsr(p->as, p->from.reg, v, r, p->scond, p);
break;
case 51:
v = regoff(&p->from);
r = p->from.reg;
if(r == NREG)
r = o->param;
o1 = ofsr(p->as, p->to.reg, v, r, p->scond, p) | (1<<20);
break;
case 52:
o1 = omvl(p, &p->to, REGTMP);
if(!o1)
break;
r = p->to.reg;
if(r == NREG)
r = o->param;
o2 = oprrr(AADD, p->scond) | (REGTMP << 12) | (REGTMP << 16) | r;
o3 = ofsr(p->as, p->from.reg, 0, REGTMP, p->scond, p);
break;
case 53:
o1 = omvl(p, &p->from, REGTMP);
if(!o1)
break;
r = p->from.reg;
if(r == NREG)
r = o->param;
o2 = oprrr(AADD, p->scond) | (REGTMP << 12) | (REGTMP << 16) | r;
o3 = ofsr(p->as, p->to.reg, 0, REGTMP, p->scond, p) | (1<<20);
break;
case 54:
o1 = oprrr(p->as, p->scond);
if(p->from.type == D_FCONST) {
rf = chipfloat(p->from.ieee);
if(rf < 0){
diag("invalid floating-point immediate\n%P", p);
rf = 0;
}
rf |= (1<<3);
} else
rf = p->from.reg;
rt = p->to.reg;
r = p->reg;
if(p->to.type == D_NONE)
rt = 0;
else if(o1 & (1<<15))
r = 0;
else if(r == NREG)
r = rt;
o1 |= rf | (r<<16) | (rt<<12);
break;
case 55:
o1 = oprrr(p->as, p->scond);
rf = p->from.reg;
rt = p->to.reg;
if(p->to.type == D_NONE){
rt = 0;
diag("to.type==D_NONE (asm/fp)");
}
if(p->from.type == D_REG)
o1 |= (rf<<12) | (rt<<16);
else
o1 |= rf | (rt<<12);
break;
case 56:
o1 = ((p->scond & C_SCOND) << 28) | (0xe << 24) | (1<<8) | (1<<4);
o1 |= ((p->to.reg+1)<<21) | (p->from.reg << 12);
break;
case 57:
o1 = ((p->scond & C_SCOND) << 28) | (0xe << 24) | (1<<8) | (1<<4);
o1 |= ((p->from.reg+1)<<21) | (p->to.reg<<12) | (1<<20);
break;
case 58:
o1 = oprrr(AAND, p->scond);
o1 |= immrot(0xff);
rt = p->to.reg;
r = p->from.reg;
if(p->to.type == D_NONE)
rt = 0;
if(r == NREG)
r = rt;
o1 |= (r<<16) | (rt<<12);
break;
case 59:
if(p->from.reg == NREG) {
if(p->as != AMOVW)
diag("byte MOV from shifter operand");
goto mov;
}
if(p->from.offset&(1<<4))
diag("bad shift in LDR");
o1 = olrr(p->from.offset, p->from.reg, p->to.reg, p->scond);
if(p->as == AMOVBU)
o1 |= 1<<22;
break;
case 60:
if(p->from.reg == NREG) {
diag("byte MOV from shifter operand");
goto mov;
}
if(p->from.offset&(~0xf))
diag("bad shift in LDRSB");
o1 = olhrr(p->from.offset, p->from.reg, p->to.reg, p->scond);
o1 ^= (1<<5)|(1<<6);
break;
case 61:
if(p->to.reg == NREG)
diag("MOV to shifter operand");
o1 = osrr(p->from.reg, p->to.offset, p->to.reg, p->scond);
if(p->as == AMOVB || p->as == AMOVBU)
o1 |= 1<<22;
break;
case 62:
o1 = olrr(p->from.reg, REGPC, REGPC, p->scond);
o1 |= 2<<7;
break;
case 63:
if(p->cond != P) {
o1 = p->cond->pc;
if(dlm)
dynreloc(S, p->pc, 1);
}
break;
case 64:
o1 = omvl(p, &p->to, REGTMP);
if(!o1)
break;
o2 = osr(p->as, p->from.reg, 0, REGTMP, p->scond);
break;
case 65:
case 66:
o1 = omvl(p, &p->from, REGTMP);
if(!o1)
break;
o2 = olr(0, REGTMP, p->to.reg, p->scond);
if(p->as == AMOVBU || p->as == AMOVB)
o2 |= 1<<22;
if(o->type == 65)
break;
o3 = oprrr(ASLL, p->scond);
if(p->as == AMOVBU || p->as == AMOVHU)
o4 = oprrr(ASRL, p->scond);
else
o4 = oprrr(ASRA, p->scond);
r = p->to.reg;
o3 |= (r)|(r<<12);
o4 |= (r)|(r<<12);
if(p->as == AMOVB || p->as == AMOVBU) {
o3 |= (24<<7);
o4 |= (24<<7);
} else {
o3 |= (16<<7);
o4 |= (16<<7);
}
break;
case 67:
o1 = omvl(p, &p->to, REGTMP);
if(!o1)
break;
o2 = osr(p->as, p->from.reg, 0, REGTMP, p->scond);
o3 = oprrr(ASRL, p->scond);
o3 |= (8<<7)|(p->from.reg)|(p->from.reg<<12);
o3 |= (1<<6);
o4 = oprrr(AADD, p->scond);
o4 |= (REGTMP << 12) | (REGTMP << 16);
o4 |= immrot(1);
o5 = osr(p->as, p->from.reg, 0, REGTMP, p->scond);
o6 = oprrr(ASRL, p->scond);
o6 |= (24<<7)|(p->from.reg)|(p->from.reg<<12);
o6 |= (1<<6);
break;
case 68:
o1 = omvl(p, &p->to, REGTMP);
if(!o1)
break;
o2 = ofsr(p->as, p->from.reg, 0, REGTMP, p->scond, p);
break;
case 69:
o1 = omvl(p, &p->from, REGTMP);
if(!o1)
break;
o2 = ofsr(p->as, p->to.reg, 0, REGTMP, p->scond, p) | (1<<20);
break;
case 70:
aclass(&p->to);
r = p->to.reg;
if(r == NREG)
r = o->param;
o1 = oshr(p->from.reg, instoffset, r, p->scond);
break;
case 71:
aclass(&p->from);
r = p->from.reg;
if(r == NREG)
r = o->param;
o1 = olhr(instoffset, r, p->to.reg, p->scond);
if(p->as == AMOVB)
o1 ^= (1<<5)|(1<<6);
else if(p->as == AMOVH)
o1 ^= (1<<6);
break;
case 72:
o1 = omvl(p, &p->to, REGTMP);
if(!o1)
break;
r = p->to.reg;
if(r == NREG)
r = o->param;
o2 = oshrr(p->from.reg, REGTMP,r, p->scond);
break;
case 73:
o1 = omvl(p, &p->from, REGTMP);
if(!o1)
break;
r = p->from.reg;
if(r == NREG)
r = o->param;
o2 = olhrr(REGTMP, r, p->to.reg, p->scond);
if(p->as == AMOVB)
o2 ^= (1<<5)|(1<<6);
else if(p->as == AMOVH)
o2 ^= (1<<6);
break;
case 74:
#ifdef CALLEEBX
diag("bx $i case (arm)");
#endif
if(!seenthumb)
diag("ABX $I and seenthumb==0");
v = p->cond->pc;
if(p->to.sym->thumb)
v |= 1;
o1 = olr(8, REGPC, REGTMP, p->scond&C_SCOND);
o2 = oprrr(AADD, p->scond) | immrot(8) | (REGPC<<16) | (REGLINK<<12);
o3 = ((p->scond&C_SCOND)<<28) | (0x12fff<<8) | (1<<4) | REGTMP;
o4 = opbra(AB, 14);
o5 = v;
break;
case 75:
aclass(&p->to);
if(instoffset != 0)
diag("non-zero offset in ABX");
o1 = oprrr(AADD, p->scond);
o1 |= immrot(instoffset);
o1 |= p->to.reg << 16;
o1 |= REGTMP << 12;
o2 = oprrr(AADD, p->scond) | immrot(0) | (REGPC<<16) | (REGLINK<<12);
o3 = ((p->scond&C_SCOND)<<28) | (0x12fff<<8) | (1<<4) | REGTMP;
break;
case 76:
if(!seenthumb)
diag("ABXRET and seenthumb==0");
aclass(&p->to);
if(instoffset != 0)
diag("non-zero offset in ABXRET");
o1 = ((p->scond&C_SCOND)<<28) | (0x12fff<<8) | (1<<4) | p->to.reg;
break;
}
v = p->pc;
switch(o->size) {
default:
if(debug['a'])
Bprint(&bso, " %.8lux:\t\t%P\n", v, p);
break;
case 4:
if(debug['a'])
Bprint(&bso, " %.8lux: %.8lux\t%P\n", v, o1, p);
lputl(o1);
break;
case 8:
if(debug['a'])
Bprint(&bso, " %.8lux: %.8lux %.8lux%P\n", v, o1, o2, p);
lputl(o1);
lputl(o2);
break;
case 12:
if(debug['a'])
Bprint(&bso, " %.8lux: %.8lux %.8lux %.8lux%P\n", v, o1, o2, o3, p);
lputl(o1);
lputl(o2);
lputl(o3);
break;
case 16:
if(debug['a'])
Bprint(&bso, " %.8lux: %.8lux %.8lux %.8lux %.8lux%P\n",
v, o1, o2, o3, o4, p);
lputl(o1);
lputl(o2);
lputl(o3);
lputl(o4);
break;
case 20:
if(debug['a'])
Bprint(&bso, " %.8lux: %.8lux %.8lux %.8lux %.8lux %.8lux%P\n",
v, o1, o2, o3, o4, o5, p);
lputl(o1);
lputl(o2);
lputl(o3);
lputl(o4);
lputl(o5);
break;
case 24:
if(debug['a'])
Bprint(&bso, " %.8lux: %.8lux %.8lux %.8lux %.8lux %.8lux %.8lux%P\n",
v, o1, o2, o3, o4, o5, o6, p);
lputl(o1);
lputl(o2);
lputl(o3);
lputl(o4);
lputl(o5);
lputl(o6);
break;
}
}
long
oprrr(int a, int sc)
{
long o;
o = (sc & C_SCOND) << 28;
if(sc & C_SBIT)
o |= 1 << 20;
if(sc & (C_PBIT|C_WBIT))
diag(".P/.W on dp instruction");
switch(a) {
case AMULU:
case AMUL: return o | (0x0<<21) | (0x9<<4);
case AMULA: return o | (0x1<<21) | (0x9<<4);
case AMULLU: return o | (0x4<<21) | (0x9<<4);
case AMULL: return o | (0x6<<21) | (0x9<<4);
case AMULALU: return o | (0x5<<21) | (0x9<<4);
case AMULAL: return o | (0x7<<21) | (0x9<<4);
case AAND: return o | (0x0<<21);
case AEOR: return o | (0x1<<21);
case ASUB: return o | (0x2<<21);
case ARSB: return o | (0x3<<21);
case AADD: return o | (0x4<<21);
case AADC: return o | (0x5<<21);
case ASBC: return o | (0x6<<21);
case ARSC: return o | (0x7<<21);
case ATST: return o | (0x8<<21) | (1<<20);
case ATEQ: return o | (0x9<<21) | (1<<20);
case ACMP: return o | (0xa<<21) | (1<<20);
case ACMN: return o | (0xb<<21) | (1<<20);
case AORR: return o | (0xc<<21);
case AMOVW: return o | (0xd<<21);
case ABIC: return o | (0xe<<21);
case AMVN: return o | (0xf<<21);
case ASLL: return o | (0xd<<21) | (0<<5);
case ASRL: return o | (0xd<<21) | (1<<5);
case ASRA: return o | (0xd<<21) | (2<<5);
case ASWI: return o | (0xf<<24);
case AADDD: return o | (0xe<<24) | (0x0<<20) | (1<<8) | (1<<7);
case AADDF: return o | (0xe<<24) | (0x0<<20) | (1<<8);
case AMULD: return o | (0xe<<24) | (0x1<<20) | (1<<8) | (1<<7);
case AMULF: return o | (0xe<<24) | (0x1<<20) | (1<<8);
case ASUBD: return o | (0xe<<24) | (0x2<<20) | (1<<8) | (1<<7);
case ASUBF: return o | (0xe<<24) | (0x2<<20) | (1<<8);
case ADIVD: return o | (0xe<<24) | (0x4<<20) | (1<<8) | (1<<7);
case ADIVF: return o | (0xe<<24) | (0x4<<20) | (1<<8);
case ACMPD:
case ACMPF: return o | (0xe<<24) | (0x9<<20) | (0xF<<12) | (1<<8) | (1<<4);
case AMOVF:
case AMOVDF: return o | (0xe<<24) | (0x0<<20) | (1<<15) | (1<<8);
case AMOVD:
case AMOVFD: return o | (0xe<<24) | (0x0<<20) | (1<<15) | (1<<8) | (1<<7);
case AMOVWF: return o | (0xe<<24) | (0<<20) | (1<<8) | (1<<4);
case AMOVWD: return o | (0xe<<24) | (0<<20) | (1<<8) | (1<<4) | (1<<7);
case AMOVFW: return o | (0xe<<24) | (1<<20) | (1<<8) | (1<<4);
case AMOVDW: return o | (0xe<<24) | (1<<20) | (1<<8) | (1<<4) | (1<<7);
}
diag("bad rrr %d", a);
prasm(curp);
return 0;
}
long
opbra(int a, int sc)
{
if(sc & (C_SBIT|C_PBIT|C_WBIT))
diag(".S/.P/.W on bra instruction");
sc &= C_SCOND;
if(a == ABL)
return (sc<<28)|(0x5<<25)|(0x1<<24);
if(sc != 0xe)
diag(".COND on bcond instruction");
switch(a) {
case ABEQ: return (0x0<<28)|(0x5<<25);
case ABNE: return (0x1<<28)|(0x5<<25);
case ABCS: return (0x2<<28)|(0x5<<25);
case ABHS: return (0x2<<28)|(0x5<<25);
case ABCC: return (0x3<<28)|(0x5<<25);
case ABLO: return (0x3<<28)|(0x5<<25);
case ABMI: return (0x4<<28)|(0x5<<25);
case ABPL: return (0x5<<28)|(0x5<<25);
case ABVS: return (0x6<<28)|(0x5<<25);
case ABVC: return (0x7<<28)|(0x5<<25);
case ABHI: return (0x8<<28)|(0x5<<25);
case ABLS: return (0x9<<28)|(0x5<<25);
case ABGE: return (0xa<<28)|(0x5<<25);
case ABLT: return (0xb<<28)|(0x5<<25);
case ABGT: return (0xc<<28)|(0x5<<25);
case ABLE: return (0xd<<28)|(0x5<<25);
case AB: return (0xe<<28)|(0x5<<25);
}
diag("bad bra %A", a);
prasm(curp);
return 0;
}
long
olr(long v, int b, int r, int sc)
{
long o;
if(sc & C_SBIT)
diag(".S on LDR/STR instruction");
o = (sc & C_SCOND) << 28;
if(!(sc & C_PBIT))
o |= 1 << 24;
if(!(sc & C_UBIT))
o |= 1 << 23;
if(sc & C_WBIT)
o |= 1 << 21;
o |= (0x1<<26) | (1<<20);
if(v < 0) {
v = -v;
o ^= 1 << 23;
}
if(v >= (1<<12))
diag("literal span too large: %d (R%d)\n%P", v, b, PP);
o |= v;
o |= b << 16;
o |= r << 12;
return o;
}
long
olhr(long v, int b, int r, int sc)
{
long o;
if(sc & C_SBIT)
diag(".S on LDRH/STRH instruction");
o = (sc & C_SCOND) << 28;
if(!(sc & C_PBIT))
o |= 1 << 24;
if(sc & C_WBIT)
o |= 1 << 21;
o |= (1<<23) | (1<<20)|(0xb<<4);
if(v < 0) {
v = -v;
o ^= 1 << 23;
}
if(v >= (1<<8))
diag("literal span too large: %d (R%d)\n%P", v, b, PP);
o |= (v&0xf)|((v>>4)<<8)|(1<<22);
o |= b << 16;
o |= r << 12;
return o;
}
long
osr(int a, int r, long v, int b, int sc)
{
long o;
o = olr(v, b, r, sc) ^ (1<<20);
if(a != AMOVW)
o |= 1<<22;
return o;
}
long
oshr(int r, long v, int b, int sc)
{
long o;
o = olhr(v, b, r, sc) ^ (1<<20);
return o;
}
long
osrr(int r, int i, int b, int sc)
{
return olr(i, b, r, sc) ^ ((1<<25) | (1<<20));
}
long
oshrr(int r, int i, int b, int sc)
{
return olhr(i, b, r, sc) ^ ((1<<22) | (1<<20));
}
long
olrr(int i, int b, int r, int sc)
{
return olr(i, b, r, sc) ^ (1<<25);
}
long
olhrr(int i, int b, int r, int sc)
{
return olhr(i, b, r, sc) ^ (1<<22);
}
long
ofsr(int a, int r, long v, int b, int sc, Prog *p)
{
long o;
if(sc & C_SBIT)
diag(".S on FLDR/FSTR instruction");
o = (sc & C_SCOND) << 28;
if(!(sc & C_PBIT))
o |= 1 << 24;
if(sc & C_WBIT)
o |= 1 << 21;
o |= (6<<25) | (1<<24) | (1<<23);
if(v < 0) {
v = -v;
o ^= 1 << 23;
}
if(v & 3)
diag("odd offset for floating point op: %d\n%P", v, p);
else if(v >= (1<<10))
diag("literal span too large: %d\n%P", v, p);
o |= (v>>2) & 0xFF;
o |= b << 16;
o |= r << 12;
o |= 1 << 8;
switch(a) {
default:
diag("bad fst %A", a);
case AMOVD:
o |= 1<<15;
case AMOVF:
break;
}
return o;
}
long
omvl(Prog *p, Adr *a, int dr)
{
long v, o1;
if(!p->cond) {
aclass(a);
v = immrot(~instoffset);
if(v == 0) {
diag("missing literal");
prasm(p);
return 0;
}
o1 = oprrr(AMVN, p->scond&C_SCOND);
o1 |= v;
o1 |= dr << 12;
} else {
v = p->cond->pc - p->pc - 8;
o1 = olr(v, REGPC, dr, p->scond&C_SCOND);
}
return o1;
}
static Ieee chipfloats[] = {
{0x00000000, 0x00000000},
{0x00000000, 0x3ff00000},
{0x00000000, 0x40000000},
{0x00000000, 0x40080000},
{0x00000000, 0x40100000},
{0x00000000, 0x40140000},
{0x00000000, 0x3fe00000},
{0x00000000, 0x40240000},
};
int
chipfloat(Ieee *e)
{
Ieee *p;
int n;
for(n = sizeof(chipfloats)/sizeof(chipfloats[0]); --n >= 0;){
p = &chipfloats[n];
if(p->l == e->l && p->h == e->h)
return n;
}
return -1;
}