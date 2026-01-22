#include <u.h>
#include <libc.h>
#include <bio.h>
#include <mach.h>
#define HUGEINT 0x7fffffff
#define NNAME 20
typedef struct txtsym Txtsym;
typedef struct file File;
typedef struct hist Hist;
struct txtsym {
int n;
Sym **locals;
Sym *sym;
};
struct hist {
char *name;
long line;
long offset;
};
struct file {
uvlong addr;
union {
Txtsym *txt;
Sym *sym;
};
int n;
Hist *hist;
};
static int debug = 0;
static Sym **autos;
static File *files;
static int fmax;
static Sym **fnames;
static Sym **globals;
static Hist *hist;
static int isbuilt;
static long nauto;
static long nfiles;
static long nglob;
static long nhist;
static long nsym;
static int ntxt;
static uchar *pcline;
static uchar *pclineend;
static uchar *spoff;
static uchar *spoffend;
static Sym *symbols;
static Txtsym *txt;
static uvlong txtstart;
static uvlong txtend;
static void cleansyms(void);
static long decodename(Biobuf*, Sym*);
static short *encfname(char*);
static int fline(char*, int, long, Hist*, Hist**);
static void fillsym(Sym*, Symbol*);
static int findglobal(char*, Symbol*);
static int findlocvar(Symbol*, char *, Symbol*);
static int findtext(char*, Symbol*);
static int hcomp(Hist*, short*);
static int hline(File*, short*, long*);
static void printhist(char*, Hist*, int);
static int buildtbls(void);
static int symcomp(void*, void*);
static int symerrmsg(int, char*);
static int txtcomp(void*, void*);
static int filecomp(void*, void*);
int
syminit(int fd, Fhdr *fp)
{
Sym *p;
long i, l, size;
vlong vl;
Biobuf b;
int svalsz;
if(fp->symsz == 0)
return 0;
if(fp->type == FNONE)
return 0;
cleansyms();
textseg(fp->txtaddr, fp);
symbols = malloc((fp->symsz/(4+1+2)+1)*sizeof(Sym));
if(symbols == 0) {
werrstr("can't malloc %ld bytes", fp->symsz);
return -1;
}
Binit(&b, fd, OREAD);
Bseek(&b, fp->symoff, 0);
nsym = 0;
size = 0;
if((fp->_magic && (fp->magic & HDR_MAGIC)) || mach->szaddr == 8)
svalsz = 8;
else
svalsz = 4;
for(p = symbols; size < fp->symsz; p++, nsym++) {
if(svalsz == 8){
if(Bread(&b, &vl, 8) != 8)
return symerrmsg(8, "symbol");
p->value = beswav(vl);
}
else{
if(Bread(&b, &l, 4) != 4)
return symerrmsg(4, "symbol");
p->value = (u32int)beswal(l);
}
if(Bread(&b, &p->type, sizeof(p->type)) != sizeof(p->type))
return symerrmsg(sizeof(p->value), "symbol");
i = decodename(&b, p);
if(i < 0)
return -1;
size += i+svalsz+sizeof(p->type);
switch (p->type) {
case 'l':
case 'L':
case 't':
case 'T':
ntxt++;
break;
case 'd':
case 'D':
case 'b':
case 'B':
nglob++;
break;
case 'f':
if(strcmp(p->name, ".frame") == 0) {
p->type = 'm';
nauto++;
}
else if(p->value > fmax)
fmax = p->value;
break;
case 'a':
case 'p':
case 'm':
nauto++;
break;
case 'z':
if(p->value == 1) {
nhist++;
nfiles++;
}
nhist++;
break;
default:
break;
}
}
if (debug)
print("NG: %ld NT: %d NF: %d\n", nglob, ntxt, fmax);
if (fp->sppcsz) {
spoff = (uchar *)malloc(fp->sppcsz);
if(spoff == 0) {
werrstr("can't malloc %ld bytes", fp->sppcsz);
return -1;
}
Bseek(&b, fp->sppcoff, 0);
if(Bread(&b, spoff, fp->sppcsz) != fp->sppcsz){
spoff = 0;
return symerrmsg(fp->sppcsz, "sp-pc");
}
spoffend = spoff+fp->sppcsz;
}
if (fp->lnpcsz) {
pcline = (uchar *)malloc(fp->lnpcsz);
if(pcline == 0) {
werrstr("can't malloc %ld bytes", fp->lnpcsz);
return -1;
}
Bseek(&b, fp->lnpcoff, 0);
if(Bread(&b, pcline, fp->lnpcsz) != fp->lnpcsz){
pcline = 0;
return symerrmsg(fp->lnpcsz, "pc-line");
}
pclineend = pcline+fp->lnpcsz;
}
return nsym;
}
static int
symerrmsg(int n, char *table)
{
werrstr("can't read %d bytes of %s table", n, table);
return -1;
}
static long
decodename(Biobuf *bp, Sym *p)
{
char *cp;
int c1, c2;
long n;
vlong o;
if((p->type & 0x80) == 0) {
p->name = malloc(NNAME);
if(p->name == 0) {
werrstr("can't malloc %d bytes", NNAME);
return -1;
}
if(Bread(bp, p->name, NNAME) != NNAME)
return symerrmsg(NNAME, "symbol");
Bseek(bp, 3, 1);
return NNAME+3;
}
p->type &= ~0x80;
if(p->type == 'z' || p->type == 'Z') {
o = Bseek(bp, 0, 1);
if(Bgetc(bp) < 0) {
werrstr("can't read symbol name");
return -1;
}
for(;;) {
c1 = Bgetc(bp);
c2 = Bgetc(bp);
if(c1 < 0 || c2 < 0) {
werrstr("can't read symbol name");
return -1;
}
if(c1 == 0 && c2 == 0)
break;
}
n = Bseek(bp, 0, 1)-o;
p->name = malloc(n);
if(p->name == 0) {
werrstr("can't malloc %ld bytes", n);
return -1;
}
Bseek(bp, -n, 1);
if(Bread(bp, p->name, n) != n) {
werrstr("can't read %ld bytes of symbol name", n);
return -1;
}
} else {
cp = Brdline(bp, '\0');
if(cp == 0) {
werrstr("can't read symbol name");
return -1;
}
n = Blinelen(bp);
p->name = malloc(n);
if(p->name == 0) {
werrstr("can't malloc %ld bytes", n);
return -1;
}
strcpy(p->name, cp);
}
return n;
}
static void
cleansyms(void)
{
if(globals)
free(globals);
globals = 0;
nglob = 0;
if(txt)
free(txt);
txt = 0;
ntxt = 0;
if(fnames)
free(fnames);
fnames = 0;
fmax = 0;
if(files)
free(files);
files = 0;
nfiles = 0;
if(hist)
free(hist);
hist = 0;
nhist = 0;
if(autos)
free(autos);
autos = 0;
nauto = 0;
isbuilt = 0;
if(symbols)
free(symbols);
symbols = 0;
nsym = 0;
if(spoff)
free(spoff);
spoff = 0;
if(pcline)
free(pcline);
pcline = 0;
}
void
textseg(uvlong base, Fhdr *fp)
{
txtstart = base;
txtend = base+fp->txtsz;
}
Sym *
symbase(long *n)
{
*n = nsym;
return symbols;
}
Sym *
getsym(int index)
{
if(index >= 0 && index < nsym)
return &symbols[index];
return 0;
}
static int
buildtbls(void)
{
long i;
int j, nh, ng, nt;
File *f;
Txtsym *tp;
Hist *hp;
Sym *p, **ap;
if(isbuilt)
return 1;
isbuilt = 1;
if(nglob) {
globals = malloc(nglob*sizeof(*globals));
if(!globals) {
werrstr("can't malloc global symbol table");
return 0;
}
}
if(ntxt) {
txt = malloc(ntxt*sizeof(*txt));
if (!txt) {
werrstr("can't malloc text symbol table");
return 0;
}
}
fnames = malloc((fmax+1)*sizeof(*fnames));
if (!fnames) {
werrstr("can't malloc file name table");
return 0;
}
memset(fnames, 0, (fmax+1)*sizeof(*fnames));
files = malloc(nfiles*sizeof(*files));
if(!files) {
werrstr("can't malloc file table");
return 0;
}
hist = malloc(nhist*sizeof(Hist));
if(hist == 0) {
werrstr("can't malloc history stack");
return 0;
}
autos = malloc(nauto*sizeof(Sym*));
if(autos == 0) {
werrstr("can't malloc auto symbol table");
return 0;
}
ng = nt = nh = 0;
f = 0;
tp = 0;
i = nsym;
hp = hist;
ap = autos;
for(p = symbols; i-- > 0; p++) {
switch(p->type) {
case 'D':
case 'd':
case 'B':
case 'b':
if(debug)
print("Global: %s %llux\n", p->name, p->value);
globals[ng++] = p;
break;
case 'z':
if(p->value == 1) {
if(f) {
f->n = nh;
f->hist[nh].name = 0;
hp += nh+1;
f++;
}
else
f = files;
f->hist = hp;
f->sym = 0;
f->addr = 0;
nh = 0;
}
f->hist[nh].name = p->name;
f->hist[nh].line = p->value;
f->hist[nh].offset = 0;
if(debug)
printhist("-> ", &f->hist[nh], 1);
nh++;
break;
case 'Z':
if(f && nh > 0)
f->hist[nh-1].offset = p->value;
break;
case 'T':
case 't':
case 'L':
case 'l':
tp = &txt[nt++];
tp->n = 0;
tp->sym = p;
tp->locals = ap;
if(debug)
print("TEXT: %s at %llux\n", p->name, p->value);
if(f && !f->sym) {
f->sym = p;
f->addr = p->value;
}
break;
case 'a':
case 'p':
case 'm':
if(!tp)
print("Warning: Free floating local var: %s\n",
p->name);
else {
if(debug)
print("Local: %s %llux\n", p->name, p->value);
tp->locals[tp->n] = p;
tp->n++;
ap++;
}
break;
case 'f':
if(debug)
print("Fname: %s\n", p->name);
fnames[p->value] = p;
break;
default:
break;
}
}
qsort(globals, nglob, sizeof(Sym*), symcomp);
qsort(txt, ntxt, sizeof(Txtsym), txtcomp);
qsort(files, nfiles, sizeof(File), filecomp);
tp = txt;
for(i = 0, f = files; i < nfiles; i++, f++) {
for(j = 0; j < ntxt; j++) {
if(f->sym == tp->sym) {
if(debug) {
print("LINK: %s to at %llux", f->sym->name, f->addr);
printhist("... ", f->hist, 1);
}
f->txt = tp++;
break;
}
if(++tp >= txt+ntxt)
tp = txt;
}
}
return 1;
}
int
lookup(char *fn, char *var, Symbol *s)
{
int found;
if(buildtbls() == 0)
return 0;
if(fn) {
found = findtext(fn, s);
if(var == 0)
return found;
else if(!found)
return 0;
} else if(var) {
found = findtext(var, s);
if(found)
return 1;
} else return 0;
if(found)
return findlocal(s, var, s);
return findglobal(var, s);
}
static int
findtext(char *name, Symbol *s)
{
int i;
for(i = 0; i < ntxt; i++) {
if(strcmp(txt[i].sym->name, name) == 0) {
fillsym(txt[i].sym, s);
s->handle = (void *) &txt[i];
s->index = i;
return 1;
}
}
return 0;
}
static int
findglobal(char *name, Symbol *s)
{
long i;
for(i = 0; i < nglob; i++) {
if(strcmp(globals[i]->name, name) == 0) {
fillsym(globals[i], s);
s->index = i;
return 1;
}
}
return 0;
}
int
findlocal(Symbol *s1, char *name, Symbol *s2)
{
if(s1 == 0)
return 0;
if(buildtbls() == 0)
return 0;
return findlocvar(s1, name, s2);
}
static int
findlocvar(Symbol *s1, char *name, Symbol *s2)
{
Txtsym *tp;
int i;
tp = (Txtsym *)s1->handle;
if(tp && tp->locals) {
for(i = 0; i < tp->n; i++)
if (strcmp(tp->locals[i]->name, name) == 0) {
fillsym(tp->locals[i], s2);
s2->handle = (void *)tp;
s2->index = tp->n-1 - i;
return 1;
}
}
return 0;
}
int
textsym(Symbol *s, int index)
{
if(buildtbls() == 0)
return 0;
if(index < 0 || index >= ntxt)
return 0;
fillsym(txt[index].sym, s);
s->handle = (void *)&txt[index];
s->index = index;
return 1;
}
int
filesym(int index, char *buf, int n)
{
Hist *hp;
if(buildtbls() == 0)
return 0;
if(index < 0 || index >= nfiles)
return 0;
hp = files[index].hist;
if(!hp || !hp->name)
return 0;
return fileelem(fnames, (uchar*)hp->name, buf, n);
}
int
getauto(Symbol *s1, int off, int type, Symbol *s2)
{
Txtsym *tp;
Sym *p;
int i, t;
if(s1 == 0)
return 0;
if(type == CPARAM)
t = 'p';
else if(type == CAUTO)
t = 'a';
else
return 0;
if(buildtbls() == 0)
return 0;
tp = (Txtsym *)s1->handle;
if(tp == 0)
return 0;
for(i = 0; i < tp->n; i++) {
p = tp->locals[i];
if(p->type == t && p->value == off) {
fillsym(p, s2);
s2->handle = s1->handle;
s2->index = tp->n-1 - i;
return 1;
}
}
return 0;
}
static int
srchtext(uvlong addr)
{
uvlong val;
int top, bot, mid;
Sym *sp;
val = addr;
bot = 0;
top = ntxt;
for (mid = (bot+top)/2; mid < top; mid = (bot+top)/2) {
sp = txt[mid].sym;
if(sp == nil)
return -1;
if(val < sp->value)
top = mid;
else if(mid != ntxt-1 && val >= txt[mid+1].sym->value)
bot = mid;
else
return mid;
}
return -1;
}
static int
srchdata(uvlong addr)
{
uvlong val;
int top, bot, mid;
Sym *sp;
bot = 0;
top = nglob;
val = addr;
for(mid = (bot+top)/2; mid < top; mid = (bot+top)/2) {
sp = globals[mid];
if(sp == nil)
return -1;
if(val < sp->value)
top = mid;
else if(mid < nglob-1 && val >= globals[mid+1]->value)
bot = mid;
else
return mid;
}
return -1;
}
int
findsym(uvlong val, int type, Symbol *s)
{
int i;
if(buildtbls() == 0)
return 0;
if(type == CTEXT || type == CANY) {
i = srchtext(val);
if(i >= 0) {
if(type == CTEXT || i != ntxt-1) {
fillsym(txt[i].sym, s);
s->handle = (void *) &txt[i];
s->index = i;
return 1;
}
}
}
if(type == CDATA || type == CANY) {
i = srchdata(val);
if(i >= 0) {
fillsym(globals[i], s);
s->index = i;
return 1;
}
}
return 0;
}
int
fnbound(uvlong addr, uvlong *bounds)
{
int i;
if(buildtbls() == 0)
return 0;
i = srchtext(addr);
if(0 <= i && i < ntxt-1) {
bounds[0] = txt[i].sym->value;
bounds[1] = txt[i+1].sym->value;
return 1;
}
return 0;
}
int
localsym(Symbol *s, int index)
{
Txtsym *tp;
if(s == 0 || index < 0)
return 0;
if(buildtbls() == 0)
return 0;
tp = (Txtsym *)s->handle;
if(tp && tp->locals && index < tp->n) {
fillsym(tp->locals[tp->n-index-1], s);
s->handle = (void *)tp;
s->index = index;
return 1;
}
return 0;
}
int
globalsym(Symbol *s, int index)
{
if(s == 0)
return 0;
if(buildtbls() == 0)
return 0;
if(index >=0 && index < nglob) {
fillsym(globals[index], s);
s->index = index;
return 1;
}
return 0;
}
uvlong
file2pc(char *file, long line)
{
File *fp;
long i;
uvlong pc, start, end;
short *name;
if(buildtbls() == 0 || files == 0)
return ~0;
name = encfname(file);
if(name == 0) {
werrstr("file %s not found", file);
return ~0;
}
for(i = 0, fp = files; i < nfiles; i++, fp++)
if (hline(fp, name, &line))
break;
free(name);
if(i >= nfiles) {
werrstr("line %ld in file %s not found", line, file);
return ~0;
}
start = fp->addr;
if(i < nfiles-1)
end = (fp+1)->addr;
else
end = 0;
if(debug)
print("find pc for %ld - between: %llux and %llux\n", line, start, end);
pc = line2addr(line, start, end);
if(pc == ~0) {
werrstr("line %ld not in file %s", line, file);
return ~0;
}
return pc;
}
static int
pathcomp(char *s, int n)
{
int i;
for(i = 0; i <= fmax; i++)
if(fnames[i] && strncmp(s, fnames[i]->name, n) == 0)
return i;
return -1;
}
static short*
encfname(char *file)
{
int i, j;
char *cp, *cp2;
short *dest;
if(*file == '/')
cp2 = file+1;
else {
cp2 = strchr(file, '/');
if(!cp2)
cp2 = strchr(file, 0);
}
cp = file;
dest = 0;
for(i = 0; *cp; i++) {
j = pathcomp(cp, cp2-cp);
if(j < 0)
return 0;
dest = realloc(dest, (i+1)*sizeof(short));
dest[i] = j;
cp = cp2;
while(*cp == '/')
cp++;
cp2 = strchr(cp, '/');
if(!cp2)
cp2 = strchr(cp, 0);
}
dest = realloc(dest, (i+1)*sizeof(short));
dest[i] = 0;
return dest;
}
static int
hline(File *fp, short *name, long *line)
{
Hist *hp;
int offset, depth;
long ln;
for(hp = fp->hist; hp->name; hp++)
if(hp->name[1] || hp->name[2]) {
if(hcomp(hp, name))
break;
}
if(!hp->name)
return 0;
if(debug)
printhist("hline found ... ", hp, 1);
ln = *line;
offset = hp->line-1;
depth = 1;
for(hp++; depth && hp->name; hp++) {
if(debug)
printhist("hline inspect ... ", hp, 1);
if(hp->name[1] || hp->name[2]) {
if(hp->offset){
offset = 0;
if(hcomp(hp, name)) {
if(*line <= hp->offset)
break;
ln = *line+hp->line-hp->offset;
depth = 1;
} else
depth = 2;
} else if(depth == 1 && ln < hp->line-offset)
break;
else if(depth++ == 1)
offset -= hp->line;
} else if(--depth == 1)
offset += hp->line;
}
*line = ln+offset;
return 1;
}
static int
hcomp(Hist *hp, short *sp)
{
uchar *cp;
int i, j;
short *s;
cp = (uchar *)hp->name;
s = sp;
if (*s == 0)
return 0;
for (i = 1; j = (cp[i]<<8)|cp[i+1]; i += 2) {
if(j == 0)
break;
if(*s == j)
s++;
else
s = sp;
}
return *s == 0;
}
long
fileline(char *str, int n, uvlong dot)
{
long line, top, bot, mid;
File *f;
*str = 0;
if(buildtbls() == 0)
return 0;
bot = 0;
top = nfiles;
for (mid = (bot+top)/2; mid < top; mid = (bot+top)/2) {
f = &files[mid];
if(dot < f->addr)
top = mid;
else if(mid < nfiles-1 && dot >= (f+1)->addr)
bot = mid;
else {
line = pc2line(dot);
if(line > 0 && fline(str, n, line, f->hist, 0) >= 0)
return 1;
break;
}
}
return 0;
}
static int
fline(char *str, int n, long line, Hist *base, Hist **ret)
{
Hist *start;
Hist *h;
long delta;
int k;
start = base;
h = base;
delta = h->line;
while(h && h->name && line > h->line) {
if(h->name[1] || h->name[2]) {
if(h->offset != 0) {
delta = h->line-h->offset+1;
start = h;
base = h++;
} else {
if(start == base)
start = h++;
else {
k = fline(str, n, line, start, &h);
if(k <= 0)
return k;
}
}
} else {
if(start == base && ret) {
*ret = h;
return 1;
} else {
delta += h->line-start->line;
h++;
start = base;
}
}
}
if(!h)
return -1;
if(start != base)
line = line-start->line+1;
else
line = line-delta+1;
if(!h->name)
strncpy(str, "<eof>", n);
else {
k = fileelem(fnames, (uchar*)start->name, str, n);
if(k+8 < n)
sprint(str+k, ":%ld", line);
}
return 0;
}
int
fileelem(Sym **fp, uchar *cp, char *buf, int n)
{
int i, j;
char *c, *bp, *end;
Sym *sym;
bp = buf;
end = buf+n-1;
for(i = 1; j = (cp[i]<<8)|cp[i+1]; i+=2){
sym = fp[j];
if (sym == nil)
break;
c = sym->name;
if(bp != buf && bp[-1] != '/' && bp < end)
*bp++ = '/';
while(bp < end && *c)
*bp++ = *c++;
}
*bp = 0;
i = bp-buf;
if(i > 1) {
cleanname(buf);
i = strlen(buf);
}
return i;
}
static int
symcomp(void *a, void *b)
{
int i;
i = (*(Sym**)a)->value - (*(Sym**)b)->value;
if (i)
return i;
return strcmp((*(Sym**)a)->name, (*(Sym**)b)->name);
}
static int
txtcomp(void *a, void *b)
{
return ((Txtsym*)a)->sym->value - ((Txtsym*)b)->sym->value;
}
static int
filecomp(void *a, void *b)
{
return ((File*)a)->addr - ((File*)b)->addr;
}
static void
fillsym(Sym *sp, Symbol *s)
{
s->type = sp->type;
s->value = sp->value;
s->name = sp->name;
s->index = 0;
switch(sp->type) {
case 'b':
case 'B':
case 'D':
case 'd':
s->class = CDATA;
break;
case 't':
case 'T':
case 'l':
case 'L':
s->class = CTEXT;
break;
case 'a':
s->class = CAUTO;
break;
case 'p':
s->class = CPARAM;
break;
case 'm':
s->class = CSTAB;
break;
default:
s->class = CNONE;
break;
}
s->handle = 0;
}
uvlong
pc2sp(uvlong pc)
{
uchar *c, u;
uvlong currpc, currsp;
if(spoff == 0)
return ~0;
currsp = 0;
currpc = txtstart - mach->pcquant;
if(pc<currpc || pc>txtend)
return ~0;
for(c = spoff; c < spoffend; c++) {
if (currpc >= pc)
return currsp;
u = *c;
if (u == 0) {
currsp += (c[1]<<24)|(c[2]<<16)|(c[3]<<8)|c[4];
c += 4;
}
else if (u < 65)
currsp += 4*u;
else if (u < 129)
currsp -= 4*(u-64);
else
currpc += mach->pcquant*(u-129);
currpc += mach->pcquant;
}
return ~0;
}
long
pc2line(uvlong pc)
{
uchar *c, u;
uvlong currpc;
long currline;
if(pcline == 0)
return -1;
currline = 0;
currpc = txtstart-mach->pcquant;
if(pc<currpc || pc>txtend)
return ~0;
for(c = pcline; c < pclineend; c++) {
if(currpc >= pc)
return currline;
u = *c;
if(u == 0) {
currline += (c[1]<<24)|(c[2]<<16)|(c[3]<<8)|c[4];
c += 4;
}
else if(u < 65)
currline += u;
else if(u < 129)
currline -= (u-64);
else
currpc += mach->pcquant*(u-129);
currpc += mach->pcquant;
}
return ~0;
}
uvlong
line2addr(long line, uvlong basepc, uvlong endpc)
{
uchar *c, u;
uvlong currpc, pc;
long currline;
long delta, d;
int found;
if(pcline == 0 || line == 0)
return ~0;
currline = 0;
currpc = txtstart-mach->pcquant;
pc = ~0;
found = 0;
delta = HUGEINT;
for(c = pcline; c < pclineend; c++) {
if(endpc && currpc >= endpc)
break;
if(currpc >= basepc) {
if(currline >= line) {
d = currline-line;
found = 1;
} else
d = line-currline;
if(d < delta) {
delta = d;
pc = currpc;
}
}
u = *c;
if(u == 0) {
currline += (c[1]<<24)|(c[2]<<16)|(c[3]<<8)|c[4];
c += 4;
}
else if(u < 65)
currline += u;
else if(u < 129)
currline -= (u-64);
else
currpc += mach->pcquant*(u-129);
currpc += mach->pcquant;
}
if(found)
return pc;
return ~0;
}
static void
printhist(char *msg, Hist *hp, int count)
{
int i;
uchar *cp;
char buf[128];
i = 0;
while(hp->name) {
if(count && ++i > count)
break;
print("%s Line: %lx (%ld)  Offset: %lx (%ld)  Name: ", msg,
hp->line, hp->line, hp->offset, hp->offset);
for(cp = (uchar *)hp->name+1; (*cp<<8)|cp[1]; cp += 2) {
if (cp != (uchar *)hp->name+1)
print("/");
print("%x", (*cp<<8)|cp[1]);
}
fileelem(fnames, (uchar *) hp->name, buf, sizeof(buf));
print(" (%s)\n", buf);
hp++;
}
}
#ifdef DEBUG
void
dumphist(char *name)
{
int i;
File *f;
short *fname;
if(buildtbls() == 0)
return;
if(name)
fname = encfname(name);
for(i = 0, f = files; i < nfiles; i++, f++)
if(fname == 0 || hcomp(f->hist, fname))
printhist("> ", f->hist, f->n);
if(fname)
free(fname);
}
#endif