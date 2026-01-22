#include <u.h>
#include <libc.h>
#include <bio.h>
#include <mach.h>
#include <ctype.h>
static	int	rtrace(uvlong, uvlong, uvlong);
static	int	ctrace(uvlong, uvlong, uvlong);
static	int	i386trace(uvlong, uvlong, uvlong);
static	int	amd64trace(uvlong, uvlong, uvlong);
static	uvlong	getval(uvlong);
static	void	inithdr(int);
static	void	fatal(char*, ...);
static	void	readstack(void);
static	Fhdr	fhdr;
static	int	interactive;
#define	FRAMENAME	".frame"
static void
usage(void)
{
fprint(2, "usage: ktrace [-i] kernel pc sp [link]\n");
exits("usage");
}
static void
printaddr(char *addr, uvlong pc)
{
int i;
char *p;
if(strlen(addr) == 8 && strchr(addr, '+') == nil){
for(i=0; i<8; i++)
if(!isxdigit(addr[i]))
break;
if(i == 8){
print("src(%#.8llux);
return;
}
}
if(p=strchr(addr, '+')){
*p++ = 0;
print("src(%#.8llux);
}else
print("src(%#.8llux);
}
static void (*fmt)(char*, uvlong) = printaddr;
void
main(int argc, char *argv[])
{
int (*t)(uvlong, uvlong, uvlong);
uvlong pc, sp, link;
int fd;
ARGBEGIN{
case 'i':
interactive++;
break;
default:
usage();
}ARGEND
link = 0;
t = ctrace;
switch(argc){
case 4:
t = rtrace;
link = strtoull(argv[3], 0, 16);
break;
case 3:
break;
default:
usage();
}
pc = strtoull(argv[1], 0, 16);
sp = strtoull(argv[2], 0, 16);
if(!interactive)
readstack();
fd = open(argv[0], OREAD);
if(fd < 0)
fatal("can't open %s: %r", argv[0]);
inithdr(fd);
switch(fhdr.magic){
case I_MAGIC:
t = i386trace;
break;
case S_MAGIC:
t = amd64trace;
break;
case A_MAGIC:
case J_MAGIC:
t = ctrace;
break;
case K_MAGIC:
case D_MAGIC:
case V_MAGIC:
case M_MAGIC:
case E_MAGIC:
case Q_MAGIC:
case N_MAGIC:
case L_MAGIC:
t = rtrace;
break;
case X_MAGIC:
sysfatal("can't ktrace %s", argv[0]);
break;
default:
fprint(2, "%s: warning: can't tell what type of stack %s uses; assuming it's %s\n",
argv0, argv[0], argc == 4 ? "risc" : "cisc");
break;
}
(*t)(pc, sp, link);
exits(0);
}
static void
inithdr(int fd)
{
seek(fd, 0, 0);
if(!crackhdr(fd, &fhdr))
fatal("read text header");
if(syminit(fd, &fhdr) < 0)
fatal("%r\n");
}
static int
rtrace(uvlong pc, uvlong sp, uvlong link)
{
Symbol s, f;
char buf[128];
uvlong oldpc;
int i;
i = 0;
while(findsym(pc, CTEXT, &s)) {
if(pc == s.value)
f.value = 0;
else if(findlocal(&s, FRAMENAME, &f) == 0)
break;
symoff(buf, sizeof buf, pc, CANY);
fmt(buf, pc);
oldpc = pc;
if(s.type == 'L' || s.type == 'l' || pc <= s.value+mach->pcquant){
if(link == 0)
fprint(2, "%s: need to supply a valid link register\n", argv0);
pc = link;
}else{
pc = getval(sp);
if(pc == 0)
break;
}
if(pc == 0 || (pc == oldpc && f.value == 0))
break;
sp += f.value;
if(++i > 40)
break;
}
return i;
}
static int
ctrace(uvlong pc, uvlong sp, uvlong link)
{
Symbol s;
char buf[128];
int found;
uvlong opc, moved;
long j;
USED(link);
j = 0;
opc = 0;
while(pc && opc != pc) {
moved = pc2sp(pc);
if (moved == ~0){
print("pc2sp(%#.8llux) = -1 %r\n", pc);
break;
}
found = findsym(pc, CTEXT, &s);
if (!found){
print("findsym fails\n");
break;
}
symoff(buf, sizeof buf, pc, CANY);
fmt(buf, pc);
sp += moved;
opc = pc;
pc = getval(sp);
if(pc == 0)
break;
sp += mach->szaddr;
if(++j > 40)
break;
}
return j;
}
static int
i386trace(uvlong pc, uvlong sp, uvlong link)
{
int i;
uvlong osp;
Symbol s, f;
char buf[128];
USED(link);
i = 0;
osp = 0;
while(findsym(pc, CTEXT, &s)) {
symoff(buf, sizeof buf, pc, CANY);
fmt(buf, pc);
if(pc != s.value) {
if(findlocal(&s, FRAMENAME, &f) == 0)
break;
sp += f.value-mach->szaddr;
}else if(strcmp(s.name, "forkret") == 0){
print("
sp +=  15 * mach->szaddr;
}
pc = getval(sp);
if(pc == 0 && strcmp(s.name, "forkret") == 0){
sp += 3 * mach->szaddr;
print("
s.name = "";
pc = getval(sp);
}
if(pc == 0) {
print("
break;
}
osp = sp;
sp += mach->szaddr;
if(strcmp(s.name, "forkret") == 0)
sp += 2 * mach->szaddr;
if(++i > 40)
break;
}
return i;
}
static int
amd64trace(uvlong pc, uvlong sp, uvlong link)
{
int i, isintrr;
uvlong osp;
Symbol s, f;
char buf[128];
USED(link);
i = 0;
osp = 0;
while(findsym(pc, CTEXT, &s)) {
symoff(buf, sizeof buf, pc, CANY);
fmt(buf, pc);
if(strcmp(s.name, "_intrr") == 0)
isintrr = 1;
else
isintrr = 0;
if(pc != s.value) {
if(findlocal(&s, FRAMENAME, &f) == 0)
break;
sp += f.value-mach->szaddr;
}
else if(isintrr){
print("
sp += 19 * mach->szaddr;
}
pc = getval(sp);
if(pc == 0 && isintrr){
sp += 3 * mach->szaddr;
print("
s.name = "";
sp = getval(sp);
pc = getval(sp);
}
if(pc == 0) {
print("
break;
}
osp = sp;
if(!isintrr)
sp += mach->szaddr;
if(++i > 40)
break;
}
return i;
}
int naddr;
uvlong addr[1024];
uvlong val[1024];
static void
putval(uvlong a, uvlong v)
{
if(naddr < nelem(addr)){
addr[naddr] = a;
val[naddr] = v;
naddr++;
}
}
static void
readstack(void)
{
Biobuf b;
char *p;
char *f[64];
int nf, i;
Binit(&b, 0, OREAD);
while(p=Brdline(&b, '\n')){
p[Blinelen(&b)-1] = 0;
nf = tokenize(p, f, nelem(f));
for(i=0; i<nf; i++){
if(p=strchr(f[i], '=')){
*p++ = 0;
putval(strtoull(f[i], 0, 16), strtoull(p, 0, 16));
}
}
}
}
static uvlong
getval(uvlong a)
{
char buf[256];
int i, n;
uvlong r;
if(interactive){
print("
n = read(0, buf, sizeof(buf)-1);
if(n <= 0)
return 0;
buf[n] = '\0';
r = strtoull(buf, 0, 16);
}else{
r = 0;
for(i=0; i<naddr; i++)
if(addr[i] == a)
r = val[i];
}
return r;
}
static void
fatal(char *fmt, ...)
{
char buf[4096];
va_list arg;
va_start(arg, fmt);
vseprint(buf, buf+sizeof(buf), fmt, arg);
va_end(arg);
fprint(2, "ktrace: %s\n", buf);
exits(buf);
}