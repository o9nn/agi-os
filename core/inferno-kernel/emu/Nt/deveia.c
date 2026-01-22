#define Unknown win_Unknown
#include <windows.h>
#undef Unknown
#undef Sleep
#include "dat.h"
#include "fns.h"
#include "error.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <lm.h>
#include <direct.h>
static void openport(int);
static void wrctl(int, char*);
static long rdstat(int, void*, long, ulong );
enum
{
Devchar = 't',
Ndataqid = 1,
Nctlqid,
Nstatqid,
Nqid = 3,
Maxctl = 128,
CommBufSize = ((((8192+128)*2)+3) & ~3)
};
#define NETTYPE(x) ((x)&0x0F)
#define NETID(x) ((x)>>4)
#define NETQID(i,t) (((i)<<4)|(t))
static Dirtab *eiadir;
static int ndir;
typedef struct Eia Eia;
struct Eia {
Ref r;
HANDLE comfh;
int restore;
DCB dcb;
int id;
};
static COMMTIMEOUTS timeouts;
static char* sysdev[] = {
"COM1:",
"COM2:",
"COM3:",
"COM4:",
"COM5:",
"COM6:",
"COM7:",
"COM8:",
NULL
};
static Eia *eia;
typedef struct OptTable OptTable;
struct OptTable {
char *str;
DWORD flag;
};
#define BAD ((DWORD)-1)
static OptTable size[] = {
{"5", 5},
{"6", 6},
{"7", 7},
{"8", 8},
{NULL, BAD}
};
static OptTable stopbits[] = {
{"1", ONESTOPBIT},
{"1.5", ONE5STOPBITS},
{"2", TWOSTOPBITS},
{NULL, BAD}
};
static OptTable parity[] = {
{"o", ODDPARITY},
{"e", EVENPARITY},
{"s", SPACEPARITY},
{"m", MARKPARITY},
{"n", NOPARITY},
{NULL, NOPARITY}
};
static char *
ftos(OptTable *tbl, DWORD flag)
{
while(tbl->str && tbl->flag != flag)
tbl++;
if(tbl->str == 0)
return "unknown";
return tbl->str;
}
static DWORD
stof(OptTable *tbl, char *str)
{
while(tbl->str && strcmp(tbl->str, str) != 0)
tbl++;
return tbl->flag;
}
static void
eiainit(void)
{
int i,x;
byte ports;
int nports;
int max;
Dirtab *dp;
timeouts.ReadIntervalTimeout = 2;
timeouts.ReadTotalTimeoutMultiplier = 0;
timeouts.ReadTotalTimeoutConstant = 200;
timeouts.WriteTotalTimeoutMultiplier = 0;
timeouts.WriteTotalTimeoutConstant = 400;
ports = nports = max = 0;
for(i=0; (sysdev[i] != NULL) && (i<8); i++) {
HANDLE comfh = CreateFile(sysdev[i], 0, 0, NULL,
OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
if(comfh != INVALID_HANDLE_VALUE) {
ports |= 1<<i;
CloseHandle(comfh);
nports++;
max = i;
}
}
if(nports == 0)
return;
ndir = Nqid*nports+1;
dp = eiadir = malloc(ndir*sizeof(Dirtab));
if(dp == 0)
panic("eiainit");
eia = malloc(nports*sizeof(Eia));
if(eia == 0) {
free(dp);
panic("eiainit");
}
sprint(dp->name, ".");
dp->qid.path = 0;
dp->qid.type = QTDIR;
dp->perm = DMDIR|0555;
dp++;
x = 0;
for(i = 0; i <= max; i++) {
if( (ports & (1<<i)) == 0)
continue;
sprint(dp->name, "eia%d", i);
dp->qid.path = NETQID(x, Ndataqid);
dp->perm = 0660;
dp++;
sprint(dp->name, "eia%dctl", i);
dp->qid.path = NETQID(x, Nctlqid);
dp->perm = 0660;
dp++;
sprint(dp->name, "eia%dstatus", i);
dp->qid.path = NETQID(x, Nstatqid);
dp->perm = 0660;
dp++;
eia[x].restore = 0;
eia[x].id = i;
x++;
}
}
static Chan*
eiaattach(char *spec)
{
if(eiadir == nil)
error(Enodev);
return devattach(Devchar, spec);
}
static Walkqid*
eiawalk(Chan *c, Chan *nc, char **name, int nname)
{
return devwalk(c, nc, name, nname, eiadir, ndir, devgen);
}
static int
eiastat(Chan *c, uchar *db, int n)
{
return devstat(c, db, n, eiadir, ndir, devgen);
}
static Chan*
eiaopen(Chan *c, int mode)
{
int port = NETID(c->qid.path);
c = devopen(c, mode, eiadir, ndir, devgen);
switch(NETTYPE(c->qid.path)) {
case Nctlqid:
case Ndataqid:
case Nstatqid:
if(incref(&eia[port].r) != 1)
break;
if(waserror()) {
decref(&eia[port].r);
nexterror();
}
openport(port);
poperror();
break;
}
return c;
}
static void
eiaclose(Chan *c)
{
int port = NETID(c->qid.path);
if((c->flag & COPEN) == 0)
return;
switch(NETTYPE(c->qid.path)) {
case Nctlqid:
case Ndataqid:
case Nstatqid:
if(decref(&eia[port].r) == 0) {
osenter();
CloseHandle(eia[port].comfh);
osleave();
}
break;
}
}
static long
eiaread(Chan *c, void *buf, long n, vlong offset)
{
DWORD cnt;
int port = NETID(c->qid.path);
BOOL good;
if(c->qid.type & QTDIR)
return devdirread(c, buf, n, eiadir, ndir, devgen);
switch(NETTYPE(c->qid.path)) {
case Ndataqid:
cnt = 0;
while(cnt==0) {
osenter();
good = ReadFile(eia[port].comfh, buf, n, &cnt, NULL);
SleepEx(0,FALSE);
osleave();
if(!good)
oserror();
}
return cnt;
case Nctlqid:
return readnum(offset, buf, n, eia[port].id, NUMSIZE);
case Nstatqid:
return rdstat(port, buf, n, offset);
}
return 0;
}
static long
eiawrite(Chan *c, void *buf, long n, vlong offset)
{
DWORD cnt;
char cmd[Maxctl];
int port = NETID(c->qid.path);
BOOL good;
uchar *data;
if(c->qid.type & QTDIR)
error(Eperm);
switch(NETTYPE(c->qid.path)) {
case Ndataqid:
cnt = 0;
data = (uchar*)buf;
while(n>0) {
osenter();
good = WriteFile(eia[port].comfh, data, n, &cnt, NULL);
osleave();
if(!good)
oserror();
data += cnt;
n -= cnt;
}
return (data-(uchar*)buf);
case Nctlqid:
if(n >= sizeof(cmd))
n = sizeof(cmd)-1;
memmove(cmd, buf, n);
cmd[n] = 0;
wrctl(port, cmd);
return n;
}
return 0;
}
static int
eiawstat(Chan *c, uchar *dp, int n)
{
Dir d;
int i;
if(!iseve())
error(Eperm);
if(c->qid.type & QTDIR)
error(Eperm);
if(NETTYPE(c->qid.path) == Nstatqid)
error(Eperm);
n = convM2D(dp, n, &d, nil);
i = Nqid*NETID(c->qid.path)+NETTYPE(c->qid.path)-Ndataqid;
if(d.mode != ~0UL)
eiadir[i+1].perm = d.mode&0666;
return n;
}
Dev eiadevtab = {
Devchar,
"eia",
eiainit,
eiaattach,
eiawalk,
eiastat,
eiaopen,
devcreate,
eiaclose,
eiaread,
devbread,
eiawrite,
devbwrite,
devremove,
eiawstat
};
static void
openport(int port)
{
Eia* p = &eia[port];
p->comfh = CreateFile(sysdev[p->id],
GENERIC_READ|GENERIC_WRITE,
0,
NULL,
OPEN_EXISTING,
FILE_ATTRIBUTE_NORMAL,
NULL);
if(p->comfh == INVALID_HANDLE_VALUE)
oserror();
if(waserror()){
CloseHandle(p->comfh);
p->comfh = INVALID_HANDLE_VALUE;
nexterror();
}
if(!SetupComm(p->comfh, CommBufSize, CommBufSize))
oserror();
if(!p->restore) {
if(!GetCommState(p->comfh, &p->dcb))
oserror();
p->dcb.BaudRate = 9600;
p->dcb.ByteSize = 8;
p->dcb.fParity = 0;
p->dcb.Parity = NOPARITY;
p->dcb.StopBits = ONESTOPBIT;
p->dcb.fInX = 0;
p->dcb.fOutX = 0;
p->dcb.fAbortOnError = 1;
}
if(!SetCommState(p->comfh, &p->dcb) ||
!SetCommTimeouts(p->comfh, &timeouts))
oserror();
poperror();
}
static long
rdstat(int port, void *buf, long n, ulong offset)
{
HANDLE comfh = eia[port].comfh;
char str[Maxctl];
char *s;
DCB dcb;
DWORD modemstatus;
DWORD porterr;
COMSTAT portstat;
int frame, overrun, i;
static enum {
L_CTS, L_DSR, L_RING, L_DCD, L_DTR, L_RTS, L_MAX
};
int status[L_MAX];
static char* lines[] = {
"cts", "dsr", "ring", "dcd", "dtr", "rts", NULL
};
if(!ClearCommError(comfh, &porterr, &portstat))
oserror();
if(!GetCommState(comfh, &dcb))
oserror();
if(!GetCommModemStatus(comfh, &modemstatus))
oserror();
status[L_CTS] = MS_CTS_ON & modemstatus;
status[L_DSR] = MS_DSR_ON & modemstatus;
status[L_RING] = MS_RING_ON & modemstatus;
status[L_DCD] = MS_RLSD_ON & modemstatus;
status[L_DTR] = FALSE;
status[L_RTS] = FALSE;
frame = porterr & CE_FRAME;
overrun = porterr & CE_OVERRUN;
s = seprint(str, str+sizeof(str), "opens %d ferr %d oerr %d baud %d",
eia[port].r.ref-1,
frame,
overrun,
dcb.BaudRate);
for(i=0; i < L_MAX; i++)
if(status[i])
s = seprint(s, str+sizeof(str), " %s", lines[i]);
seprint(s, str+sizeof(str), "\n");
return readstr(offset, buf, n, str);
}
static void
wrctl(int port, char *cmd)
{
DCB dcb;
int nf, n, i;
char *f[16];
HANDLE comfh = eia[port].comfh;
DWORD flag, opt;
BOOL rslt;
int chg;
if(!GetCommState(comfh, &dcb))
oserror();
chg = 0;
nf = tokenize(cmd, f, nelem(f));
for(i = 0; i < nf; i++){
if(strcmp(f[i], "break") == 0){
if(!SetCommBreak(comfh))
oserror();
SleepEx((DWORD)300, FALSE);
if(!ClearCommBreak(comfh))
oserror();
continue;
}
n = atoi(f[i]+1);
switch(*f[i]) {
case 'B':
case 'b':
if(n < 110)
error(Ebadarg);
dcb.BaudRate = n;
chg = 1;
break;
case 'C':
case 'c':
break;
case 'D':
case 'd':
opt = n ? SETDTR : CLRDTR;
if(!EscapeCommFunction(comfh, opt))
oserror();
break;
case 'E':
case 'e':
break;
case 'F':
case 'f':
if(!PurgeComm(comfh, PURGE_TXCLEAR))
oserror();
break;
case 'H':
case 'h':
break;
case 'I':
case 'i':
break;
case 'K':
case 'k':
if(!SetCommBreak(comfh))
oserror();
SleepEx((DWORD)300, FALSE);
if(!ClearCommBreak(comfh))
oserror();
break;
case 'L':
case 'l':
flag = stof(size, f[0]+1);
if(flag == BAD)
error(Ebadarg);
dcb.ByteSize = (BYTE)flag;
chg = 1;
break;
case 'M':
case 'm':
dcb.fOutxCtsFlow = (n!=0);
chg = 1;
break;
case 'N':
case 'n':
break;
case 'P':
case 'p':
flag = stof(parity, f[0]+1);
if(flag==BAD)
error(Ebadarg);
dcb.Parity = (BYTE)flag;
chg = 1;
break;
case 'Q':
case 'q':
break;
case 'R':
case 'r':
opt = n ? SETRTS : CLRRTS;
if(!EscapeCommFunction(comfh, opt))
oserror();
break;
case 'S':
case 's':
flag = stof(stopbits, f[0]+1);
if(flag==BAD)
error(Ebadarg);
dcb.StopBits = flag;
chg = 1;
break;
case 'T':
case 't':
break;
case 'W':
case 'w':
break;
case 'X':
case 'x':
opt = n ? SETXON : SETXOFF;
if(!EscapeCommFunction(comfh, opt))
oserror();
break;
default:
break;
}
}
if(!chg)
return;
osenter();
rslt = FlushFileBuffers(comfh);
if(rslt)
rslt = SetCommState(comfh, &dcb);
osleave();
if(!rslt)
oserror();
eia[port].restore = 1;
eia[port].dcb = dcb;
}