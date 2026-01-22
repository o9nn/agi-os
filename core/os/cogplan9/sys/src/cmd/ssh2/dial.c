#include <u.h>
#include <libc.h>
#include <ctype.h>
typedef struct Conn Conn;
typedef struct Dest Dest;
typedef struct DS DS;
enum
{
Maxstring = 128,
Maxpath = 256,
Maxcsreply = 64*80,
Maxconnms = 2*60*1000,
};
struct DS {
char buf[Maxstring];
char *netdir;
char *proto;
char *rem;
char *local;
char *dir;
int *cfdp;
};
struct Conn {
int pid;
int dead;
int dfd;
int cfd;
char dir[NETPATHLEN+1];
char err[ERRMAX];
};
struct Dest {
Conn *conn;
Conn *connend;
int nkid;
long oalarm;
int naddrs;
QLock winlck;
int winner;
char *nextaddr;
char addrlist[Maxcsreply];
};
static int call(char*, char*, DS*, Dest*, Conn*);
static int csdial(DS*);
static void _dial_string_parse(char*, DS*);
static int
dialimpl(char *dest, char *local, char *dir, int *cfdp)
{
DS ds;
int rv;
char err[ERRMAX], alterr[ERRMAX];
ds.local = local;
ds.dir = dir;
ds.cfdp = cfdp;
_dial_string_parse(dest, &ds);
if(ds.netdir)
return csdial(&ds);
ds.netdir = "/net";
rv = csdial(&ds);
if(rv >= 0)
return rv;
err[0] = '\0';
errstr(err, sizeof err);
if(strstr(err, "refused") != 0){
werrstr("%s", err);
return rv;
}
ds.netdir = "/net.alt";
rv = csdial(&ds);
if(rv >= 0)
return rv;
alterr[0] = 0;
errstr(alterr, sizeof alterr);
if(strstr(alterr, "translate") || strstr(alterr, "does not exist"))
werrstr("%s", err);
else
werrstr("%s", alterr);
return rv;
}
int (*_dial)(char *, char *, char *, int *) = dialimpl;
int
dial(char *dest, char *local, char *dir, int *cfdp)
{
return (*_dial)(dest, local, dir, cfdp);
}
static int
connsalloc(Dest *dp, int addrs)
{
Conn *conn;
free(dp->conn);
dp->connend = nil;
assert(addrs > 0);
dp->conn = mallocz(addrs * sizeof *dp->conn, 1);
if(dp->conn == nil)
return -1;
dp->connend = dp->conn + addrs;
for(conn = dp->conn; conn < dp->connend; conn++)
conn->cfd = conn->dfd = -1;
return 0;
}
static void
freedest(Dest *dp)
{
long oalarm;
if (dp == nil)
return;
oalarm = dp->oalarm;
free(dp->conn);
free(dp);
if (oalarm >= 0)
alarm(oalarm);
}
static void
closeopenfd(int *fdp)
{
if (*fdp >= 0) {
close(*fdp);
*fdp = -1;
}
}
static void
notedeath(Dest *dp, char *exitsts)
{
int i, n, pid;
char *fields[5];
Conn *conn;
for (i = 0; i < nelem(fields); i++)
fields[i] = "";
n = tokenize(exitsts, fields, nelem(fields));
if (n < 4)
return;
pid = atoi(fields[0]);
if (pid <= 0)
return;
for (conn = dp->conn; conn < dp->connend; conn++)
if (conn->pid == pid && !conn->dead) {
if (conn - dp->conn != dp->winner) {
closeopenfd(&conn->dfd);
closeopenfd(&conn->cfd);
}
strncpy(conn->err, fields[4], sizeof conn->err - 1);
conn->err[sizeof conn->err - 1] = '\0';
conn->dead = 1;
return;
}
}
static int
outstandingprocs(Dest *dp)
{
Conn *conn;
for (conn = dp->conn; conn < dp->connend; conn++)
if (!conn->dead)
return 1;
return 0;
}
static int
reap(Dest *dp)
{
char exitsts[2*ERRMAX];
if (outstandingprocs(dp) && await(exitsts, sizeof exitsts) >= 0) {
notedeath(dp, exitsts);
return 0;
}
return -1;
}
static int
fillinds(DS *ds, Dest *dp)
{
Conn *conn;
if (dp->winner < 0)
return -1;
conn = &dp->conn[dp->winner];
if (ds->cfdp)
*ds->cfdp = conn->cfd;
if (ds->dir) {
strncpy(ds->dir, conn->dir, NETPATHLEN);
ds->dir[NETPATHLEN-1] = '\0';
}
return conn->dfd;
}
static int
connectwait(Dest *dp, char *besterr)
{
Conn *conn;
while (dp->winner < 0 && reap(dp) >= 0)
;
for (conn = dp->conn; conn < dp->connend; conn++)
if (!conn->dead)
postnote(PNPROC, conn->pid, "alarm");
while (reap(dp) >= 0)
;
for (conn = dp->conn; conn < dp->connend; conn++)
if (conn - dp->conn != dp->winner && conn->dead &&
conn->err[0]) {
strncpy(besterr, conn->err, ERRMAX-1);
conn->err[ERRMAX-1] = '\0';
break;
}
return dp->winner;
}
static int
parsecs(Dest *dp, char **clonep, char **destp)
{
char *dest, *p;
dest = strchr(dp->nextaddr, ' ');
if(dest == nil)
return -1;
*dest++ = '\0';
p = strchr(dest, '\n');
if(p == nil)
return -1;
*p++ = '\0';
*clonep = dp->nextaddr;
*destp = dest;
dp->nextaddr = p;
return 0;
}
static void
pickuperr(char *besterr, char *err)
{
err[0] = '\0';
errstr(err, ERRMAX);
if(strstr(err, "does not exist") == 0)
strcpy(besterr, err);
}
static int
catcher(void *, char *s)
{
return strstr(s, "alarm") != nil;
}
static int
dialmulti(DS *ds, Dest *dp)
{
int rv, kid, kidme;
char *clone, *dest;
char err[ERRMAX], besterr[ERRMAX];
dp->winner = -1;
dp->nkid = 0;
while(dp->winner < 0 && *dp->nextaddr != '\0' &&
parsecs(dp, &clone, &dest) >= 0) {
kidme = dp->nkid++;
kid = rfork(RFPROC|RFMEM);
if (kid < 0)
--dp->nkid;
else if (kid == 0) {
atnotify(catcher, 1);
*besterr = '\0';
rv = call(clone, dest, ds, dp, &dp->conn[kidme]);
if(rv < 0)
pickuperr(besterr, err);
_exits(besterr);
}
}
rv = connectwait(dp, besterr);
if(rv < 0 && *besterr)
werrstr("%s", besterr);
else
werrstr("%s", err);
return rv;
}
static int
csdial(DS *ds)
{
int n, fd, rv, addrs, bleft;
char c;
char *addrp, *clone2, *dest;
char buf[Maxstring], clone[Maxpath], err[ERRMAX], besterr[ERRMAX];
Dest *dp;
dp = mallocz(sizeof *dp, 1);
if(dp == nil)
return -1;
dp->winner = -1;
dp->oalarm = alarm(0);
if (connsalloc(dp, 1) < 0) {
freedest(dp);
return -1;
}
snprint(buf, sizeof(buf), "%s/cs", ds->netdir);
fd = open(buf, ORDWR);
if(fd < 0){
snprint(clone, sizeof(clone), "%s/%s/clone", ds->netdir, ds->proto);
rv = call(clone, ds->rem, ds, dp, &dp->conn[0]);
fillinds(ds, dp);
freedest(dp);
return rv;
}
snprint(buf, sizeof(buf), "%s!%s", ds->proto, ds->rem);
if(write(fd, buf, strlen(buf)) < 0){
close(fd);
freedest(dp);
return -1;
}
seek(fd, 0, 0);
addrs = 0;
addrp = dp->nextaddr = dp->addrlist;
bleft = sizeof dp->addrlist - 2;
while(bleft > 0 && (n = read(fd, addrp, bleft)) > 0) {
if (addrp[n-1] != '\n')
addrp[n++] = '\n';
addrs++;
addrp += n;
bleft -= n;
}
if (addrs > 0 && bleft <= 0 && read(fd, &c, 1) == 1)
addrs--;
close(fd);
*besterr = 0;
rv = -1;
dp->naddrs = addrs;
if (addrs == 0)
werrstr("no address to dial");
else if (addrs == 1) {
if (parsecs(dp, &clone2, &dest) >= 0 &&
(rv = call(clone2, dest, ds, dp, &dp->conn[0])) < 0) {
pickuperr(besterr, err);
werrstr("%s", besterr);
}
} else if (connsalloc(dp, addrs) >= 0)
rv = dialmulti(ds, dp);
if (rv >= 0 && dp->winner >= 0)
rv = fillinds(ds, dp);
freedest(dp);
return rv;
}
static int
call(char *clone, char *dest, DS *ds, Dest *dp, Conn *conn)
{
int fd, cfd, n, calleralarm, oalarm;
char cname[Maxpath], name[Maxpath], data[Maxpath], *p;
if(*clone == '/'){
p = strchr(clone+1, '/');
if(p == nil)
p = clone;
else
p++;
} else
p = clone;
snprint(cname, sizeof cname, "%s/%s", ds->netdir, p);
conn->pid = getpid();
conn->cfd = cfd = open(cname, ORDWR);
if(cfd < 0)
return -1;
n = read(cfd, name, sizeof(name)-1);
if(n < 0){
closeopenfd(&conn->cfd);
return -1;
}
name[n] = 0;
for(p = name; *p == ' '; p++)
;
snprint(name, sizeof(name), "%ld", strtoul(p, 0, 0));
p = strrchr(cname, '/');
*p = 0;
if(ds->dir)
snprint(conn->dir, NETPATHLEN, "%s/%s", cname, name);
snprint(data, sizeof(data), "%s/%s/data", cname, name);
calleralarm = dp->oalarm > 0;
if (calleralarm)
alarm(dp->oalarm);
else if (dp->naddrs > 1)
alarm(Maxconnms);
if(ds->local)
snprint(name, sizeof(name), "connect %s %s", dest, ds->local);
else
snprint(name, sizeof(name), "connect %s", dest);
if(write(cfd, name, strlen(name)) < 0){
closeopenfd(&conn->cfd);
return -1;
}
oalarm = alarm(0);
if (calleralarm)
dp->oalarm = oalarm;
conn->dfd = fd = open(data, ORDWR);
if(fd < 0){
closeopenfd(&conn->cfd);
alarm(dp->oalarm);
return -1;
}
if(ds->cfdp == nil)
closeopenfd(&conn->cfd);
n = conn - dp->conn;
if (dp->winner < 0) {
qlock(&dp->winlck);
if (dp->winner < 0 && conn < dp->connend)
dp->winner = n;
qunlock(&dp->winlck);
}
alarm(calleralarm? dp->oalarm: 0);
return fd;
}
static char *
backoverchans(char *st, char *p)
{
char *sl;
for (sl = p; --p >= st && isascii(*p) && isdigit(*p); sl = p) {
while (--p >= st && isascii(*p) && isdigit(*p))
;
if (p < st || *p != '/')
break;
while (p > st && p[-1] == '/')
p--;
}
return sl;
}
static void
_dial_string_parse(char *str, DS *ds)
{
char *p, *p2;
strncpy(ds->buf, str, Maxstring);
ds->buf[Maxstring-1] = 0;
p = strchr(ds->buf, '!');
if(p == 0) {
ds->netdir = 0;
ds->proto = "net";
ds->rem = ds->buf;
} else {
if(*ds->buf != '/' && *ds->buf != '#'){
ds->netdir = 0;
ds->proto = ds->buf;
} else {
p2 = backoverchans(ds->buf, p);
while (--p2 > ds->buf && *p2 != '/')
;
*p2++ = 0;
ds->netdir = ds->buf;
ds->proto = p2;
}
*p = 0;
ds->rem = p + 1;
}
}