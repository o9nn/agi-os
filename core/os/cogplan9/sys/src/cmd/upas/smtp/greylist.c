#include "common.h"
#include "smtpd.h"
#include "smtp.h"
#include <ctype.h>
#include <ip.h>
#include <ndb.h>
enum {
Nonspammax = 14*60*60,
Nonspammin = 5*60,
};
typedef struct {
int	existed;
int	created;
int	noperm;
long	mtime;
} Greysts;
static char whitelist[] = "/mail/grey/whitelist";
static int
onwhitelist(void)
{
int lnlen;
char *line, *parse, *p;
char input[128];
uchar *mask;
uchar mask4[IPaddrlen], addr4[IPaddrlen];
uchar rmask[IPaddrlen], addr[IPaddrlen];
uchar ipmasked[IPaddrlen], addrmasked[IPaddrlen];
Biobuf *wl;
wl = Bopen(whitelist, OREAD);
if (wl == nil)
return 1;
while ((line = Brdline(wl, '\n')) != nil) {
lnlen = Blinelen(wl);
line[lnlen-1] = '\0';
p = strpbrk(line, " \t");
if (p)
*p = 0;
if (line[0] == '#' || line[0] == 0)
continue;
parse = line;
if (strchr(line, '/') == nil) {
strecpy(input, input + sizeof input - 5, line);
if (strchr(line, ':') != nil)
strcat(input, "/128");
else if (strchr(line, '.') != nil)
strcat(input, "/24");
parse = input;
}
mask = rmask;
if (strchr(line, ':') != nil) {
parseip(addr, parse);
p = strchr(parse, '/');
if (p != nil)
parseipmask(mask, p);
else
mask = IPallbits;
} else {
v4parsecidr(addr4, mask4, parse);
v4tov6(addr, addr4);
v4tov6(mask, mask4);
}
maskip(addr, mask, addrmasked);
maskip(rsysip, mask, ipmasked);
if (equivip6(ipmasked, addrmasked))
break;
}
Bterm(wl);
return line != nil;
}
static int mkdirs(char *);
static int
mkpdirs(char *path)
{
int rv = 0;
char *sl = strrchr(path, '/');
if (sl != nil) {
*sl = '\0';
rv = mkdirs(path);
*sl = '/';
}
return rv;
}
static int
mkdirs(char *path)
{
int fd;
if (access(path, AEXIST) >= 0)
return 0;
if (mkpdirs(path) < 0)
return -1;
fd = create(path, OREAD, 0777|DMDIR);
if (fd < 0)
return access(path, AEXIST) < 0? -1: 0;
close(fd);
return 0;
}
static long
getmtime(char *file)
{
int fd;
long mtime = -1;
Dir *ds;
fd = open(file, ORDWR);
if (fd < 0)
return mtime;
ds = dirfstat(fd);
if (ds != nil) {
mtime = ds->mtime;
if (0) {
ds->mtime = time(0);
if (dirfwstat(fd, ds) < 0)
syslog(0, "smtpd", "dirfwstat %s: %r", file);
}
free(ds);
write(fd, "x", 1);
}
close(fd);
return mtime;
}
static void
tryaddgrey(char *file, Greysts *gsp)
{
int fd = create(file, OWRITE|OEXCL, 0666);
gsp->created = (fd >= 0);
if (fd >= 0) {
close(fd);
gsp->existed = 0;
gsp->mtime = time(0);
} else {
gsp->existed = access(file, AEXIST) >= 0;
if (gsp->existed)
gsp->mtime = getmtime(file);
else if (mkpdirs(file) < 0)
gsp->noperm = 1;
}
}
static void
addgreylist(char *file, Greysts *gsp)
{
tryaddgrey(file, gsp);
if (!gsp->created && !gsp->existed && !gsp->noperm)
tryaddgrey(file, gsp);
}
static int
recentcall(Greysts *gsp)
{
long delay = time(0) - gsp->mtime;
if (!gsp->existed)
return 0;
return delay >= Nonspammin && delay <= Nonspammax;
}
static int
isrcptrecent(char *rcpt)
{
char *user;
char file[256];
Greysts gs;
Greysts *gsp = &gs;
if (rcpt[0] == '\0' || strchr(rcpt, '/') != nil ||
strcmp(rcpt, ".") == 0 || strcmp(rcpt, "..") == 0)
return 0;
user = strrchr(rcpt, '!');
if (user == nil)
user = rcpt;
else
user++;
snprint(file, sizeof file, "/mail/grey/tmp/%s/%s/%s",
nci->lsys, nci->rsys, user);
memset(gsp, 0, sizeof *gsp);
addgreylist(file, gsp);
if (gsp->existed && recentcall(gsp)) {
syslog(0, "smtpd",
"%s/%s was grey; adding IP to white", nci->rsys, rcpt);
return 1;
} else if (gsp->existed)
syslog(0, "smtpd", "call for %s/%s was just minutes ago "
"or long ago", nci->rsys, rcpt);
else
syslog(0, "smtpd", "no call registered for %s/%s; registering",
nci->rsys, rcpt);
return 0;
}
void
vfysenderhostok(void)
{
char *fqdn;
int recent = 0;
Link *l;
if (onwhitelist())
return;
for (l = rcvers.first; l; l = l->next)
if (isrcptrecent(s_to_c(l->p)))
recent = 1;
if (recent) {
int fd = create(whitelist, OWRITE, 0666|DMAPPEND);
if (fd >= 0) {
seek(fd, 0, 2);
fqdn = csgetvalue(nci->root, "ip", nci->rsys, "dom",
nil);
if (fqdn != nil)
fprint(fd, "%s %s\n", nci->rsys, fqdn);
else
fprint(fd, "%s\n", nci->rsys);
free(fqdn);
close(fd);
}
} else {
syslog(0, "smtpd",
"no recent call from %s for a rcpt; rejecting with temporary failure",
nci->rsys);
reply("451 please try again soon from the same IP.\r\n");
exits("no recent call for a rcpt");
}
}