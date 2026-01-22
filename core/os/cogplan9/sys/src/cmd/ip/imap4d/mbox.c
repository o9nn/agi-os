#include <u.h>
#include <libc.h>
#include <bio.h>
#include <auth.h>
#include "imap4d.h"
static NamedInt	flagChars[NFlags] =
{
{"s",	MSeen},
{"a",	MAnswered},
{"f",	MFlagged},
{"D",	MDeleted},
{"d",	MDraft},
{"r",	MRecent},
};
static	int	fsCtl = -1;
static	void	boxFlags(Box *box);
static	int	createImp(Box *box, Qid *qid);
static	void	fsInit(void);
static	void	mboxGone(Box *box);
static	MbLock	*openImp(Box *box, int new);
static	int	parseImp(Biobuf *b, Box *box);
static	int	readBox(Box *box);
static	ulong	uidRenumber(Msg *m, ulong uid, int force);
static	int	impFlags(Box *box, Msg *m, char *flags);
Box*
openBox(char *name, char *fsname, int writable)
{
Box *box;
MbLock *ml;
int n, new;
if(cistrcmp(name, "inbox") == 0)
if(access("msgs", AEXIST) == 0)
name = "msgs";
else
name = "mbox";
fsInit();
debuglog("imap4d open %s %s\n", name, fsname);
if(fprint(fsCtl, "open '/mail/box/%s/%s' %s", username, name, fsname) < 0){
char err[ERRMAX];
rerrstr(err, sizeof err);
if(strstr(err, "file does not exist") == nil)
fprint(2,
"imap4d at %lud: upas/fs open %s/%s as %s failed: '%s' %s",
time(nil), username, name, fsname, err,
ctime(time(nil)));
fprint(fsCtl, "close %s", fsname);
return nil;
}
box = MKZ(Box);
box->writable = writable;
n = strlen(name) + 1;
box->name = emalloc(n);
strcpy(box->name, name);
n += STRLEN(".imp");
box->imp = emalloc(n);
snprint(box->imp, n, "%s.imp", name);
n = strlen(fsname) + 1;
box->fs = emalloc(n);
strcpy(box->fs, fsname);
n = STRLEN("/mail/fs/") + strlen(fsname) + 1;
box->fsDir = emalloc(n);
snprint(box->fsDir, n, "/mail/fs/%s", fsname);
box->uidnext = 1;
new = readBox(box);
if(new >= 0){
ml = openImp(box, new);
if(ml != nil){
closeImp(box, ml);
return box;
}
}
closeBox(box, 0);
return nil;
}
MbLock*
checkBox(Box *box, int imped)
{
MbLock *ml;
Dir *d;
int new;
if(box == nil)
return nil;
d = cdDirstat(box->fsDir, ".");
if(d == nil){
mboxGone(box);
return nil;
}
new = 0;
if(box->qid.path != d->qid.path || box->qid.vers != d->qid.vers
|| box->mtime != d->mtime){
new = readBox(box);
if(new < 0){
free(d);
return nil;
}
}
free(d);
ml = openImp(box, new);
if(ml == nil)
box->writable = 0;
else if(!imped){
closeImp(box, ml);
ml = nil;
}
return ml;
}
static void
mboxGone(Box *box)
{
Msg *m;
if(cdExists(mboxDir, box->name) < 0)
cdRemove(mboxDir, box->imp);
for(m = box->msgs; m != nil; m = m->next)
m->expunged = 1;
box->writable = 0;
}
static int
readBox(Box *box)
{
Msg *msgs, *m, *last;
Dir *d;
char *s;
long max, id;
int i, nd, fd, new;
fd = cdOpen(box->fsDir, ".", OREAD);
if(fd < 0){
syslog(0, "mail",
"imap4d at %lud: upas/fs stat of %s/%s aka %s failed: %r",
time(nil), username, box->name, box->fsDir);
mboxGone(box);
return -1;
}
d = dirfstat(fd);
if(d == nil){
close(fd);
return -1;
}
box->mtime = d->mtime;
box->qid = d->qid;
last = nil;
msgs = box->msgs;
max = 0;
new = 0;
free(d);
while((nd = dirread(fd, &d)) > 0){
for(i = 0; i < nd; i++){
s = d[i].name;
id = strtol(s, &s, 10);
if(id <= max || *s != '\0'
|| (d[i].mode & DMDIR) != DMDIR)
continue;
max = id;
while(msgs != nil){
last = msgs;
msgs = msgs->next;
if(last->id == id)
goto continueDir;
last->expunged = 1;
}
new = 1;
m = MKZ(Msg);
m->id = id;
m->fsDir = box->fsDir;
m->fs = emalloc(2 * (MsgNameLen + 1));
m->efs = seprint(m->fs, m->fs + (MsgNameLen + 1), "%lud/", id);
m->size = ~0UL;
m->lines = ~0UL;
m->prev = last;
m->flags = MRecent;
if(!msgInfo(m))
freeMsg(m);
else{
if(last == nil)
box->msgs = m;
else
last->next = m;
last = m;
}
continueDir:;
}
free(d);
}
close(fd);
for(; msgs != nil; msgs = msgs->next)
msgs->expunged = 1;
id = 1;
for(m = box->msgs; m != nil; m = m->next){
if(m->seq && m->seq != id)
bye("internal error assigning message numbers");
m->seq = id++;
}
box->max = id - 1;
return new;
}
#define IMPMAGIC	"imap internal mailbox description\n"
static MbLock*
openImp(Box *box, int new)
{
Qid qid;
Biobuf b;
MbLock *ml;
int fd;
int once;
ml = mbLock();
if(ml == nil)
return nil;
fd = cdOpen(mboxDir, box->imp, OREAD);
once = 0;
ZZZhack:
if(fd < 0 || fqid(fd, &qid) < 0){
if(fd < 0){
char buf[ERRMAX];
errstr(buf, sizeof buf);
if(cistrstr(buf, "does not exist") == nil)
fprint(2, "imap4d at %lud: imp open failed: %s\n", time(nil), buf);
if(!once && cistrstr(buf, "locked") != nil){
once = 1;
fprint(2, "imap4d at %lud: imp %s/%s %s locked when it shouldn't be; spinning\n", time(nil), username, box->name, box->imp);
fd = openLocked(mboxDir, box->imp, OREAD);
goto ZZZhack;
}
}
if(fd >= 0)
close(fd);
fd = createImp(box, &qid);
if(fd < 0){
mbUnlock(ml);
return nil;
}
box->dirtyImp = 1;
if(box->uidvalidity == 0)
box->uidvalidity = box->mtime;
box->impQid = qid;
new = 1;
}else if(qid.path != box->impQid.path || qid.vers != box->impQid.vers){
Binit(&b, fd, OREAD);
if(!parseImp(&b, box)){
box->dirtyImp = 1;
if(box->uidvalidity == 0)
box->uidvalidity = box->mtime;
}
Bterm(&b);
box->impQid = qid;
new = 1;
}
if(new)
boxFlags(box);
close(fd);
return ml;
}
void
closeImp(Box *box, MbLock *ml)
{
Msg *m;
Qid qid;
Biobuf b;
char buf[NFlags+1];
int fd;
if(ml == nil)
return;
if(!box->dirtyImp){
mbUnlock(ml);
return;
}
fd = cdCreate(mboxDir, box->imp, OWRITE, 0664);
if(fd < 0){
mbUnlock(ml);
return;
}
Binit(&b, fd, OWRITE);
box->dirtyImp = 0;
Bprint(&b, "%s", IMPMAGIC);
Bprint(&b, "%.*lud %.*lud\n", NUid, box->uidvalidity, NUid, box->uidnext);
for(m = box->msgs; m != nil; m = m->next){
if(m->expunged)
continue;
wrImpFlags(buf, m->flags, strcmp(box->fs, "imap") == 0);
Bprint(&b, "%.*s %.*lud %s\n", NDigest, m->info[IDigest], NUid, m->uid, buf);
}
Bterm(&b);
if(fqid(fd, &qid) >= 0)
box->impQid = qid;
close(fd);
mbUnlock(ml);
}
void
wrImpFlags(char *buf, int flags, int killRecent)
{
int i;
for(i = 0; i < NFlags; i++){
if((flags & flagChars[i].v)
&& (flagChars[i].v != MRecent || !killRecent))
buf[i] = flagChars[i].name[0];
else
buf[i] = '-';
}
buf[i] = '\0';
}
int
emptyImp(char *mbox)
{
Dir *d;
long mode;
int fd;
fd = cdCreate(mboxDir, impName(mbox), OWRITE, 0664);
if(fd < 0)
return -1;
d = cdDirstat(mboxDir, mbox);
if(d == nil){
close(fd);
return -1;
}
fprint(fd, "%s%.*lud %.*lud\n", IMPMAGIC, NUid, d->mtime, NUid, 1UL);
mode = d->mode & 0777;
nulldir(d);
d->mode = mode;
dirfwstat(fd, d);
free(d);
return fd;
}
static int
createImp(Box *box, Qid *qid)
{
Dir *d;
long mode;
int fd;
fd = cdCreate(mboxDir, box->imp, OREAD, 0664);
if(fd < 0)
return -1;
d = cdDirstat(mboxDir, box->name);
if(d != nil){
mode = d->mode & 0777;
nulldir(d);
d->mode = mode;
dirfwstat(fd, d);
free(d);
}
if(fqid(fd, qid) < 0){
close(fd);
return -1;
}
return fd;
}
static int
parseImp(Biobuf *b, Box *box)
{
Msg *m, *mm;
char *s, *t, *toks[3];
ulong uid, u;
int match, n;
m = box->msgs;
s = Brdline(b, '\n');
if(s == nil || Blinelen(b) != STRLEN(IMPMAGIC)
|| strncmp(s, IMPMAGIC, STRLEN(IMPMAGIC)) != 0)
return 0;
s = Brdline(b, '\n');
if(s == nil || Blinelen(b) != 2*NUid + 2)
return 0;
s[2*NUid + 1] = '\0';
u = strtoul(s, &t, 10);
if(u != box->uidvalidity && box->uidvalidity != 0)
return 0;
box->uidvalidity = u;
if(*t != ' ' || t != s + NUid)
return 0;
t++;
u = strtoul(t, &t, 10);
if(box->uidnext > u)
return 0;
box->uidnext = u;
if(t != s + 2*NUid+1 || box->uidnext == 0)
return 0;
uid = ~0;
while(m != nil){
s = Brdline(b, '\n');
if(s == nil)
break;
n = Blinelen(b) - 1;
if(n != NDigest + NUid + NFlags + 2
|| s[NDigest] != ' ' || s[NDigest + NUid + 1] != ' ')
return 0;
toks[0] = s;
s[NDigest] = '\0';
toks[1] = s + NDigest + 1;
s[NDigest + NUid + 1] = '\0';
toks[2] = s + NDigest + NUid + 2;
s[n] = '\0';
t = toks[1];
u = strtoul(t, &t, 10);
if(*t != '\0' || uid != ~0 && (uid >= u && u || u && !uid))
return 0;
uid = u;
if(!uid){
for(; m != nil && m->uid; m = m->next)
;
for(mm = m; mm != nil; mm = mm->next){
if(mm->info[IDigest] != nil &&
strcmp(mm->info[IDigest], toks[0]) == 0){
if(!mm->uid)
mm->flags = 0;
if(!impFlags(box, mm, toks[2]))
return 0;
m = mm->next;
break;
}
}
continue;
}
for(; m != nil && (m->expunged || m->uid && m->uid < uid); m = m->next)
;
if(m == nil)
break;
match = m->info[IDigest] != nil &&
strcmp(m->info[IDigest], toks[0]) == 0;
if(uid && (m->uid == uid || !m->uid && match)){
if(!match)
bye("inconsistent uid");
if(!m->uid)
m->flags = 0;
if(!impFlags(box, m, toks[2]))
return 0;
m->uid = uid;
m = m->next;
}
}
return 1;
}
static int
impFlags(Box *box, Msg *m, char *flags)
{
int i, f;
f = 0;
for(i = 0; i < NFlags; i++){
if(flags[i] == '-')
continue;
if(flags[i] != flagChars[i].name[0])
return 0;
f |= flagChars[i].v;
}
if((f & MRecent) && strcmp(box->fs, "imap") == 0)
box->dirtyImp = 1;
f |= m->flags & MRecent;
if(m->uid && m->flags != f){
box->sendFlags = 1;
m->sendFlags = 1;
}
m->flags = f;
return 1;
}
static void
boxFlags(Box *box)
{
Msg *m;
box->recent = 0;
for(m = box->msgs; m != nil; m = m->next){
if(m->uid == 0){
box->dirtyImp = 1;
box->uidnext = uidRenumber(m, box->uidnext, 0);
}
if(m->flags & MRecent)
box->recent++;
}
}
static ulong
uidRenumber(Msg *m, ulong uid, int force)
{
for(; m != nil; m = m->next){
if(!force && m->uid != 0)
bye("uid renumbering with a valid uid");
m->uid = uid++;
}
return uid;
}
void
closeBox(Box *box, int opened)
{
Msg *m, *next;
myChdir(mboxDir);
if(box->writable){
deleteMsgs(box);
if(expungeMsgs(box, 0))
closeImp(box, checkBox(box, 1));
}
if(fprint(fsCtl, "close %s", box->fs) < 0 && opened)
bye("can't talk to mail server");
for(m = box->msgs; m != nil; m = next){
next = m->next;
freeMsg(m);
}
free(box->name);
free(box->fs);
free(box->fsDir);
free(box->imp);
free(box);
}
int
deleteMsgs(Box *box)
{
Msg *m;
char buf[BufSize], *p, *start;
int ok;
if(!box->writable)
return 0;
ok = 1;
start = seprint(buf, buf + sizeof(buf), "delete %s", box->fs);
p = start;
for(m = box->msgs; m != nil; m = m->next){
if((m->flags & MDeleted) && !m->expunged){
m->expunged = 1;
p = seprint(p, buf + sizeof(buf), " %lud", m->id);
if(p + 32 >= buf + sizeof(buf)){
if(write(fsCtl, buf, p - buf) < 0)
bye("can't talk to mail server");
p = start;
}
}
}
if(p != start && write(fsCtl, buf, p - buf) < 0)
bye("can't talk to mail server");
return ok;
}
int
expungeMsgs(Box *box, int send)
{
Msg *m, *next, *last;
ulong n;
n = 0;
last = nil;
for(m = box->msgs; m != nil; m = next){
m->seq -= n;
next = m->next;
if(m->expunged){
if(send)
Bprint(&bout, "* %lud expunge\r\n", m->seq);
if(m->flags & MRecent)
box->recent--;
n++;
if(last == nil)
box->msgs = next;
else
last->next = next;
freeMsg(m);
}else
last = m;
}
if(n){
box->max -= n;
box->dirtyImp = 1;
}
return n;
}
static void
fsInit(void)
{
if(fsCtl >= 0)
return;
fsCtl = open("/mail/fs/ctl", ORDWR);
if(fsCtl < 0)
bye("can't open mail file system");
if(fprint(fsCtl, "close mbox") < 0)
bye("can't initialize mail file system");
}
static char *stoplist[] =
{
"mbox",
"pipeto",
"forward",
"names",
"pipefrom",
"headers",
"imap.ok",
0
};
enum {
Maxokbytes	= 4096,
Maxfolders	= Maxokbytes / 4,
};
static char *folders[Maxfolders];
static char *folderbuff;
static void
readokfolders(void)
{
int fd, nr;
fd = open("imap.ok", OREAD);
if(fd < 0)
return;
folderbuff = malloc(Maxokbytes);
if(folderbuff == nil) {
close(fd);
return;
}
nr = read(fd, folderbuff, Maxokbytes-1);
close(fd);
if(nr < 0){
free(folderbuff);
folderbuff = nil;
return;
}
folderbuff[nr] = 0;
tokenize(folderbuff, folders, nelem(folders));
}
int
okMbox(char *path)
{
char *name;
int i;
if(folderbuff == nil && access("imap.ok", AREAD) == 0)
readokfolders();
name = strrchr(path, '/');
if(name == nil)
name = path;
else
name++;
if(folderbuff != nil){
for(i = 0; i < nelem(folders) && folders[i] != nil; i++)
if(cistrcmp(folders[i], name) == 0)
return 1;
return 0;
}
if(strlen(name) + STRLEN(".imp") >= MboxNameLen)
return 0;
for(i = 0; stoplist[i]; i++)
if(strcmp(name, stoplist[i]) == 0)
return 0;
if(isprefix("L.", name) || isprefix("imap-tmp.", name)
|| issuffix(".imp", name)
|| strcmp("imap.subscribed", name) == 0
|| isdotdot(name) || name[0] == '/')
return 0;
return 1;
}