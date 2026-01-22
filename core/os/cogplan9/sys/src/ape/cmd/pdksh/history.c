#include "sh.h"
#include "ksh_stat.h"
#ifdef HISTORY
# ifdef EASY_HISTORY
# ifndef HISTFILE
# ifdef OS2
# define HISTFILE "history.ksh"
# else
# define HISTFILE ".pdksh_history"
# endif
# endif
# else
# include <sys/file.h>
# include <sys/mman.h>
static int histfd;
static int hsize;
static int hist_count_lines ARGS((unsigned char *, int));
static int hist_shrink ARGS((unsigned char *, int));
static unsigned char *hist_skip_back ARGS((unsigned char *,int *,int));
static void histload ARGS((Source *, unsigned char *, int));
static void histinsert ARGS((Source *, int, unsigned char *));
static void writehistfile ARGS((int, char *));
static int sprinkle ARGS((int));
# ifdef MAP_FILE
# define MAP_FLAGS (MAP_FILE|MAP_PRIVATE)
# else
# define MAP_FLAGS MAP_PRIVATE
# endif
# endif
static int hist_execute ARGS((char *cmd));
static int hist_replace ARGS((char **hp, const char *pat, const char *rep,
int global));
static char **hist_get ARGS((const char *str, int approx, int allow_cur));
static char **hist_get_newest ARGS((int allow_cur));
static char **hist_get_oldest ARGS(());
static void histbackup ARGS((void));
static char **current;
static int curpos;
static char *hname;
static int hstarted;
static Source *hist_source;
int
c_fc(wp)
char **wp;
{
struct shf *shf;
struct temp UNINITIALIZED(*tf);
char *p, *editor = (char *) 0;
int gflag = 0, lflag = 0, nflag = 0, sflag = 0, rflag = 0;
int optc;
char *first = (char *) 0, *last = (char *) 0;
char **hfirst, **hlast, **hp;
while ((optc = ksh_getopt(wp, &builtin_opt, "e:glnrs0,1,2,3,4,5,6,7,8,9,")) != EOF)
switch (optc) {
case 'e':
p = builtin_opt.optarg;
if (strcmp(p, "-") == 0)
sflag++;
else {
editor = str_nsave(p, strlen(p) + 4, ATEMP);
strcat(editor, " $_");
}
break;
case 'g':
gflag++;
break;
case 'l':
lflag++;
break;
case 'n':
nflag++;
break;
case 'r':
rflag++;
break;
case 's':
sflag++;
break;
case '0': case '1': case '2': case '3': case '4':
case '5': case '6': case '7': case '8': case '9':
p = shf_smprintf("-%c%s",
optc, builtin_opt.optarg);
if (!first)
first = p;
else if (!last)
last = p;
else {
bi_errorf("too many arguments");
return 1;
}
break;
case '?':
return 1;
}
wp += builtin_opt.optind;
if (sflag) {
char *pat = (char *) 0, *rep = (char *) 0;
if (editor || lflag || nflag || rflag) {
bi_errorf("can't use -e, -l, -n, -r with -s (-e -)");
return 1;
}
if (*wp && **wp && (p = strchr(*wp + 1, '='))) {
pat = str_save(*wp, ATEMP);
p = pat + (p - *wp);
*p++ = '\0';
rep = p;
wp++;
}
if (!first && (first = *wp))
wp++;
if (last || *wp) {
bi_errorf("too many arguments");
return 1;
}
hp = first ? hist_get(first, FALSE, FALSE)
: hist_get_newest(FALSE);
if (!hp)
return 1;
return hist_replace(hp, pat, rep, gflag);
}
if (editor && (lflag || nflag)) {
bi_errorf("can't use -l, -n with -e");
return 1;
}
if (!first && (first = *wp))
wp++;
if (!last && (last = *wp))
wp++;
if (*wp) {
bi_errorf("too many arguments");
return 1;
}
if (!first) {
hfirst = lflag ? hist_get("-16", TRUE, TRUE)
: hist_get_newest(FALSE);
if (!hfirst)
return 1;
hlast = hist_get_newest(FALSE);
} else {
hfirst = hist_get(first, (lflag || last) ? TRUE : FALSE,
lflag ? TRUE : FALSE);
if (!hfirst)
return 1;
hlast = last ? hist_get(last, TRUE, lflag ? TRUE : FALSE)
: (lflag ? hist_get_newest(FALSE) : hfirst);
if (!hlast)
return 1;
}
if (hfirst > hlast) {
char **temp;
temp = hfirst; hfirst = hlast; hlast = temp;
rflag = !rflag;
}
if (lflag) {
char *s, *t;
const char *nfmt = nflag ? "\t" : "%d\t";
for (hp = rflag ? hlast : hfirst;
hp >= hfirst && hp <= hlast; hp += rflag ? -1 : 1)
{
shf_fprintf(shl_stdout, nfmt,
hist_source->line - (int) (histptr - hp));
for (s = *hp; (t = strchr(s, '\n')); s = t)
shf_fprintf(shl_stdout, "%.*s\t", ++t - s, s);
shf_fprintf(shl_stdout, "%s\n", s);
}
shf_flush(shl_stdout);
return 0;
}
tf = maketemp(ATEMP, TT_HIST_EDIT, &e->temps);
if (!(shf = tf->shf)) {
bi_errorf("cannot create temp file %s - %s",
tf->name, strerror(errno));
return 1;
}
for (hp = rflag ? hlast : hfirst;
hp >= hfirst && hp <= hlast; hp += rflag ? -1 : 1)
shf_fprintf(shf, "%s\n", *hp);
if (shf_close(shf) == EOF) {
bi_errorf("error writing temporary file - %s", strerror(errno));
return 1;
}
setstr(local("_", FALSE), tf->name, KSH_RETURN_ERROR);
{
Source *sold = source;
int ret;
ret = command(editor ? editor : "${FCEDIT:-/bin/ed} $_");
source = sold;
if (ret)
return ret;
}
{
struct stat statb;
XString xs;
char *xp;
int n;
if (!(shf = shf_open(tf->name, O_RDONLY, 0, 0))) {
bi_errorf("cannot open temp file %s", tf->name);
return 1;
}
n = fstat(shf_fileno(shf), &statb) < 0 ? 128
: statb.st_size + 1;
Xinit(xs, xp, n, hist_source->areap);
while ((n = shf_read(xp, Xnleft(xs, xp), shf)) > 0) {
xp += n;
if (Xnleft(xs, xp) <= 0)
XcheckN(xs, xp, Xlength(xs, xp));
}
if (n < 0) {
bi_errorf("error reading temp file %s - %s",
tf->name, strerror(shf_errno(shf)));
shf_close(shf);
return 1;
}
shf_close(shf);
*xp = '\0';
strip_nuls(Xstring(xs, xp), Xlength(xs, xp));
return hist_execute(Xstring(xs, xp));
}
}
static int
hist_execute(cmd)
char *cmd;
{
Source *sold;
int ret;
char *p, *q;
histbackup();
for (p = cmd; p; p = q) {
if ((q = strchr(p, '\n'))) {
*q++ = '\0';
if (!*q)
q = (char *) 0;
}
#ifdef EASY_HISTORY
if (p != cmd)
histappend(p, TRUE);
else
#endif
histsave(++(hist_source->line), p, 1);
shellf("%s\n", p);
if ((p = q))
q[-1] = '\n';
}
sold = source;
ret = command(cmd);
source = sold;
return ret;
}
static int
hist_replace(hp, pat, rep, global)
char **hp;
const char *pat;
const char *rep;
int global;
{
char *line;
if (!pat)
line = str_save(*hp, ATEMP);
else {
char *s, *s1;
int pat_len = strlen(pat);
int rep_len = strlen(rep);
int len;
XString xs;
char *xp;
int any_subst = 0;
Xinit(xs, xp, 128, ATEMP);
for (s = *hp; (s1 = strstr(s, pat))
&& (!any_subst || global) ; s = s1 + pat_len)
{
any_subst = 1;
len = s1 - s;
XcheckN(xs, xp, len + rep_len);
memcpy(xp, s, len);
xp += len;
memcpy(xp, rep, rep_len);
xp += rep_len;
}
if (!any_subst) {
bi_errorf("substitution failed");
return 1;
}
len = strlen(s) + 1;
XcheckN(xs, xp, len);
memcpy(xp, s, len);
xp += len;
line = Xclose(xs, xp);
}
return hist_execute(line);
}
static char **
hist_get(str, approx, allow_cur)
const char *str;
int approx;
int allow_cur;
{
char **hp = (char **) 0;
int n;
if (getn(str, &n)) {
hp = histptr + (n < 0 ? n : (n - hist_source->line));
if (hp < history) {
if (approx)
hp = hist_get_oldest();
else {
bi_errorf("%s: not in history", str);
hp = (char **) 0;
}
} else if (hp > histptr) {
if (approx)
hp = hist_get_newest(allow_cur);
else {
bi_errorf("%s: not in history", str);
hp = (char **) 0;
}
} else if (!allow_cur && hp == histptr) {
bi_errorf("%s: invalid range", str);
hp = (char **) 0;
}
} else {
int anchored = *str == '?' ? (++str, 0) : 1;
n = findhist(histptr - history - 1, 0, str, anchored);
if (n < 0) {
bi_errorf("%s: not in history", str);
hp = (char **) 0;
} else
hp = &history[n];
}
return hp;
}
static char **
hist_get_newest(allow_cur)
int allow_cur;
{
if (histptr < history || (!allow_cur && histptr == history)) {
bi_errorf("no history (yet)");
return (char **) 0;
}
if (allow_cur)
return histptr;
return histptr - 1;
}
static char **
hist_get_oldest()
{
if (histptr <= history) {
bi_errorf("no history (yet)");
return (char **) 0;
}
return history;
}
static void
histbackup()
{
static int last_line = -1;
if (histptr >= history && last_line != hist_source->line) {
hist_source->line--;
afree((void*)*histptr, APERM);
histptr--;
last_line = hist_source->line;
}
}
char **
histpos()
{
return current;
}
int
histN()
{
return curpos;
}
int
histnum(n)
int n;
{
int last = histptr - history;
if (n < 0 || n >= last) {
current = histptr;
curpos = last;
return last;
} else {
current = &history[n];
curpos = n;
return n;
}
}
int
findhist(start, fwd, str, anchored)
int start;
int fwd;
const char *str;
int anchored;
{
char **hp;
int maxhist = histptr - history;
int incr = fwd ? 1 : -1;
int len = strlen(str);
if (start < 0 || start >= maxhist)
start = maxhist;
hp = &history[start];
for (; hp >= history && hp <= histptr; hp += incr)
if ((anchored && strncmp(*hp, str, len) == 0)
|| (!anchored && strstr(*hp, str)))
return hp - history;
return -1;
}
void
sethistsize(n)
int n;
{
if (n > 0 && n != histsize) {
int cursize = histptr - history;
if (n < cursize) {
memmove(history, histptr - n, n * sizeof(char *));
cursize = n;
}
history = (char **)aresize(history, n*sizeof(char *), APERM);
histsize = n;
histptr = history + cursize;
}
}
void
sethistfile(name)
const char *name;
{
if (hstarted == 0)
return;
if (hname && strcmp(hname, name) == 0)
return;
# ifdef EASY_HISTORY
if (hname) {
afree(hname, APERM);
hname = NULL;
}
# else
if (histfd) {
(void) close(histfd);
histfd = 0;
hsize = 0;
afree(hname, APERM);
hname = NULL;
histptr = history - 1;
hist_source->line = 0;
}
# endif
hist_init(hist_source);
}
void
init_histvec()
{
if (history == (char **)NULL) {
histsize = HISTORYSIZE;
history = (char **)alloc(histsize*sizeof (char *), APERM);
histptr = history - 1;
}
}
# ifdef EASY_HISTORY
void
histsave(lno, cmd, dowrite)
int lno;
const char *cmd;
int dowrite;
{
register char **hp = histptr;
char *cp;
if (++hp >= history + histsize) {
afree((void*)history[0], APERM);
memmove(history, history + 1,
sizeof(history[0]) * (histsize - 1));
hp = &history[histsize - 1];
}
*hp = str_save(cmd, APERM);
cp = *hp + strlen(*hp);
if (cp > *hp && cp[-1] == '\n')
cp[-1] = '\0';
histptr = hp;
}
void
histappend(cmd, nl_separate)
const char *cmd;
int nl_separate;
{
int hlen, clen;
char *p;
hlen = strlen(*histptr);
clen = strlen(cmd);
if (clen > 0 && cmd[clen-1] == '\n')
clen--;
p = *histptr = (char *) aresize(*histptr, hlen + clen + 2, APERM);
p += hlen;
if (nl_separate)
*p++ = '\n';
memcpy(p, cmd, clen);
p[clen] = '\0';
}
void
hist_init(s)
Source *s;
{
char *f;
FILE *fh;
if (Flag(FTALKING) == 0)
return;
hstarted = 1;
hist_source = s;
if ((f = str_val(global("HISTFILE"))) == NULL || *f == '\0') {
# if 1
hname = NULL;
return;
# else
char *home = str_val(global("HOME"));
int len;
if (home == NULL)
home = null;
f = HISTFILE;
hname = alloc(len = strlen(home) + strlen(f) + 2, APERM);
shf_snprintf(hname, len, "%s/%s", home, f);
# endif
} else
hname = str_save(f, APERM);
if ((fh = fopen(hname, "r"))) {
int pos = 0, nread = 0;
int contin = 0;
char *end;
char hline[LINE + 1];
while (1) {
if (pos >= nread) {
pos = 0;
nread = fread(hline, 1, LINE, fh);
if (nread <= 0)
break;
hline[nread] = '\0';
}
end = strchr(hline + pos, 0);
if (contin)
histappend(hline + pos, 0);
else {
hist_source->line++;
histsave(0, hline + pos, 0);
}
pos = end - hline + 1;
contin = end == &hline[nread];
}
fclose(fh);
}
}
void
hist_finish()
{
static int once;
FILE *fh;
register int i;
register char **hp;
if (once++)
return;
i = histptr - history;
if (i >= histsize)
hp = &histptr[-histsize];
else
hp = history;
if (hname && (fh = fopen(hname, "w")))
{
for (i = 0; hp + i <= histptr && hp[i]; i++)
fprintf(fh, "%s%c", hp[i], '\0');
fclose(fh);
}
}
# else
void
histsave(lno, cmd, dowrite)
int lno;
const char *cmd;
int dowrite;
{
register char **hp;
char *c, *cp;
c = str_save(cmd, APERM);
if ((cp = strchr(c, '\n')) != NULL)
*cp = '\0';
if (histfd && dowrite)
writehistfile(lno, c);
hp = histptr;
if (++hp >= history + histsize) {
afree((void*)*history, APERM);
for (hp = history; hp < history + histsize - 1; hp++)
hp[0] = hp[1];
}
*hp = c;
histptr = hp;
}
# define HMAGIC1 0xab
# define HMAGIC2 0xcd
# define COMMAND 0xff
void
hist_init(s)
Source *s;
{
unsigned char *base;
int lines;
int fd;
if (Flag(FTALKING) == 0)
return;
hstarted = 1;
hist_source = s;
hname = str_val(global("HISTFILE"));
if (hname == NULL)
return;
hname = str_save(hname, APERM);
retry:
if ((fd = open(hname, O_RDWR|O_CREAT|O_APPEND, 0600)) < 0)
return;
histfd = savefd(fd, 0);
(void) flock(histfd, LOCK_EX);
hsize = lseek(histfd, 0L, SEEK_END);
if (hsize == 0) {
if (sprinkle(histfd)) {
hist_finish();
return;
}
}
else if (hsize > 0) {
base = (unsigned char *)mmap(0, hsize, PROT_READ, MAP_FLAGS, histfd, 0);
if ((int)base == -1 || *base != HMAGIC1 || base[1] != HMAGIC2) {
if ((int)base != -1)
munmap((caddr_t)base, hsize);
hist_finish();
unlink(hname);
goto retry;
}
if (hsize > 2) {
lines = hist_count_lines(base+2, hsize-2);
if (lines > histsize) {
if (hist_shrink(base, hsize))
unlink(hname);
munmap((caddr_t)base, hsize);
hist_finish();
goto retry;
}
}
histload(hist_source, base+2, hsize-2);
munmap((caddr_t)base, hsize);
}
(void) flock(histfd, LOCK_UN);
hsize = lseek(histfd, 0L, SEEK_END);
}
typedef enum state {
shdr,
sline,
sn1,
sn2, sn3, sn4,
} State;
static int
hist_count_lines(base, bytes)
register unsigned char *base;
register int bytes;
{
State state = shdr;
register lines = 0;
while (bytes--) {
switch (state)
{
case shdr:
if (*base == COMMAND)
state = sn1;
break;
case sn1:
state = sn2; break;
case sn2:
state = sn3; break;
case sn3:
state = sn4; break;
case sn4:
state = sline; break;
case sline:
if (*base == '\0')
lines++, state = shdr;
}
base++;
}
return lines;
}
static int
hist_shrink(oldbase, oldbytes)
unsigned char *oldbase;
int oldbytes;
{
int fd;
char nfile[1024];
struct stat statb;
unsigned char *nbase = oldbase;
int nbytes = oldbytes;
nbase = hist_skip_back(nbase, &nbytes, histsize);
if (nbase == NULL)
return 1;
if (nbase == oldbase)
return 0;
(void) shf_snprintf(nfile, sizeof(nfile), "%s.%d", hname, procpid);
if ((fd = creat(nfile, 0600)) < 0)
return 1;
if (sprinkle(fd)) {
close(fd);
unlink(nfile);
return 1;
}
if (write(fd, nbase, nbytes) != nbytes) {
close(fd);
unlink(nfile);
return 1;
}
if (fstat(histfd, &statb) >= 0)
fchown(fd, statb.st_uid, statb.st_gid);
close(fd);
if (rename(nfile, hname) < 0)
return 1;
return 0;
}
static unsigned char *
hist_skip_back(base, bytes, no)
unsigned char *base;
int *bytes;
int no;
{
register int lines = 0;
register unsigned char *ep;
for (ep = base + *bytes; --ep > base; ) {
for (; ep > base && *ep != COMMAND; ep--)
;
if (ep == base)
break;
if (++lines == no) {
*bytes = *bytes - ((char *)ep - (char *)base);
return ep;
}
}
return NULL;
}
static void
histload(s, base, bytes)
Source *s;
register unsigned char *base;
register int bytes;
{
State state;
int lno;
unsigned char *line;
for (state = shdr; bytes-- > 0; base++) {
switch (state) {
case shdr:
if (*base == COMMAND)
state = sn1;
break;
case sn1:
lno = (((*base)&0xff)<<24);
state = sn2;
break;
case sn2:
lno |= (((*base)&0xff)<<16);
state = sn3;
break;
case sn3:
lno |= (((*base)&0xff)<<8);
state = sn4;
break;
case sn4:
lno |= (*base)&0xff;
line = base+1;
state = sline;
break;
case sline:
if (*base == '\0') {
if (histptr >= history && lno-1 != s->line) {
histinsert(s, lno, line);
}
else {
s->line = lno;
histsave(lno, (char *)line, 0);
}
state = shdr;
}
}
}
}
static void
histinsert(s, lno, line)
Source *s;
int lno;
unsigned char *line;
{
register char **hp;
if (lno >= s->line-(histptr-history) && lno <= s->line) {
hp = &histptr[lno-s->line];
if (*hp)
afree((void*)*hp, APERM);
*hp = str_save((char *)line, APERM);
}
}
static void
writehistfile(lno, cmd)
int lno;
char *cmd;
{
int sizenow;
unsigned char *base;
unsigned char *new;
int bytes;
char hdr[5];
(void) flock(histfd, LOCK_EX);
sizenow = lseek(histfd, 0L, SEEK_END);
if (sizenow != hsize) {
if (sizenow > hsize) {
bytes = sizenow - hsize;
base = (unsigned char *)mmap(0, sizenow, PROT_READ, MAP_FLAGS, histfd, 0);
if ((int)base == -1)
goto bad;
new = base + hsize;
if (*new != COMMAND) {
munmap((caddr_t)base, sizenow);
goto bad;
}
hist_source->line--;
histload(hist_source, new, bytes);
hist_source->line++;
lno = hist_source->line;
munmap((caddr_t)base, sizenow);
hsize = sizenow;
} else {
goto bad;
}
}
hdr[0] = COMMAND;
hdr[1] = (lno>>24)&0xff;
hdr[2] = (lno>>16)&0xff;
hdr[3] = (lno>>8)&0xff;
hdr[4] = lno&0xff;
(void) write(histfd, hdr, 5);
(void) write(histfd, cmd, strlen(cmd)+1);
hsize = lseek(histfd, 0L, SEEK_END);
(void) flock(histfd, LOCK_UN);
return;
bad:
hist_finish();
}
void
hist_finish()
{
(void) flock(histfd, LOCK_UN);
(void) close(histfd);
histfd = 0;
}
static int
sprinkle(fd)
int fd;
{
static char mag[] = { HMAGIC1, HMAGIC2 };
return(write(fd, mag, 2) != 2);
}
# endif
#else
void
init_histvec()
{
}
void
hist_init(s)
Source *s;
{
}
void
hist_finish()
{
}
void
histsave(lno, cmd, dowrite)
int lno;
const char *cmd;
int dowrite;
{
errorf("history not enabled");
}
#endif