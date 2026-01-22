#include "sh.h"
#include <pwd.h>
#include "ksh_dir.h"
#include "ksh_stat.h"
typedef struct Expand {
const char *str;
union {
const char **strv;
struct shf *shf;
} u;
struct tbl *var;
short	split;
} Expand;
#define	XBASE		0
#define	XSUB		1
#define	XARGSEP		2
#define	XARG		3
#define	XCOM		4
#define XNULLSUB	5
#define IFS_WORD	0
#define IFS_WS		1
#define IFS_NWS		2
static	int	varsub ARGS((Expand *xp, char *sp, char *word, int *stypep, int *slenp));
static	int	comsub ARGS((Expand *xp, char *cp));
static	char   *trimsub ARGS((char *str, char *pat, int how));
static	void	glob ARGS((char *cp, XPtrV *wp, int markdirs));
static	void	globit ARGS((XString *xs, char **xpp, char *sp, XPtrV *wp,
int check));
static char	*maybe_expand_tilde ARGS((char *p, XString *dsp, char **dpp,
int isassign));
static	char   *tilde ARGS((char *acp));
static	char   *homedir ARGS((char *name));
#ifdef BRACE_EXPAND
static void	alt_expand ARGS((XPtrV *wp, char *start, char *exp_start,
char *end, int fdo));
#endif
char *
substitute(cp, f)
const char *cp;
int f;
{
struct source *s, *sold;
sold = source;
s = pushs(SWSTR, ATEMP);
s->start = s->str = cp;
source = s;
if (yylex(ONEWORD) != LWORD)
internal_errorf(1, "substitute");
source = sold;
afree(s, ATEMP);
return evalstr(yylval.cp, f);
}
char **
eval(ap, f)
register char **ap;
int f;
{
XPtrV w;
if (*ap == NULL)
return ap;
XPinit(w, 32);
XPput(w, NULL);
#ifdef	SHARPBANG
XPput(w, NULL);
#endif
while (*ap != NULL)
expand(*ap++, &w, f);
XPput(w, NULL);
#ifdef	SHARPBANG
return (char **) XPclose(w) + 2;
#else
return (char **) XPclose(w) + 1;
#endif
}
char *
evalstr(cp, f)
char *cp;
int f;
{
XPtrV w;
XPinit(w, 1);
expand(cp, &w, f);
cp = (XPsize(w) == 0) ? null : (char*) *XPptrv(w);
XPfree(w);
return cp;
}
char *
evalonestr(cp, f)
register char *cp;
int f;
{
XPtrV w;
XPinit(w, 1);
expand(cp, &w, f);
switch (XPsize(w)) {
case 0:
cp = null;
break;
case 1:
cp = (char*) *XPptrv(w);
break;
default:
cp = evalstr(cp, f&~DOGLOB);
break;
}
XPfree(w);
return cp;
}
typedef struct SubType {
short	stype;
short	base;
short	f;
struct tbl *var;
short	quote;
struct SubType *prev;
struct SubType *next;
} SubType;
void
expand(cp, wp, f)
char *cp;
register XPtrV *wp;
int f;
{
register int UNINITIALIZED(c);
register int type;
register int quote = 0;
XString ds;
register char *dp, *sp;
int fdo, word;
int doblank;
Expand x;
SubType st_head, *st;
int UNINITIALIZED(newlines);
int saw_eq, tilde_ok;
int make_magic;
if (cp == NULL)
internal_errorf(1, "expand(NULL)");
if ((f & DOVACHECK) && is_wdvarassign(cp)) {
f &= ~(DOVACHECK|DOBLANK|DOGLOB|DOTILDE);
f |= DOASNTILDE;
}
if (Flag(FNOGLOB))
f &= ~DOGLOB;
if (Flag(FMARKDIRS))
f |= DOMARKDIRS;
#ifdef BRACE_EXPAND
if (Flag(FBRACEEXPAND) && (f & DOGLOB))
f |= DOBRACE_;
#endif
Xinit(ds, dp, 128, ATEMP);
type = XBASE;
sp = cp;
fdo = 0;
saw_eq = 0;
tilde_ok = (f & (DOTILDE|DOASNTILDE)) ? 1 : 0;
doblank = 0;
make_magic = 0;
word = (f&DOBLANK) ? IFS_WS : IFS_WORD;
st_head.next = (SubType *) 0;
st = &st_head;
while (1) {
Xcheck(ds, dp);
switch (type) {
case XBASE:
c = *sp++;
switch (c) {
case EOS:
c = 0;
break;
case CHAR:
c = *sp++;
break;
case QCHAR:
quote |= 2;
c = *sp++;
break;
case OQUOTE:
word = IFS_WORD;
tilde_ok = 0;
quote = 1;
continue;
case CQUOTE:
quote = 0;
continue;
case COMSUB:
tilde_ok = 0;
if (f & DONTRUNCOMMAND) {
word = IFS_WORD;
*dp++ = '$'; *dp++ = '(';
while (*sp != '\0') {
Xcheck(ds, dp);
*dp++ = *sp++;
}
*dp++ = ')';
} else {
type = comsub(&x, sp);
if (type == XCOM && (f&DOBLANK))
doblank++;
sp = strchr(sp, 0) + 1;
newlines = 0;
}
continue;
case EXPRSUB:
word = IFS_WORD;
tilde_ok = 0;
if (f & DONTRUNCOMMAND) {
*dp++ = '$'; *dp++ = '('; *dp++ = '(';
while (*sp != '\0') {
Xcheck(ds, dp);
*dp++ = *sp++;
}
*dp++ = ')'; *dp++ = ')';
} else {
struct tbl v;
char *p;
v.flag = DEFINED|ISSET|INTEGER;
v.type = 10;
v.name[0] = '\0';
v_evaluate(&v, substitute(sp, 0),
KSH_UNWIND_ERROR);
sp = strchr(sp, 0) + 1;
for (p = str_val(&v); *p; ) {
Xcheck(ds, dp);
*dp++ = *p++;
}
}
continue;
case OSUBST:
{
char *varname = ++sp;
int stype;
int slen;
sp = strchr(sp, '\0') + 1;
type = varsub(&x, varname, sp, &stype, &slen);
if (type < 0) {
char endc;
char *str, *end;
end = (char *) wdscan(sp, CSUBST);
endc = *end;
*end = EOS;
str = snptreef((char *) 0, 64, "%S",
varname - 1);
*end = endc;
errorf("%s: bad substitution", str);
}
if (f&DOBLANK)
doblank++;
tilde_ok = 0;
if (type == XBASE) {
if (!st->next) {
SubType *newst;
newst = (SubType *) alloc(
sizeof(SubType), ATEMP);
newst->next = (SubType *) 0;
newst->prev = st;
st->next = newst;
}
st = st->next;
st->stype = stype;
st->base = Xsavepos(ds, dp);
st->f = f;
st->var = x.var;
st->quote = quote;
if (stype)
sp += slen;
switch (stype & 0x7f) {
case '#':
case '%':
f = DOPAT | (f&DONTRUNCOMMAND)
| DOTEMP_;
quote = 0;
*dp++ = MAGIC;
*dp++ = '@' + 0x80;
break;
case '=':
if (!(x.var->flag & INTEGER))
f |= DOASNTILDE|DOTILDE;
f |= DOTEMP_;
f &= ~(DOBLANK|DOGLOB|DOBRACE_);
tilde_ok = 1;
break;
case '?':
f &= ~DOBLANK;
f |= DOTEMP_;
default:
tilde_ok = 1;
f |= DOTILDE;
}
} else
sp = (char *) wdscan(sp, CSUBST);
continue;
}
case CSUBST:
sp++;
tilde_ok = 0;
*dp = '\0';
quote = st->quote;
f = st->f;
if (f&DOBLANK)
doblank--;
switch (st->stype&0x7f) {
case '#':
case '%':
*dp++ = MAGIC; *dp++ = ')'; *dp = '\0';
dp = Xrestpos(ds, dp, st->base);
x.str = trimsub(str_val(st->var),
dp, st->stype);
type = XSUB;
if (f&DOBLANK)
doblank++;
st = st->prev;
continue;
case '=':
dp = Xrestpos(ds, dp, st->base);
setstr(st->var, debunk(
(char *) alloc(strlen(dp) + 1,
ATEMP), dp),
KSH_UNWIND_ERROR);
x.str = str_val(st->var);
type = XSUB;
if (f&DOBLANK)
doblank++;
st = st->prev;
continue;
case '?':
{
char *s = Xrestpos(ds, dp, st->base);
errorf("%s: %s", st->var->name,
dp == s ?
"parameter null or not set"
: (debunk(s, s), s));
}
}
st = st->prev;
type = XBASE;
continue;
case OPAT:
make_magic = 1;
c = *sp++ + 0x80;
break;
case SPAT:
make_magic = 1;
c = '|';
break;
case CPAT:
make_magic = 1;
c =  ')';
break;
}
break;
case XNULLSUB:
type = XBASE;
if (f&DOBLANK) {
doblank--;
if (dp == Xstring(ds, dp))
word = IFS_WS;
}
continue;
case XSUB:
if ((c = *x.str++) == 0) {
type = XBASE;
if (f&DOBLANK)
doblank--;
continue;
}
break;
case XARGSEP:
type = XARG;
quote = 1;
case XARG:
if ((c = *x.str++) == '\0') {
if (quote && x.split)
word = IFS_WORD;
if ((x.str = *x.u.strv++) == NULL) {
type = XBASE;
if (f&DOBLANK)
doblank--;
continue;
}
c = ifs0;
if (c == 0) {
if (quote && !x.split)
continue;
c = ' ';
}
if (quote && x.split) {
type = XARGSEP;
quote = 0;
}
}
break;
case XCOM:
if (newlines) {
c = '\n';
--newlines;
} else {
while ((c = shf_getc(x.u.shf)) == 0 || c == '\n')
if (c == '\n')
newlines++;
if (newlines && c != EOF) {
shf_ungetc(c, x.u.shf);
c = '\n';
--newlines;
}
}
if (c == EOF) {
newlines = 0;
shf_close(x.u.shf);
if (x.split)
subst_exstat = waitlast();
type = XBASE;
if (f&DOBLANK)
doblank--;
continue;
}
break;
}
if (c == 0 || (!quote && (f & DOBLANK) && doblank && !make_magic
&& ctype(c, C_IFS)))
{
if (word == IFS_WORD
|| (!ctype(c, C_IFSWS) && (c || word == IFS_NWS)))
{
char *p;
*dp++ = '\0';
p = Xclose(ds, dp);
#ifdef BRACE_EXPAND
if (fdo & DOBRACE_)
alt_expand(wp, p, p,
p + Xlength(ds, (dp - 1)),
fdo | (f & DOMARKDIRS));
else
#endif
if (fdo & DOGLOB)
glob(p, wp, f & DOMARKDIRS);
else if ((f & DOPAT) || !(fdo & DOMAGIC_))
XPput(*wp, p);
else
XPput(*wp, debunk(p, p));
fdo = 0;
saw_eq = 0;
tilde_ok = (f & (DOTILDE|DOASNTILDE)) ? 1 : 0;
if (c != 0)
Xinit(ds, dp, 128, ATEMP);
}
if (c == 0)
return;
if (word != IFS_NWS)
word = ctype(c, C_IFSWS) ? IFS_WS : IFS_NWS;
} else {
tilde_ok <<= 1;
if (!quote)
switch (c) {
case '[':
case NOT:
case '-':
case ']':
if (f & (DOPAT | DOGLOB)) {
fdo |= DOMAGIC_;
if (c == '[')
fdo |= f & DOGLOB;
*dp++ = MAGIC;
}
break;
case '*':
case '?':
if (f & (DOPAT | DOGLOB)) {
fdo |= DOMAGIC_ | (f & DOGLOB);
*dp++ = MAGIC;
}
break;
#ifdef BRACE_EXPAND
case OBRACE:
case ',':
case CBRACE:
if ((f & DOBRACE_) && (c == OBRACE
|| (fdo & DOBRACE_)))
{
fdo |= DOBRACE_|DOMAGIC_;
*dp++ = MAGIC;
}
break;
#endif
case '=':
if (!(f & DOTEMP_) && !saw_eq) {
saw_eq = 1;
tilde_ok = 1;
}
break;
case PATHSEP:
if (!(f & DOTEMP_) && (f & DOASNTILDE))
tilde_ok = 1;
break;
case '~':
if (type == XBASE
&& (f & (DOTILDE|DOASNTILDE))
&& (tilde_ok & 2))
{
char *p, *dp_x;
dp_x = dp;
p = maybe_expand_tilde(sp,
&ds, &dp_x,
f & DOASNTILDE);
if (p) {
if (dp != dp_x)
word = IFS_WORD;
dp = dp_x;
sp = p;
continue;
}
}
break;
}
else
quote &= ~2;
if (make_magic) {
make_magic = 0;
fdo |= DOMAGIC_ | (f & DOGLOB);
*dp++ = MAGIC;
} else if (ISMAGIC(c)) {
fdo |= DOMAGIC_;
*dp++ = MAGIC;
}
*dp++ = c;
word = IFS_WORD;
}
}
}
static int
varsub(xp, sp, word, stypep, slenp)
Expand *xp;
char *sp;
char *word;
int *stypep;
int *slenp;
{
int c;
int state;
int stype;
int slen;
char *p;
struct tbl *vp;
if (sp[0] == '\0')
return -1;
xp->var = (struct tbl *) 0;
if (sp[0] == '#' && (c = sp[1]) != '\0') {
int zero_ok = 0;
if (*word != CSUBST)
return -1;
sp++;
if ((p=strchr(sp,'[')) && (p[1]=='*'||p[1]=='@') && p[2]==']') {
int n = 0;
int max = 0;
vp = global(arrayname(sp));
if (vp->flag & (ISSET|ARRAY))
zero_ok = 1;
for (; vp; vp = vp->u.array)
if (vp->flag & ISSET) {
max = vp->index + 1;
n++;
}
c = n;
} else if (c == '*' || c == '@')
c = e->loc->argc;
else {
p = str_val(global(sp));
zero_ok = p != null;
c = strlen(p);
}
if (Flag(FNOUNSET) && c == 0 && !zero_ok)
errorf("%s: parameter not set", sp);
*stypep = 0;
xp->str = str_save(ulton((unsigned long)c, 10), ATEMP);
return XSUB;
}
stype = 0;
c = word[slen = 0] == CHAR ? word[1] : 0;
if (c == ':') {
slen += 2;
stype = 0x80;
c = word[slen + 0] == CHAR ? word[slen + 1] : 0;
}
if (ctype(c, C_SUBOP1)) {
slen += 2;
stype |= c;
} else if (ctype(c, C_SUBOP2)) {
slen += 2;
stype = c;
if (word[slen + 0] == CHAR && c == word[slen + 1]) {
stype |= 0x80;
slen += 2;
}
} else if (stype)
return -1;
if (!stype && *word != CSUBST)
return -1;
*stypep = stype;
*slenp = slen;
c = sp[0];
if (c == '*' || c == '@') {
switch (stype & 0x7f) {
case '=':
case '%':
case '#':
return -1;
}
if (e->loc->argc == 0) {
xp->str = null;
state = c == '@' ? XNULLSUB : XSUB;
} else {
xp->u.strv = (const char **) e->loc->argv + 1;
xp->str = *xp->u.strv++;
xp->split = c == '@';
state = XARG;
}
} else {
if ((p=strchr(sp,'[')) && (p[1]=='*'||p[1]=='@') && p[2]==']') {
XPtrV wv;
switch (stype & 0x7f) {
case '=':
case '%':
case '#':
return -1;
}
XPinit(wv, 32);
vp = global(arrayname(sp));
for (; vp; vp = vp->u.array) {
if (!(vp->flag&ISSET))
continue;
XPput(wv, str_val(vp));
}
if (XPsize(wv) == 0) {
xp->str = null;
state = p[1] == '@' ? XNULLSUB : XSUB;
XPfree(wv);
} else {
XPput(wv, 0);
xp->u.strv = (const char **) XPptrv(wv);
xp->str = *xp->u.strv++;
xp->split = p[1] == '@';
state = XARG;
}
} else {
if ((stype & 0x7f) == '='
&& (ctype(*sp, C_VAR1) || digit(*sp)))
return -1;
xp->var = global(sp);
xp->str = str_val(xp->var);
state = XSUB;
}
}
c = stype&0x7f;
if (ctype(c, C_SUBOP2) ||
(((stype&0x80) ? *xp->str=='\0' : xp->str==null) ?
c == '=' || c == '-' || c == '?' : c == '+'))
state = XBASE;
if (Flag(FNOUNSET) && xp->str == null
&& (ctype(c, C_SUBOP2) || (state != XBASE && c != '+')))
errorf("%s: parameter not set", sp);
return state;
}
static int
comsub(xp, cp)
register Expand *xp;
char *cp;
{
Source *s, *sold;
register struct op *t;
struct shf *shf;
s = pushs(SSTRING, ATEMP);
s->start = s->str = cp;
sold = source;
t = compile(s);
source = sold;
if (t == NULL)
return XBASE;
if (t != NULL && t->type == TCOM &&
*t->args == NULL && *t->vars == NULL && t->ioact != NULL) {
register struct ioword *io = *t->ioact;
char *name;
if ((io->flag&IOTYPE) != IOREAD)
errorf("funny $() command: %s",
snptreef((char *) 0, 32, "%R", io));
shf = shf_open(name = evalstr(io->name, DOTILDE), O_RDONLY, 0,
SHF_MAPHI|SHF_CLEXEC);
if (shf == NULL)
errorf("%s: cannot open $() input", name);
xp->split = 0;
} else {
int ofd1, pv[2];
openpipe(pv);
shf = shf_fdopen(pv[0], SHF_RD, (struct shf *) 0);
ofd1 = savefd(1, 0);
ksh_dup2(pv[1], 1, FALSE);
close(pv[1]);
execute(t, XFORK|XXCOM|XPIPEO);
restfd(1, ofd1);
startlast();
xp->split = 1;
}
xp->u.shf = shf;
return XCOM;
}
static char *
trimsub(str, pat, how)
register char *str;
char *pat;
int how;
{
register char *end = strchr(str, 0);
register char *p, c;
switch (how&0xff) {
case '#':
for (p = str; p <= end; p++) {
c = *p; *p = '\0';
if (gmatch(str, pat, FALSE)) {
*p = c;
return p;
}
*p = c;
}
break;
case '#'|0x80:
for (p = end; p >= str; p--) {
c = *p; *p = '\0';
if (gmatch(str, pat, FALSE)) {
*p = c;
return p;
}
*p = c;
}
break;
case '%':
for (p = end; p >= str; p--) {
if (gmatch(p, pat, FALSE))
return str_nsave(str, p - str, ATEMP);
}
break;
case '%'|0x80:
for (p = str; p <= end; p++) {
if (gmatch(p, pat, FALSE))
return str_nsave(str, p - str, ATEMP);
}
break;
}
return str;
}
static void
glob(cp, wp, markdirs)
char *cp;
register XPtrV *wp;
int markdirs;
{
int oldsize = XPsize(*wp);
if (glob_str(cp, wp, markdirs) == 0)
XPput(*wp, debunk(cp, cp));
else
qsortp(XPptrv(*wp) + oldsize, (size_t)(XPsize(*wp) - oldsize),
xstrcmp);
}
#define GF_NONE		0
#define GF_EXCHECK	BIT(0)
#define GF_GLOBBED	BIT(1)
#define GF_MARKDIR	BIT(2)
int
glob_str(cp, wp, markdirs)
char *cp;
XPtrV *wp;
int markdirs;
{
int oldsize = XPsize(*wp);
XString xs;
char *xp;
Xinit(xs, xp, 256, ATEMP);
globit(&xs, &xp, cp, wp, markdirs ? GF_MARKDIR : GF_NONE);
Xfree(xs, xp);
return XPsize(*wp) - oldsize;
}
static void
globit(xs, xpp, sp, wp, check)
XString *xs;
char **xpp;
char *sp;
register XPtrV *wp;
int check;
{
register char *np;
char *xp = *xpp;
char *se;
char odirsep;
intrcheck();
if (sp == NULL) {
if ((check & GF_EXCHECK)
|| ((check & GF_MARKDIR) && (check & GF_GLOBBED)))
{
#define stat_check()	(stat_done ? stat_done : \
(stat_done = stat(Xstring(*xs, xp), &statb) < 0 \
? -1 : 1))
struct stat lstatb, statb;
int stat_done = 0;
if (lstat(Xstring(*xs, xp), &lstatb) < 0)
return;
if ((check & GF_EXCHECK) && xp > Xstring(*xs, xp)
&& ISDIRSEP(xp[-1]) && !S_ISDIR(lstatb.st_mode)
#ifdef S_ISLNK
&& (!S_ISLNK(lstatb.st_mode)
|| stat_check() < 0
|| !S_ISDIR(statb.st_mode))
#endif
)
return;
if (((check & GF_MARKDIR) && (check & GF_GLOBBED))
&& xp > Xstring(*xs, xp) && !ISDIRSEP(xp[-1])
&& (S_ISDIR(lstatb.st_mode)
#ifdef S_ISLNK
|| (S_ISLNK(lstatb.st_mode)
&& stat_check() > 0
&& S_ISDIR(statb.st_mode))
#endif
))
{
*xp++ = DIRSEP;
*xp = '\0';
}
}
#ifdef OS2
# define KLUDGE_VAL	4
#else
# define KLUDGE_VAL	0
#endif
XPput(*wp, str_nsave(Xstring(*xs, xp), Xlength(*xs, xp)
+ KLUDGE_VAL, ATEMP));
return;
}
if (xp > Xstring(*xs, xp))
*xp++ = DIRSEP;
while (ISDIRSEP(*sp)) {
Xcheck(*xs, xp);
*xp++ = *sp++;
}
np = ksh_strchr_dirsep(sp);
if (np != NULL) {
se = np;
odirsep = *np;
*np++ = '\0';
} else {
odirsep = '\0';
se = sp + strlen(sp);
}
if (!has_globbing(sp, se)) {
XcheckN(*xs, xp, se - sp + 1);
debunk(xp, sp);
xp += strlen(xp);
*xpp = xp;
globit(xs, xpp, np, wp, check);
} else {
DIR *dirp;
struct dirent *d;
char *name;
int len;
int prefix_len;
*xp = '\0';
prefix_len = Xlength(*xs, xp);
dirp = ksh_opendir(prefix_len ? Xstring(*xs, xp) : ".");
if (dirp == NULL)
goto Nodir;
while ((d = readdir(dirp)) != NULL) {
name = d->d_name;
if (name[0] == '.' &&
(name[1] == 0 || (name[1] == '.' && name[2] == 0)))
continue;
if ((*name == '.' && *sp != '.')
|| !gmatch(name, sp, TRUE))
continue;
len = NLENGTH(d) + 1;
XcheckN(*xs, xp, len);
memcpy(xp, name, len);
*xpp = xp + len - 1;
globit(xs, xpp, np, wp,
(check & GF_MARKDIR) | GF_GLOBBED
| (np ? GF_EXCHECK : GF_NONE));
xp = Xstring(*xs, xp) + prefix_len;
}
closedir(dirp);
Nodir:;
}
if (np != NULL)
*--np = odirsep;
}
#if 0
static int	copy_non_glob ARGS((XString *xs, char **xpp, char *p));
static int
copy_non_glob(xs, xpp, p)
XString *xs;
char **xpp;
char *p;
{
char *xp;
int len = strlen(p);
XcheckN(*xs, *xpp, len);
xp = *xpp;
for (; *p; p++) {
if (ISMAGIC(*p)) {
int c = *++p;
if (c == '*' || c == '?')
return 0;
if (*p == '[') {
char *q = p + 1;
if (ISMAGIC(*q) && q[1] == NOT)
q += 2;
if (ISMAGIC(*q) && q[1] == ']')
q += 2;
for (; *q; q++)
if (ISMAGIC(*q) && *++q == ']')
return 0;
}
}
*xp++ = *p;
}
*xp = '\0';
*xpp = xp;
return 1;
}
#endif
char *
debunk(dp, sp)
char *dp;
const char *sp;
{
char *d, *s;
if ((s = strchr(sp, MAGIC))) {
memcpy(dp, sp, s - sp);
for (d = dp + (s - sp); *s; s++)
if (!ISMAGIC(*s) || !(*++s & 0x80)
|| !strchr("*+?@! ", *s & 0x7f))
*d++ = *s;
else {
if ((*s & 0x7f) != ' ')
*d++ = *s & 0x7f;
*d++ = '(';
}
*d = '\0';
} else if (dp != sp)
strcpy(dp, sp);
return dp;
}
static char *
maybe_expand_tilde(p, dsp, dpp, isassign)
char *p;
XString *dsp;
char **dpp;
int isassign;
{
XString ts;
char *dp = *dpp;
char *tp, *r;
Xinit(ts, tp, 16, ATEMP);
while (p[0] == CHAR && !ISDIRSEP(p[1])
&& (!isassign || p[1] != PATHSEP))
{
Xcheck(ts, tp);
*tp++ = p[1];
p += 2;
}
*tp = '\0';
r = (p[0] == EOS || p[0] == CHAR || p[0] == CSUBST) ? tilde(Xstring(ts, tp)) : (char *) 0;
Xfree(ts, tp);
if (r) {
while (*r) {
Xcheck(*dsp, dp);
if (ISMAGIC(*r))
*dp++ = MAGIC;
*dp++ = *r++;
}
*dpp = dp;
r = p;
}
return r;
}
static char *
tilde(cp)
char *cp;
{
char *dp;
if (cp[0] == '\0')
dp = str_val(global("HOME"));
else if (cp[0] == '+' && cp[1] == '\0')
dp = str_val(global("PWD"));
else if (cp[0] == '-' && cp[1] == '\0')
dp = str_val(global("OLDPWD"));
else
dp = homedir(cp);
if (dp == null)
dp = (char *) 0;
return dp;
}
static char *
homedir(name)
char *name;
{
register struct tbl *ap;
ap = tenter(&homedirs, name, hash(name));
if (!(ap->flag & ISSET)) {
#ifdef OS2
return NULL;
#else
struct passwd *pw;
pw = getpwnam(name);
if (pw == NULL)
return NULL;
ap->val.s = str_save(pw->pw_dir, APERM);
ap->flag |= DEFINED|ISSET|ALLOC;
#endif
}
return ap->val.s;
}
#ifdef BRACE_EXPAND
static void
alt_expand(wp, start, exp_start, end, fdo)
XPtrV *wp;
char *start, *exp_start;
char *end;
int fdo;
{
int UNINITIALIZED(count);
char *brace_start, *brace_end, *UNINITIALIZED(comma);
char *field_start;
char *p;
for (p = exp_start; (p = strchr(p, MAGIC)) && p[1] != OBRACE; p += 2)
;
brace_start = p;
if (p) {
comma = (char *) 0;
count = 1;
for (p += 2; *p && count; p++) {
if (ISMAGIC(*p)) {
if (*++p == OBRACE)
count++;
else if (*p == CBRACE)
--count;
else if (*p == ',' && count == 1)
comma = p;
}
}
}
if (!p || count != 0) {
if (fdo & DOGLOB)
glob(start, wp, fdo & DOMARKDIRS);
else
XPput(*wp, debunk(start, start));
return;
}
brace_end = p;
if (!comma) {
alt_expand(wp, start, brace_end, end, fdo);
return;
}
field_start = brace_start + 2;
count = 1;
for (p = brace_start + 2; p != brace_end; p++) {
if (ISMAGIC(*p)) {
if (*++p == OBRACE)
count++;
else if ((*p == CBRACE && --count == 0)
|| (*p == ',' && count == 1))
{
char *new;
int l1, l2, l3;
l1 = brace_start - start;
l2 = (p - 1) - field_start;
l3 = end - brace_end;
new = (char *) alloc(l1 + l2 + l3 + 1, ATEMP);
memcpy(new, start, l1);
memcpy(new + l1, field_start, l2);
memcpy(new + l1 + l2, brace_end, l3);
new[l1 + l2 + l3] = '\0';
alt_expand(wp, new, new + l1,
new + l1 + l2 + l3, fdo);
field_start = p + 1;
}
}
}
return;
}
#endif