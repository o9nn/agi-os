#include "sh.h"
#include <ctype.h>
typedef struct lex_state Lex_state;
struct lex_state {
int ls_state;
union {
struct scsparen_info {
int nparen;
int csstate;
#define ls_scsparen ls_info.u_scsparen
} u_scsparen;
struct sasparen_info {
int nparen;
int start;
#define ls_sasparen ls_info.u_sasparen
} u_sasparen;
struct sletparen_info {
int nparen;
#define ls_sletparen ls_info.u_sletparen
} u_sletparen;
struct sbquote_info {
int indquotes;
#define ls_sbquote ls_info.u_sbquote
} u_sbquote;
Lex_state *base;
} ls_info;
};
typedef struct State_info State_info;
struct State_info {
Lex_state *base;
Lex_state *end;
};
static void readhere ARGS((struct ioword *iop));
static int getsc__ ARGS((void));
static void getsc_line ARGS((Source *s));
static int getsc_bn ARGS((void));
static char *get_brace_var ARGS((XString *wsp, char *wp));
static int arraysub ARGS((char **strp));
static const char *ungetsc ARGS((int c));
static void gethere ARGS((void));
static Lex_state *push_state_ ARGS((State_info *si, Lex_state *old_end));
static Lex_state *pop_state_ ARGS((State_info *si, Lex_state *old_end));
static int backslash_skip;
static int ignore_backslash_newline;
#define getsc() (*source->str != '\0' && *source->str != '\\' \
&& !backslash_skip ? *source->str++ : getsc_bn())
#define getsc_() ((*source->str != '\0') ? *source->str++ : getsc__())
#define STATE_BSIZE 32
#define PUSH_STATE(s) do { \
if (++statep == state_info.end) \
statep = push_state_(&state_info, statep); \
state = statep->ls_state = (s); \
} while (0)
#define POP_STATE() do { \
if (--statep == state_info.base) \
statep = pop_state_(&state_info, statep); \
state = statep->ls_state; \
} while (0)
int
yylex(cf)
int cf;
{
Lex_state states[STATE_BSIZE], *statep;
State_info state_info;
register int c, state;
XString ws;
register char *wp;
char *sp, *dp;
int c2;
Again:
states[0].ls_state = -1;
states[0].ls_info.base = (Lex_state *) 0;
statep = &states[1];
state_info.base = states;
state_info.end = &states[STATE_BSIZE];
Xinit(ws, wp, 64, ATEMP);
backslash_skip = 0;
ignore_backslash_newline = 0;
if (cf&ONEWORD)
state = SWORD;
#ifdef KSH
else if (cf&LETEXPR) {
*wp++ = OQUOTE;
state = SLETPAREN;
statep->ls_sletparen.nparen = 0;
}
#endif
else {
state = (cf & HEREDELIM) ? SHEREDELIM : SBASE;
while ((c = getsc()) == ' ' || c == '\t')
;
if (c == '#') {
ignore_backslash_newline++;
while ((c = getsc()) != '\0' && c != '\n')
;
ignore_backslash_newline--;
}
ungetsc(c);
}
if (source->flags & SF_ALIAS) {
source->flags &= ~SF_ALIAS;
if (!Flag(FPOSIX) || (cf & CMDWORD))
cf |= ALIAS;
}
statep->ls_state = state;
while (!((c = getsc()) == 0
|| ((state == SBASE || state == SHEREDELIM)
&& ctype(c, C_LEX1))))
{
Xcheck(ws, wp);
switch (state) {
case SBASE:
if (c == '[' && (cf & (VARASN|ARRAYVAR))) {
*wp = EOS;
if (is_wdvarname(Xstring(ws, wp), FALSE))
{
char *p, *tmp;
if (arraysub(&tmp)) {
*wp++ = CHAR;
*wp++ = c;
for (p = tmp; *p; ) {
Xcheck(ws, wp);
*wp++ = CHAR;
*wp++ = *p++;
}
afree(tmp, ATEMP);
break;
} else {
Source *s;
s = pushs(SREREAD,
source->areap);
s->start = s->str
= s->u.freeme = tmp;
s->next = source;
source = s;
}
}
*wp++ = CHAR;
*wp++ = c;
break;
}
Sbase1:
#ifdef KSH
if (c == '*' || c == '@' || c == '+' || c == '?'
|| c == '!')
{
c2 = getsc();
if (c2 == '(' ) {
*wp++ = OPAT;
*wp++ = c;
PUSH_STATE(SPATTERN);
break;
}
ungetsc(c2);
}
#endif
Sbase2:
switch (c) {
case '\\':
c = getsc();
#ifdef OS2
if (isalnum(c)) {
*wp++ = CHAR, *wp++ = '\\';
*wp++ = CHAR, *wp++ = c;
} else
#endif
if (c)
*wp++ = QCHAR, *wp++ = c;
break;
case '\'':
*wp++ = OQUOTE;
ignore_backslash_newline++;
PUSH_STATE(SSQUOTE);
break;
case '"':
*wp++ = OQUOTE;
PUSH_STATE(SDQUOTE);
break;
default:
goto Subst;
}
break;
Subst:
switch (c) {
case '\\':
c = getsc();
switch (c) {
case '"': case '\\':
case '$': case '`':
*wp++ = QCHAR, *wp++ = c;
break;
default:
Xcheck(ws, wp);
if (c) {
*wp++ = CHAR, *wp++ = '\\';
*wp++ = CHAR, *wp++ = c;
}
break;
}
break;
case '$':
c = getsc();
if (c == '(') {
c = getsc();
if (c == '(') {
PUSH_STATE(SASPAREN);
statep->ls_sasparen.nparen = 2;
statep->ls_sasparen.start =
Xsavepos(ws, wp);
*wp++ = EXPRSUB;
} else {
ungetsc(c);
PUSH_STATE(SCSPAREN);
statep->ls_scsparen.nparen = 1;
statep->ls_scsparen.csstate = 0;
*wp++ = COMSUB;
}
} else if (c == '{') {
*wp++ = OSUBST;
*wp++ = '{';
wp = get_brace_var(&ws, wp);
c = getsc();
if (c == ':') {
*wp++ = CHAR, *wp++ = c;
c = getsc();
}
if (c == '#' || c == '%') {
ungetsc(c);
PUSH_STATE(STBRACE);
} else {
ungetsc(c);
PUSH_STATE(SBRACE);
}
} else if (ctype(c, C_ALPHA)) {
*wp++ = OSUBST;
*wp++ = 'X';
do {
Xcheck(ws, wp);
*wp++ = c;
c = getsc();
} while (ctype(c, C_ALPHA|C_DIGIT));
*wp++ = '\0';
*wp++ = CSUBST;
*wp++ = 'X';
ungetsc(c);
} else if (ctype(c, C_DIGIT|C_VAR1)) {
Xcheck(ws, wp);
*wp++ = OSUBST;
*wp++ = 'X';
*wp++ = c;
*wp++ = '\0';
*wp++ = CSUBST;
*wp++ = 'X';
} else {
*wp++ = CHAR, *wp++ = '$';
ungetsc(c);
}
break;
case '`':
PUSH_STATE(SBQUOTE);
*wp++ = COMSUB;
statep->ls_sbquote.indquotes = 0;
if (!Flag(FPOSIX)) {
Lex_state *s = statep;
Lex_state *base = state_info.base;
while (1) {
for (; s != base; s--) {
if (s->ls_state == SDQUOTE) {
statep->ls_sbquote.indquotes = 1;
break;
}
}
if (s != base)
break;
if (!(s = s->ls_info.base))
break;
base = s-- - STATE_BSIZE;
}
}
break;
default:
*wp++ = CHAR, *wp++ = c;
}
break;
case SSQUOTE:
if (c == '\'') {
POP_STATE();
*wp++ = CQUOTE;
ignore_backslash_newline--;
} else
*wp++ = QCHAR, *wp++ = c;
break;
case SDQUOTE:
if (c == '"') {
POP_STATE();
*wp++ = CQUOTE;
} else
goto Subst;
break;
case SCSPAREN:
switch (statep->ls_scsparen.csstate) {
case 0:
switch (c) {
case '(':
statep->ls_scsparen.nparen++;
break;
case ')':
statep->ls_scsparen.nparen--;
break;
case '\\':
statep->ls_scsparen.csstate = 1;
break;
case '"':
statep->ls_scsparen.csstate = 2;
break;
case '\'':
statep->ls_scsparen.csstate = 4;
ignore_backslash_newline++;
break;
}
break;
case 1:
case 3:
--statep->ls_scsparen.csstate;
break;
case 2:
if (c == '"')
statep->ls_scsparen.csstate = 0;
else if (c == '\\')
statep->ls_scsparen.csstate = 3;
break;
case 4:
if (c == '\'') {
statep->ls_scsparen.csstate = 0;
ignore_backslash_newline--;
}
break;
}
if (statep->ls_scsparen.nparen == 0) {
POP_STATE();
*wp++ = 0;
} else
*wp++ = c;
break;
case SASPAREN:
if (c == '(')
statep->ls_sasparen.nparen++;
else if (c == ')') {
statep->ls_sasparen.nparen--;
if (statep->ls_sasparen.nparen == 1) {
if ((c2 = getsc()) == ')') {
POP_STATE();
*wp++ = 0;
break;
} else {
char *s;
ungetsc(c2);
s = Xrestpos(ws, wp,
statep->ls_sasparen.start);
memmove(s + 1, s, wp - s);
*s++ = COMSUB;
*s = '(';
wp++;
statep->ls_scsparen.nparen = 1;
statep->ls_scsparen.csstate = 0;
state = statep->ls_state
= SCSPAREN;
}
}
}
*wp++ = c;
break;
case SBRACE:
if (c == '}') {
POP_STATE();
*wp++ = CSUBST;
*wp++ = '}';
} else
goto Sbase1;
break;
case STBRACE:
if (c == '}') {
POP_STATE();
*wp++ = CSUBST;
*wp++ = '}';
} else if (c == '|') {
*wp++ = SPAT;
} else if (c == '(') {
*wp++ = OPAT;
*wp++ = ' ';
PUSH_STATE(SPATTERN);
} else
goto Sbase1;
break;
case SBQUOTE:
if (c == '`') {
*wp++ = 0;
POP_STATE();
} else if (c == '\\') {
switch (c = getsc()) {
case '\\':
case '$': case '`':
*wp++ = c;
break;
case '"':
if (statep->ls_sbquote.indquotes) {
*wp++ = c;
break;
}
default:
if (c) {
*wp++ = '\\';
*wp++ = c;
}
break;
}
} else
*wp++ = c;
break;
case SWORD:
goto Subst;
#ifdef KSH
case SLETPAREN:
if (c == ')') {
if (statep->ls_sletparen.nparen > 0)
--statep->ls_sletparen.nparen;
else if ((c2 = getsc()) == ')') {
c = 0;
*wp++ = CQUOTE;
goto Done;
} else
ungetsc(c2);
} else if (c == '(')
++statep->ls_sletparen.nparen;
goto Sbase2;
#endif
case SHEREDELIM:
if (c == '\\') {
c = getsc();
if (c) {
*wp++ = QCHAR;
*wp++ = c;
}
} else if (c == '\'') {
PUSH_STATE(SSQUOTE);
*wp++ = OQUOTE;
ignore_backslash_newline++;
} else if (c == '"') {
state = statep->ls_state = SHEREDQUOTE;
*wp++ = OQUOTE;
} else {
*wp++ = CHAR;
*wp++ = c;
}
break;
case SHEREDQUOTE:
if (c == '"') {
*wp++ = CQUOTE;
state = statep->ls_state = SHEREDELIM;
} else {
if (c == '\\') {
switch (c = getsc()) {
case '\\': case '"':
case '$': case '`':
break;
default:
if (c) {
*wp++ = CHAR;
*wp++ = '\\';
}
break;
}
}
*wp++ = CHAR;
*wp++ = c;
}
break;
case SPATTERN:
if ( c == ')') {
*wp++ = CPAT;
POP_STATE();
} else if (c == '|') {
*wp++ = SPAT;
} else if (c == '(') {
*wp++ = OPAT;
*wp++ = ' ';
PUSH_STATE(SPATTERN);
} else
goto Sbase1;
break;
}
}
Done:
Xcheck(ws, wp);
if (statep != &states[1])
yyerror("no closing quote\n");
if (state == SHEREDELIM)
state = SBASE;
dp = Xstring(ws, wp);
if ((c == '<' || c == '>') && state == SBASE
&& ((c2 = Xlength(ws, wp)) == 0
|| (c2 == 2 && dp[0] == CHAR && digit(dp[1]))))
{
struct ioword *iop =
(struct ioword *) alloc(sizeof(*iop), ATEMP);
if (c2 == 2)
iop->unit = dp[1] - '0';
else
iop->unit = c == '>';
c2 = getsc();
if (c == c2 || (c == '<' && c2 == '>')) {
iop->flag = c == c2 ?
(c == '>' ? IOCAT : IOHERE) : IORDWR;
if (iop->flag == IOHERE)
if ((c2 = getsc()) == '-')
iop->flag |= IOSKIP;
else
ungetsc(c2);
} else if (c2 == '&')
iop->flag = IODUP | (c == '<' ? IORDUP : 0);
else {
iop->flag = c == '>' ? IOWRITE : IOREAD;
if (c == '>' && c2 == '|')
iop->flag |= IOCLOB;
else
ungetsc(c2);
}
iop->name = (char *) 0;
iop->delim = (char *) 0;
iop->heredoc = (char *) 0;
Xfree(ws, wp);
yylval.iop = iop;
return REDIR;
}
if (wp == dp && state == SBASE) {
Xfree(ws, wp);
switch (c) {
default:
return c;
case '|':
case '&':
case ';':
if ((c2 = getsc()) == c)
c = (c == ';') ? BREAK :
(c == '|') ? LOGOR :
(c == '&') ? LOGAND :
YYERRCODE;
#ifdef KSH
else if (c == '|' && c2 == '&')
c = COPROC;
#endif
else
ungetsc(c2);
return c;
case '\n':
gethere();
if (cf & CONTIN)
goto Again;
return c;
case '(':
#ifdef KSH
if ((c2 = getsc()) == '(')
c = MDPAREN;
else
ungetsc(c2);
#endif
return c;
case ')':
return c;
}
}
*wp++ = EOS;
yylval.cp = Xclose(ws, wp);
if (state == SWORD
#ifdef KSH
|| state == SLETPAREN
#endif
)
return LWORD;
ungetsc(c);
for (sp = yylval.cp, dp = ident; dp < ident+IDENT && (c = *sp++) == CHAR; )
*dp++ = *sp++;
memset(dp, 0, (ident+IDENT) - dp + 1);
if (c != EOS)
*ident = '\0';
if (*ident != '\0' && (cf&(KEYWORD|ALIAS))) {
struct tbl *p;
int h = hash(ident);
if ((cf & KEYWORD) && (p = tsearch(&keywords, ident, h))
&& (!(cf & ESACONLY) || p->val.i == ESAC || p->val.i == '}'))
{
afree(yylval.cp, ATEMP);
return p->val.i;
}
if ((cf & ALIAS) && (p = tsearch(&aliases, ident, h))
&& (p->flag & ISSET))
{
register Source *s;
for (s = source; s->type == SALIAS; s = s->next)
if (s->u.tblp == p)
return LWORD;
s = pushs(SALIAS, source->areap);
s->start = s->str = p->val.s;
s->u.tblp = p;
s->next = source;
source = s;
afree(yylval.cp, ATEMP);
goto Again;
}
}
return LWORD;
}
static void
gethere()
{
register struct ioword **p;
for (p = heres; p < herep; p++)
readhere(*p);
herep = heres;
}
static void
readhere(iop)
struct ioword *iop;
{
register int c;
char *volatile eof;
char *eofp;
int skiptabs;
XString xs;
char *xp;
int xpos;
eof = evalstr(iop->delim, 0);
if (!(iop->flag & IOEVAL))
ignore_backslash_newline++;
Xinit(xs, xp, 256, ATEMP);
for (;;) {
eofp = eof;
skiptabs = iop->flag & IOSKIP;
xpos = Xsavepos(xs, xp);
while ((c = getsc()) != 0) {
if (skiptabs) {
if (c == '\t')
continue;
skiptabs = 0;
}
if (c != *eofp)
break;
Xcheck(xs, xp);
Xput(xs, xp, c);
eofp++;
}
if (*eofp == '\0' && (c == 0 || c == '\n')) {
xp = Xrestpos(xs, xp, xpos);
break;
}
ungetsc(c);
while ((c = getsc()) != '\n') {
if (c == 0)
yyerror("here document `%s' unclosed\n", eof);
Xcheck(xs, xp);
Xput(xs, xp, c);
}
Xcheck(xs, xp);
Xput(xs, xp, c);
}
Xput(xs, xp, '\0');
iop->heredoc = Xclose(xs, xp);
if (!(iop->flag & IOEVAL))
ignore_backslash_newline--;
}
void
#ifdef HAVE_PROTOTYPES
yyerror(const char *fmt, ...)
#else
yyerror(fmt, va_alist)
const char *fmt;
va_dcl
#endif
{
va_list va;
while (source->type == SALIAS || source->type == SREREAD)
source = source->next;
source->str = null;
error_prefix(TRUE);
SH_VA_START(va, fmt);
shf_vfprintf(shl_out, fmt, va);
va_end(va);
errorf(null);
}
Source *
pushs(type, areap)
int type;
Area *areap;
{
register Source *s;
s = (Source *) alloc(sizeof(Source), areap);
s->type = type;
s->str = null;
s->start = NULL;
s->line = 0;
s->errline = 0;
s->file = NULL;
s->flags = 0;
s->next = NULL;
s->areap = areap;
if (type == SFILE || type == SSTDIN) {
char *dummy;
Xinit(s->xs, dummy, 256, s->areap);
} else
memset(&s->xs, 0, sizeof(s->xs));
return s;
}
static int
getsc__()
{
register Source *s = source;
register int c;
while ((c = *s->str++) == 0) {
s->str = NULL;
switch (s->type) {
case SEOF:
s->str = null;
return 0;
case SSTDIN:
case SFILE:
getsc_line(s);
break;
case SWSTR:
break;
case SSTRING:
break;
case SWORDS:
s->start = s->str = *s->u.strv++;
s->type = SWORDSEP;
break;
case SWORDSEP:
if (*s->u.strv == NULL) {
s->start = s->str = newline;
s->type = SEOF;
} else {
s->start = s->str = space;
s->type = SWORDS;
}
break;
case SALIAS:
if (s->flags & SF_ALIASEND) {
source = s->next;
source->flags |= s->flags & SF_ALIAS;
s = source;
} else if (*s->u.tblp->val.s
&& isspace(strchr(s->u.tblp->val.s, 0)[-1]))
{
source = s = s->next;
s->flags |= SF_ALIAS;
} else {
source = s->next;
source->flags |= s->flags & SF_ALIAS;
c = getsc__();
if (c) {
s->flags |= SF_ALIASEND;
s->ugbuf[0] = c; s->ugbuf[1] = '\0';
s->start = s->str = s->ugbuf;
s->next = source;
source = s;
} else {
s = source;
s->str = NULL;
break;
}
}
continue;
case SREREAD:
if (s->start != s->ugbuf)
afree(s->u.freeme, ATEMP);
source = s = s->next;
continue;
}
if (s->str == NULL) {
s->type = SEOF;
s->start = s->str = null;
return '\0';
}
if (s->flags & SF_ECHO) {
shf_puts(s->str, shl_out);
shf_flush(shl_out);
}
}
return c;
}
static void
getsc_line(s)
Source *s;
{
char *xp = Xstring(s->xs, xp);
int interactive = Flag(FTALKING) && s->type == SSTDIN;
int have_tty = interactive && (s->flags & SF_TTY);
XcheckN(s->xs, xp, LINE);
*xp = '\0';
s->start = s->str = xp;
#ifdef KSH
if (have_tty && ksh_tmout) {
ksh_tmout_state = TMOUT_READING;
alarm(ksh_tmout);
}
#endif
#ifdef EDIT
if (have_tty && (0
# ifdef VI
|| Flag(FVI)
# endif
# ifdef EMACS
|| Flag(FEMACS) || Flag(FGMACS)
# endif
))
{
int nread;
nread = x_read(xp, LINE);
if (nread < 0)
nread = 0;
xp[nread] = '\0';
xp += nread;
}
else
#endif
{
if (interactive) {
pprompt(prompt, 0);
} else
s->line++;
while (1) {
char *p = shf_getse(xp, Xnleft(s->xs, xp), s->u.shf);
if (!p && shf_error(s->u.shf)
&& shf_errno(s->u.shf) == EINTR)
{
shf_clearerr(s->u.shf);
if (trap)
runtraps(0);
continue;
}
if (!p || (xp = p, xp[-1] == '\n'))
break;
xp++;
XcheckN(s->xs, xp, Xlength(s->xs, xp));
xp--;
}
if (s->type == SSTDIN)
shf_flush(s->u.shf);
}
source = s;
#ifdef KSH
if (have_tty && ksh_tmout)
{
ksh_tmout_state = TMOUT_EXECUTING;
alarm(0);
}
#endif
s->start = s->str = Xstring(s->xs, xp);
strip_nuls(Xstring(s->xs, xp), Xlength(s->xs, xp));
if (Xlength(s->xs, xp) == 0) {
if (s->type == SFILE)
shf_fdclose(s->u.shf);
s->str = NULL;
} else if (interactive) {
#ifdef HISTORY
char *p = Xstring(s->xs, xp);
if (cur_prompt == PS1)
while (*p && ctype(*p, C_IFS) && ctype(*p, C_IFSWS))
p++;
if (*p) {
# ifdef EASY_HISTORY
if (cur_prompt == PS2)
histappend(Xstring(s->xs, xp), 1);
else
# endif
{
s->line++;
histsave(s->line, s->str, 1);
}
}
#endif
}
if (interactive)
set_prompt(PS2, (Source *) 0);
}
void
set_prompt(to, s)
int to;
Source *s;
{
cur_prompt = to;
switch (to) {
case PS1:
#ifdef KSH
{
struct shf *shf;
char *ps1;
Area *saved_atemp;
ps1 = str_val(global("PS1"));
shf = shf_sopen((char *) 0, strlen(ps1) * 2,
SHF_WR | SHF_DYNAMIC, (struct shf *) 0);
while (*ps1) {
if (*ps1 != '!' || *++ps1 == '!')
shf_putchar(*ps1++, shf);
else
shf_fprintf(shf, "%d",
s ? s->line + 1 : 0);
}
ps1 = shf_sclose(shf);
saved_atemp = ATEMP;
newenv(E_ERRH);
if (ksh_sigsetjmp(e->jbuf, 0)) {
prompt = safe_prompt;
} else
prompt = str_save(substitute(ps1, 0),
saved_atemp);
quitenv();
}
#else
prompt = str_val(global("PS1"));
#endif
break;
case PS2:
prompt = str_val(global("PS2"));
break;
}
}
void
pprompt(cp, ntruncate)
const char *cp;
int ntruncate;
{
#if 0
char nbuf[32];
int c;
while (*cp != 0) {
if (*cp != '!')
c = *cp++;
else if (*++cp == '!')
c = *cp++;
else {
int len;
char *p;
shf_snprintf(p = nbuf, sizeof(nbuf), "%d",
source->line + 1);
len = strlen(nbuf);
if (ntruncate) {
if (ntruncate >= len) {
ntruncate -= len;
continue;
}
p += ntruncate;
len -= ntruncate;
ntruncate = 0;
}
shf_write(p, len, shl_out);
continue;
}
if (ntruncate)
--ntruncate;
else
shf_putc(c, shl_out);
}
#endif
shf_puts(cp + ntruncate, shl_out);
shf_flush(shl_out);
}
static char *
get_brace_var(wsp, wp)
XString *wsp;
char *wp;
{
enum parse_state {
PS_INITIAL, PS_SAW_HASH, PS_IDENT,
PS_NUMBER, PS_VAR1, PS_END
}
state;
char c;
state = PS_INITIAL;
while (1) {
c = getsc();
switch (state) {
case PS_INITIAL:
if (c == '#') {
state = PS_SAW_HASH;
break;
}
case PS_SAW_HASH:
if (letter(c))
state = PS_IDENT;
else if (digit(c))
state = PS_NUMBER;
else if (ctype(c, C_VAR1))
state = PS_VAR1;
else
state = PS_END;
break;
case PS_IDENT:
if (!letnum(c)) {
state = PS_END;
if (c == '[') {
char *tmp, *p;
if (!arraysub(&tmp))
yyerror("missing ]\n");
*wp++ = c;
for (p = tmp; *p; ) {
Xcheck(*wsp, wp);
*wp++ = *p++;
}
afree(tmp, ATEMP);
c = getsc();
}
}
break;
case PS_NUMBER:
if (!digit(c))
state = PS_END;
break;
case PS_VAR1:
state = PS_END;
break;
case PS_END:
break;
}
if (state == PS_END) {
*wp++ = '\0';
ungetsc(c);
break;
}
Xcheck(*wsp, wp);
*wp++ = c;
}
return wp;
}
static int
arraysub(strp)
char **strp;
{
XString ws;
char *wp;
char c;
int depth = 1;
Xinit(ws, wp, 32, ATEMP);
do {
c = getsc();
Xcheck(ws, wp);
*wp++ = c;
if (c == '[')
depth++;
else if (c == ']')
depth--;
} while (depth > 0 && c && c != '\n');
*wp++ = '\0';
*strp = Xclose(ws, wp);
return depth == 0 ? 1 : 0;
}
static const char *
ungetsc(c)
int c;
{
if (backslash_skip)
backslash_skip--;
if (source->str == null && c == '\0')
return source->str;
if (source->str > source->start)
source->str--;
else {
Source *s;
s = pushs(SREREAD, source->areap);
s->ugbuf[0] = c; s->ugbuf[1] = '\0';
s->start = s->str = s->ugbuf;
s->next = source;
source = s;
}
return source->str;
}
static int
getsc_bn ARGS((void))
{
int c, c2;
if (ignore_backslash_newline)
return getsc_();
if (backslash_skip == 1) {
backslash_skip = 2;
return getsc_();
}
backslash_skip = 0;
while (1) {
c = getsc_();
if (c == '\\') {
if ((c2 = getsc_()) == '\n')
continue;
ungetsc(c2);
backslash_skip = 1;
}
return c;
}
}
static Lex_state *
push_state_(si, old_end)
State_info *si;
Lex_state *old_end;
{
Lex_state *new = alloc(sizeof(Lex_state) * STATE_BSIZE, ATEMP);
new[0].ls_info.base = old_end;
si->base = &new[0];
si->end = &new[STATE_BSIZE];
return &new[1];
}
static Lex_state *
pop_state_(si, old_end)
State_info *si;
Lex_state *old_end;
{
Lex_state *old_base = si->base;
si->base = old_end->ls_info.base - STATE_BSIZE;
si->end = old_end->ls_info.base;
afree(old_base, ATEMP);
return si->base + STATE_BSIZE - 1;;
}