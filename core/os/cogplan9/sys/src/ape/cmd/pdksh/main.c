#define	EXTERN
#include "sh.h"
#include "ksh_stat.h"
#include "ksh_time.h"
extern char **environ;
static void	reclaim ARGS((void));
static void	remove_temps ARGS((struct temp *tp));
static int	is_restricted ARGS((char *name));
static const char initifs[] = "IFS= \t\n";
static const char initsubs[] = "${PS2=> } ${PS3=#? } ${PS4=+ }";
static const char version_param[] =
#ifdef KSH
"KSH_VERSION"
#else
"SH_VERSION"
#endif
;
static const char *const initcoms [] = {
"typeset", "-x", "SHELL", "PATH", "HOME", NULL,
"typeset", "-r", version_param, NULL,
"typeset", "-i", "PPID", NULL,
"typeset", "-i", "OPTIND=1", NULL,
#ifdef KSH
"eval", "typeset -i RANDOM MAILCHECK=\"${MAILCHECK-600}\" SECONDS=\"${SECONDS-0}\" TMOUT=\"${TMOUT-0}\"", NULL,
#endif
"alias",
"hash=alias -t",
"type=whence -v",
#ifdef JOBS
"stop=kill -STOP",
"suspend=kill -STOP $$",
#endif
#ifdef KSH
"autoload=typeset -fu",
"functions=typeset -f",
# ifdef HISTORY
"history=fc -l",
# endif
"integer=typeset -i",
"nohup=nohup ",
"local=typeset",
"r=fc -e -",
#endif
#ifdef KSH
"login=exec login",
"newgrp=exec newgrp",
#endif
NULL,
"alias", "-tU",
"cat", "cc", "chmod", "cp", "date", "ed", "emacs", "grep", "ls",
"mail", "make", "mv", "pr", "rm", "sed", "sh", "vi", "who",
NULL,
#ifdef EXTRA_INITCOMS
EXTRA_INITCOMS, NULL,
#endif
NULL
};
int
main(argc, argv)
int argc;
register char **argv;
{
register int i;
int argi;
Source *s;
struct block *l;
int restricted, errexit;
char **wp;
struct env env;
pid_t ppid;
#ifdef MEM_DEBUG
chmem_set_defaults("ct", 1);
#endif
#ifdef OS2
setmode (0, O_BINARY);
setmode (1, O_TEXT);
#endif
if (!*argv) {
static const char	*empty_argv[] = {
"pdksh", (char *) 0
};
argv = (char **) empty_argv;
argc = 1;
}
kshname = *argv;
ainit(&aperm);
memset(&env, 0, sizeof(env));
env.type = E_NONE;
ainit(&env.area);
e = &env;
newblock();
initio();
initvar();
initctypes();
inittraps();
#ifdef KSH
coproc_init();
#endif
tinit(&taliases, APERM, 0);
tinit(&aliases, APERM, 0);
tinit(&homedirs, APERM, 0);
initkeywords();
tinit(&builtins, APERM, 64);
for (i = 0; shbuiltins[i].name != NULL; i++)
builtin(shbuiltins[i].name, shbuiltins[i].func);
for (i = 0; kshbuiltins[i].name != NULL; i++)
builtin(kshbuiltins[i].name, kshbuiltins[i].func);
init_histvec();
def_path = DEFAULT__PATH;
#if defined(HAVE_CONFSTR) && defined(_CS_PATH)
{
size_t len = confstr(_CS_PATH, (char *) 0, 0);
char *new;
if (len > 0) {
confstr(_CS_PATH, new = alloc(len + 1, APERM), len + 1);
def_path = new;
}
}
#endif
{
struct tbl *vp = global("PATH");
setstr(vp, def_path, KSH_RETURN_ERROR);
}
Flag(FNOHUP) = 1;
#ifdef BRACE_EXPAND
Flag(FBRACEEXPAND) = 1;
#endif
#ifdef POSIXLY_CORRECT
change_flag(FPOSIX, OF_SPECIAL, 1);
#endif
if (environ != NULL)
for (wp = environ; *wp != NULL; wp++)
typeset(*wp, IMPORT|EXPORT, 0, 0, 0);
kshpid = procpid = getpid();
typeset(initifs, 0, 0, 0, 0);
substitute(initsubs, 0);
{
struct stat s_pwd, s_dot;
struct tbl *pwd_v = global("PWD");
char *pwd = str_val(pwd_v);
char *pwdx = pwd;
if (!ISABSPATH(pwd)
|| stat(pwd, &s_pwd) < 0 || stat(".", &s_dot) < 0
|| s_pwd.st_dev != s_dot.st_dev
|| s_pwd.st_ino != s_dot.st_ino)
pwdx = (char *) 0;
set_current_wd(pwdx);
if (current_wd[0])
simplify_path(current_wd);
if (current_wd[0] || pwd != null)
setstr(pwd_v, current_wd, KSH_RETURN_ERROR);
}
ppid = getppid();
setint(global("PPID"), (long) ppid);
#ifdef KSH
setint(global("RANDOM"), (long) (time((time_t *)0) * kshpid * ppid));
#endif
setstr(global(version_param), ksh_version, KSH_RETURN_ERROR);
for (wp = (char**) initcoms; *wp != NULL; wp++) {
shcomexec(wp);
for (; *wp != NULL; wp++)
;
}
ksheuid = geteuid();
safe_prompt = ksheuid ? "$ " : "# ";
{
struct tbl *vp = global("PS1");
if (!(vp->flag & ISSET)
|| (!ksheuid && !strchr(str_val(vp), '#')))
setstr(vp, safe_prompt, KSH_RETURN_ERROR);
}
Flag(FPRIVILEGED) = getuid() != ksheuid || getgid() != getegid();
Flag(FMONITOR) = 127;
argi = parse_args(argv, OF_CMDLINE, (int *) 0);
if (argi < 0)
exit(1);
if (Flag(FCOMMAND)) {
s = pushs(SSTRING, ATEMP);
if (!(s->start = s->str = argv[argi++]))
errorf("-c requires an argument");
if (argv[argi])
kshname = argv[argi++];
} else if (argi < argc && !Flag(FSTDIN)) {
s = pushs(SFILE, ATEMP);
#ifdef OS2
s->file = search(argv[argi++], path, R_OK, (int *) 0);
if (!s->file || !*s->file)
s->file = argv[argi - 1];
#else
s->file = argv[argi++];
#endif
s->u.shf = shf_open(s->file, O_RDONLY, 0, SHF_MAPHI|SHF_CLEXEC);
if (s->u.shf == NULL) {
exstat = 127;
errorf("%s: %s", s->file, strerror(errno));
}
kshname = s->file;
} else {
Flag(FSTDIN) = 1;
s = pushs(SSTDIN, ATEMP);
s->file = "<stdin>";
s->u.shf = shf_fdopen(0, SHF_RD | can_seek(0),
(struct shf *) 0);
if (!Flag(FNOTTALKING) && isatty(0) && isatty(2)) {
Flag(FTALKING) = Flag(FTALKING_I) = 1;
s->flags |= SF_TTY;
s->u.shf->flags |= SHF_INTERRUPT;
s->file = (char *) 0;
}
}
{
struct stat s_stdin;
if (fstat(0, &s_stdin) >= 0 && S_ISCHR(s_stdin.st_mode))
reset_nonblock(0);
}
i = Flag(FMONITOR) != 127;
Flag(FMONITOR) = 0;
j_init(i);
#ifdef EDIT
if (Flag(FTALKING))
x_init();
#endif
l = e->loc;
l->argv = &argv[argi - 1];
l->argc = argc - argi;
l->argv[0] = (char *) kshname;
getopts_reset(1);
restricted = Flag(FRESTRICTED);
Flag(FRESTRICTED) = 0;
errexit = Flag(FERREXIT);
Flag(FERREXIT) = 0;
if (!current_wd[0] && Flag(FTALKING))
warningf(FALSE, "Cannot determine current working directory");
if (Flag(FLOGIN)) {
#ifdef OS2
char *profile;
if (!Flag(FPRIVILEGED)
&& strcmp(profile = substitute("$INIT/profile.ksh", 0),
"/profile.ksh"))
include(profile, 0, (char **) 0, 1);
else if (include("/etc/profile.ksh", 0, (char **) 0, 1) < 0)
include("c:/usr/etc/profile.ksh", 0, (char **) 0, 1);
if (!Flag(FPRIVILEGED))
include(substitute("$HOME/profile.ksh", 0), 0,
(char **) 0, 1);
#else
include(KSH_SYSTEM_PROFILE, 0, (char **) 0, 1);
if (!Flag(FPRIVILEGED))
include(substitute("$HOME/.profile", 0), 0,
(char **) 0, 1);
#endif
}
if (Flag(FPRIVILEGED))
include("/etc/suid_profile", 0, (char **) 0, 1);
else {
char *env_file;
#ifndef KSH
if (!Flag(FPOSIX))
env_file = null;
else
#endif
env_file = str_val(global("ENV"));
#ifdef DEFAULT_ENV
if (env_file == null)
env_file = DEFAULT_ENV;
#endif
env_file = substitute(env_file, DOTILDE);
if (*env_file != '\0')
include(env_file, 0, (char **) 0, 1);
#ifdef OS2
else if (Flag(FTALKING))
include(substitute("$HOME/kshrc.ksh", 0), 0,
(char **) 0, 1);
#endif
}
if (is_restricted(argv[0]) || is_restricted(str_val(global("SHELL"))))
restricted = 1;
if (restricted) {
static const char *const restr_com[] = {
"typeset", "-r", "PATH",
"ENV", "SHELL",
(char *) 0
};
shcomexec((char **) restr_com);
Flag(FRESTRICTED) = 1;
}
if (errexit)
Flag(FERREXIT) = 1;
if (Flag(FTALKING)) {
hist_init(s);
#ifdef KSH
alarm_init();
#endif
} else
Flag(FTRACKALL) = 1;
shell(s, TRUE);
return 0;
}
int
include(name, argc, argv, intr_ok)
const char *name;
int argc;
char **argv;
int intr_ok;
{
register Source *volatile s = NULL;
Source *volatile sold;
struct shf *shf;
char **volatile old_argv;
volatile int old_argc;
int i;
shf = shf_open(name, O_RDONLY, 0, SHF_MAPHI|SHF_CLEXEC);
if (shf == NULL)
return -1;
if (argv) {
old_argv = e->loc->argv;
old_argc = e->loc->argc;
} else {
old_argv = (char **) 0;
old_argc = 0;
}
sold = source;
newenv(E_INCL);
i = ksh_sigsetjmp(e->jbuf, 0);
if (i) {
source = sold;
if (s)
shf_close(s->u.shf);
quitenv();
if (old_argv) {
e->loc->argv = old_argv;
e->loc->argc = old_argc;
}
switch (i) {
case LRETURN:
case LERROR:
return exstat & 0xff;
case LINTR:
if (intr_ok && (exstat - 128) != SIGTERM)
return 1;
case LEXIT:
case LLEAVE:
case LSHELL:
unwind(i);
default:
internal_errorf(1, "include: %d", i);
}
}
if (argv) {
e->loc->argv = argv;
e->loc->argc = argc;
}
s = pushs(SFILE, ATEMP);
s->u.shf = shf;
s->file = str_save(name, ATEMP);
i = shell(s, FALSE);
source = sold;
shf_close(s->u.shf);
quitenv();
if (old_argv) {
e->loc->argv = old_argv;
e->loc->argc = old_argc;
}
return i & 0xff;
}
int
command(comm)
const char *comm;
{
register Source *s;
s = pushs(SSTRING, ATEMP);
s->start = s->str = comm;
return shell(s, FALSE);
}
int
shell(s, toplevel)
Source *volatile s;
int volatile toplevel;
{
struct op *t;
volatile int wastty = s->flags & SF_TTY;
volatile int attempts = 13;
volatile int interactive = Flag(FTALKING) && toplevel;
int i;
newenv(E_PARSE);
if (interactive)
really_exit = 0;
i = ksh_sigsetjmp(e->jbuf, 0);
if (i) {
s->start = s->str = null;
switch (i) {
case LINTR:
case LERROR:
case LSHELL:
if (interactive) {
if (i == LINTR)
shellf(newline);
if (Flag(FIGNOREEOF) && s->type == SEOF
&& wastty)
s->type = SSTDIN;
break;
}
case LEXIT:
case LLEAVE:
case LRETURN:
quitenv();
unwind(i);
default:
quitenv();
internal_errorf(1, "shell: %d", i);
}
}
while (1) {
if (trap)
runtraps(0);
if (s->next == NULL)
if (Flag(FVERBOSE))
s->flags |= SF_ECHO;
else
s->flags &= ~SF_ECHO;
if (interactive) {
j_notify();
#ifdef KSH
mcheck();
#endif
set_prompt(PS1, s);
}
t = compile(s);
if (t != NULL && t->type == TEOF) {
if (wastty && Flag(FIGNOREEOF) && --attempts > 0) {
shellf("Use `exit' to leave ksh\n");
s->type = SSTDIN;
} else if (wastty && !really_exit
&& j_stopped_running())
{
really_exit = 1;
s->type = SSTDIN;
} else {
if (toplevel)
unwind(LEXIT);
break;
}
}
if (t && (!Flag(FNOEXEC) || (s->flags & SF_TTY)))
exstat = execute(t, 0);
if (t != NULL && t->type != TEOF && interactive && really_exit)
really_exit = 0;
reclaim();
}
quitenv();
return exstat;
}
void
unwind(i)
int i;
{
if (i == LEXIT || (Flag(FERREXIT) && (i == LERROR || i == LINTR)
&& sigtraps[SIGEXIT_].trap))
{
runtrap(&sigtraps[SIGEXIT_]);
i = LLEAVE;
} else if (Flag(FERREXIT) && (i == LERROR || i == LINTR)) {
runtrap(&sigtraps[SIGERR_]);
i = LLEAVE;
}
while (1) {
switch (e->type) {
case E_PARSE:
case E_FUNC:
case E_INCL:
case E_LOOP:
case E_ERRH:
ksh_siglongjmp(e->jbuf, i);
case E_NONE:
if (i == LINTR)
e->flags |= EF_FAKE_SIGDIE;
default:
quitenv();
}
}
}
void
newenv(type)
int type;
{
register struct env *ep;
ep = (struct env *) alloc(sizeof(*ep), ATEMP);
ep->type = type;
ep->flags = 0;
ainit(&ep->area);
ep->loc = e->loc;
ep->savefd = NULL;
ep->oenv = e;
ep->temps = NULL;
e = ep;
}
void
quitenv()
{
register struct env *ep = e;
register int fd;
if (ep->oenv && ep->oenv->loc != ep->loc)
popblock();
if (ep->savefd != NULL) {
for (fd = 0; fd < NUFILE; fd++)
if (ep->savefd[fd])
restfd(fd, ep->savefd[fd]);
if (ep->savefd[2])
shf_reopen(2, SHF_WR, shl_out);
}
reclaim();
if (ep->oenv == NULL) {
if (ep->type == E_NONE) {
if (Flag(FTALKING))
hist_finish();
j_exit();
if (ep->flags & EF_FAKE_SIGDIE) {
int sig = exstat - 128;
if (sig == SIGINT || sig == SIGTERM) {
setsig(&sigtraps[sig], SIG_DFL,
SS_RESTORE_CURR|SS_FORCE);
kill(0, sig);
}
}
#ifdef MEM_DEBUG
chmem_allfree();
#endif
}
exit(exstat);
}
e = e->oenv;
afree(ep, ATEMP);
}
void
cleanup_parents_env()
{
struct env *ep;
int fd;
for (ep = e; ep; ep = ep->oenv) {
if (ep->savefd) {
for (fd = 0; fd < NUFILE; fd++)
if (ep->savefd[fd] > 0)
close(ep->savefd[fd]);
afree(ep->savefd, &ep->area);
ep->savefd = (short *) 0;
}
}
e->oenv = (struct env *) 0;
}
void
cleanup_proc_env()
{
struct env *ep;
for (ep = e; ep; ep = ep->oenv)
remove_temps(ep->temps);
}
static void
reclaim()
{
remove_temps(e->temps);
e->temps = NULL;
afreeall(&e->area);
}
static void
remove_temps(tp)
struct temp *tp;
{
#ifdef OS2
static struct temp *delayed_remove;
struct temp *t, **tprev;
if (delayed_remove) {
for (tprev = &delayed_remove, t = delayed_remove; t; t = *tprev)
if (unlink(t->name) >= 0 || errno == ENOENT) {
*tprev = t->next;
afree(t, APERM);
} else
tprev = &t->next;
}
#endif
for (; tp != NULL; tp = tp->next)
if (tp->pid == procpid) {
#ifdef OS2
if (unlink(tp->name) < 0 && errno != ENOENT) {
t = (struct temp *) alloc(
sizeof(struct temp) + strlen(tp->name) + 1,
APERM);
memset(t, 0, sizeof(struct temp));
t->name = (char *) &t[1];
strcpy(t->name, tp->name);
t->next = delayed_remove;
delayed_remove = t;
}
#else
unlink(tp->name);
#endif
}
}
static int
is_restricted(name)
char *name;
{
char *p;
return 0;
#ifdef dumbidea
if ((p = ksh_strrchr_dirsep(name)))
name = p;
return (p = strchr(name, 'r')) && strstr(p, "sh");
#endif
}
void
aerror(ap, msg)
Area *ap;
const char *msg;
{
internal_errorf(1, "alloc: %s", msg);
errorf(null);
}