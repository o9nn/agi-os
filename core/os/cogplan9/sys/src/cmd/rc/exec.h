extern void Xappend(void), Xasync(void), Xbackq(void), Xbang(void), Xclose(void);
extern void Xconc(void), Xcount(void), Xdelfn(void), Xdol(void), Xqdol(void), Xdup(void);
extern void Xexit(void), Xfalse(void), Xfn(void), Xfor(void), Xglob(void);
extern void Xjump(void), Xmark(void), Xmatch(void), Xpipe(void), Xread(void);
extern void Xrdwr(void);
extern void Xrdfn(void), Xunredir(void), Xstar(void), Xreturn(void), Xsubshell(void);
extern void Xtrue(void), Xword(void), Xwrite(void), Xpipefd(void), Xcase(void);
extern void Xlocal(void), Xunlocal(void), Xassign(void), Xsimple(void), Xpopm(void);
extern void Xrdcmds(void), Xwastrue(void), Xif(void), Xifnot(void), Xpipewait(void);
extern void Xdelhere(void), Xpopredir(void), Xsub(void), Xeflag(void), Xsettrue(void);
extern void Xerror(char*);
extern void Xerror1(char*);
struct word{
char *word;
word *next;
};
struct list{
word *words;
list *next;
};
word *newword(char *, word *), *copywords(word *, word *);
struct redir{
char type;
short from, to;
struct redir *next;
};
#define	NSTATUS	ERRMAX
#define	ROPEN	1
#define	RDUP	2
#define	RCLOSE	3
struct thread{
union code *code;
int pc;
struct list *argv;
struct redir *redir;
struct redir *startredir;
struct var *local;
char *cmdfile;
struct io *cmdfd;
int iflast;
int eof;
int iflag;
int lineno;
int pid;
char status[NSTATUS];
tree *treenodes;
thread *ret;
};
thread *runq;
code *codecopy(code*);
code *codebuf;
int ntrap;
int trap[NSIG];
struct builtin{
char *name;
void (*fnc)(void);
};
extern struct builtin Builtin[];
int eflagok;
int havefork;
void execcd(void), execwhatis(void), execeval(void), execexec(void);
int execforkexec(void);
void execexit(void), execshift(void);
void execwait(void), execumask(void), execdot(void), execflag(void);
void execfunc(var*), execcmds(io *);