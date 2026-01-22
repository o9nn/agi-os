#define INFERNO_KEEPENVIRON
#include	<lib9.h>
#define	Lock	Rclock
#define	Ref		Rcref
typedef union Code	Code;
typedef struct Tree	Tree;
typedef struct Thread	Thread;
typedef struct Word	Word;
typedef struct Var	Var;
typedef struct List	List;
typedef struct Redir	Redir;
typedef struct Io	Io;
typedef struct Here	Here;
typedef struct Ref	Ref;
typedef struct Lock	Lock;
typedef	struct Direntry Direntry;
#define	EOF	(-1)
#define	NBUF	512
#define	APPEND	1
#define	WRITE	2
#define	READ	3
#define	HERE	4
#define	DUPFD	5
#define	CLOSE	6
#define	ROPEN	1
#define	RDUP	2
#define	RCLOSE	3
#define	NSTATUS	64
#define	IWS	0x01
#define	GLOB	((char)0x02)
union Code {
void	(*f)(void);
int	i;
char	*s;
};
struct Tree
{
int	type;
int	rtype, fd0, fd1;
char	*str;
int	quoted;
int	iskw;
Tree	*child[3];
Tree	*next;
};
struct Thread
{
Code	*code;
int	pc;
List	*argv;
Redir	*redir;
Redir	*startredir;
Var	*local;
char	*cmdfile;
Io	*cmdfd;
int	iflast;
int	eof;
int	iflag;
int	lineno;
int	pid;
char	status[NSTATUS];
Tree	*treenodes;
Thread	*ret;
};
struct Io
{
int	fd;
char	*bufp;
char	*ebuf;
char	*strp;
char	buf[NBUF];
};
struct Var
{
char	*name;
Word	*val;
int	changed;
Code	*fn;
int	fnchanged;
int	pc;
Var	*next;
};
struct Word
{
char	*word;
Word	*next;
};
struct List
{
Word	*words;
List	*next;
};
struct Redir
{
char	type;
short	from, to;
Redir	*next;
};
struct Here{
Tree	*tag;
char	*name;
Here	*next;
};
struct Lock {
int	val;
};
struct Ref
{
Lock	lk;
int	ref;
};
struct	Direntry
{
int	isdir;
char	*name;
};
void	start(Code *c, int pc, Var *local);
void	yyerror(char*);
int	yylex(void);
int	yyparse(void);
int	wordchr(int);
int	idchr(int);
int	compile(Tree*);
Code	*codecopy(Code*);
void	codefree(Code*);
void	cleanhere(char *f);
void	skipnl(void);
void	panic(char*, int);
void	kinit(void);
void	vinit(void);
Var	*vlook(char*);
Var	*gvlook(char*);
Var	*newvar(char*, Var*);
void	setvar(char*, Word*);
void	updenv(void);
void	kenter(int type, char *name);
void	deglob(char*);
void	globlist(void);
int	match(char *s, char *p, int stop);
void	setstatus(char *s);
char	*getstatus(void);
int	truestatus(void);
void	execcmds(Io*);
char	*concstatus(char *s, char *t);
char	**procargv(char*, char*, char*, char*, Word *w);
void	freewords(Word*);
Tree	*newtree(void);
Tree	*token(char*, int), *klook(char*), *tree1(int, Tree*);
Tree	*tree2(int, Tree*, Tree*), *tree3(int, Tree*, Tree*, Tree*);
Tree	*mung1(Tree*, Tree*), *mung2(Tree*, Tree*, Tree*);
Tree	*mung3(Tree*, Tree*, Tree*, Tree*), *epimung(Tree*, Tree*);
Tree	*simplemung(Tree*), *heredoc(Tree*);
void	freetree(Tree*);
void	freenodes(void);
Tree	*heredoc(Tree *tag);
extern void Xappend(void), Xasync(void), Xbackq(void), Xbang(void), Xclose(void);
extern void Xconc(void), Xcount(void), Xdelfn(void), Xdol(void), Xqdol(void), Xdup(void);
extern void Xexit(void), Xfalse(void), Xfn(void), Xfor(void), Xglob(void);
extern void Xjump(void), Xmark(void), Xmatch(void), Xpipe(void), Xread(void);
extern void Xunredir(void), Xstar(void), Xreturn(void), Xsubshell(void);
extern void Xtrue(void), Xword(void), Xwrite(void), Xpipefd(void), Xcase(void);
extern void Xlocal(void), Xunlocal(void), Xassign(void), Xsimple(void), Xpopm(void);
extern void Xrdcmds(void), Xwastrue(void), Xif(void), Xifnot(void), Xpipewait(void);
extern void Xdelhere(void), Xpopredir(void), Xsub(void), Xeflag(void), Xsettrue(void);
extern void Xerror(char*), Xperror(char*);
Word	*newword(char*, Word*);
void	pushlist(void);
void	poplist(void);
void	pushword(char*);
void	popword(void);
int	count(Word*);
Word	*copywords(Word*, Word*);
void	pushredir(int, int, int);
void	turfredir(void);
char	*list2str(Word*);
void	freelist(Word*);
Word	*conclist(Word*, Word*, Word*);
Word  	*subwords(Word*, int, Word*, Word*);
#define	pchr(b, c) if((b)->bufp==(b)->ebuf)fullbuf((b), (c));else (*(b)->bufp++=(c))
#define	rchr(b) ((b)->bufp==(b)->ebuf?emptybuf(b):(*(b)->bufp++&0xff))
Io	*openfd(int), *openstr(void), *opencore(char*, int);
int	emptybuf(Io*);
void	closeio(Io*);
void	flush(Io*);
int	fullbuf(Io*, int);
void	pfmt(Io*, char*, ...);
void	perr(Io*);
void	pstr(Io*, char*);
void	pfnc(Io*, Thread*);
void	pprompt(void);
void	dotrap(void);
void	dointr(void);
void	waitfor(uint);
Direntry* readdirect(char*);
void	fatal(char*, ...);
uint	proc(char**, int, int, int);
int	procwait(uint);
int	refinc(Ref*);
int	refdec(Ref*);
int	pipe(int*);
#define	onebyte(c)	((c&0x80)==0x00)
#define	twobyte(c)	((c&0xe0)==0xc0)
#define	threebyte(c)	((c&0xf0)==0xe0)
#define	new(type)	((type *)malloc(sizeof(type)))
extern Tree	*cmdtree;
extern Thread	*runq;
extern Io	*err;
extern int	flag[256];
extern int	doprompt;
extern char	*promptstr;
extern int	ndot;
extern int	nerror;
extern Code	*codebuf;
extern int	eflagok;
extern int	interrupted;
extern Ref	ntrap;