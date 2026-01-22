#ifndef Unix
#include <u.h>
#include <libc.h>
#define	NSIG	32
#define	SIGINT	2
#define	SIGQUIT	3
#define fcntl(fd, op, arg)
#define F_SETFD
#define FD_CLOEXEC
#else
#include "unix.h"
#endif
#ifndef ERRMAX
#define ERRMAX 128
#endif
#define	YYMAXDEPTH	500
#ifndef YYPREFIX
#ifndef PAREN
#include "x.tab.h"
#endif
#endif
typedef struct tree tree;
typedef struct word word;
typedef struct io io;
typedef union code code;
typedef struct var var;
typedef struct list list;
typedef struct redir redir;
typedef struct thread thread;
typedef struct builtin builtin;
#ifndef Unix
#pragma incomplete word
#pragma incomplete io
#endif
struct tree{
int	type;
int	rtype, fd0, fd1;
char	*str;
int	quoted;
int	iskw;
tree	*child[3];
tree	*next;
};
tree *newtree(void);
tree *token(char*, int), *klook(char*), *tree1(int, tree*);
tree *tree2(int, tree*, tree*), *tree3(int, tree*, tree*, tree*);
tree *mung1(tree*, tree*), *mung2(tree*, tree*, tree*);
tree *mung3(tree*, tree*, tree*, tree*), *epimung(tree*, tree*);
tree *simplemung(tree*), *heredoc(tree*);
void freetree(tree*);
tree *cmdtree;
union code{
void	(*f)(void);
int	i;
char	*s;
};
char *promptstr;
int doprompt;
#define	NTOK	8192
char tok[NTOK + UTFmax];
#define	APPEND	1
#define	WRITE	2
#define	READ	3
#define	HERE	4
#define	DUPFD	5
#define	CLOSE	6
#define RDWR	7
struct var{
char	*name;
word	*val;
int	changed;
code	*fn;
int	fnchanged;
int	pc;
var	*next;
};
var *vlook(char*), *gvlook(char*), *newvar(char*, var*);
#define	NVAR	521
var *gvar[NVAR];
#define	new(type)	((type *)emalloc(sizeof(type)))
void *emalloc(long);
void *Malloc(ulong);
void efree(void *);
struct here{
tree	*tag;
char	*name;
struct here *next;
};
int mypid;
#define	GLOB	'\001'
char **argp;
char **args;
int nerror;
int doprompt;
#define	PRD	0
#define	PWR	1
char *Rcmain, *Fdprefix;
int ndot;
char *getstatus(void);
int lastc;
int lastword;