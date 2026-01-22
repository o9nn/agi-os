typedef struct Pqueue	Pqueue;
typedef struct Rgrp		Rgrp;
typedef struct Tqueue	Tqueue;
typedef struct Thread	Thread;
typedef struct Execargs	Execargs;
typedef struct Proc		Proc;
typedef enum
{
Dead,
Running,
Ready,
Rendezvous,
} State;
typedef enum
{
Channone,
Chanalt,
Chansend,
Chanrecv,
} Chanstate;
enum
{
RENDHASH = 13,
Printsize = 2048,
NPRIV = 8,
};
struct Rgrp
{
Lock		lock;
Thread	*hash[RENDHASH];
};
struct Tqueue
{
int		asleep;
Thread	*head;
Thread	**tail;
};
struct Thread
{
Lock		lock;
jmp_buf		sched;
int		id;
int 		grp;
int		moribund;
State		state;
State		nextstate;
uchar		*stk;
uint		stksize;
Thread		*next;
Proc		*proc;
Thread		*nextt;
int		ret;
char		*cmdname;
int		inrendez;
Thread		*rendhash;
void*		rendtag;
void*		rendval;
int		rendbreak;
Chanstate	chan;
Alt		*alt;
void*	udata[NPRIV];
};
struct Execargs
{
char		*prog;
char		**args;
int		fd[2];
};
struct Proc
{
Lock		lock;
jmp_buf		sched;
int		pid;
int		splhi;
Thread		*thread;
int		needexec;
Execargs	exec;
Proc		*newproc;
char		exitstr[ERRMAX];
int		rforkflag;
int		nthreads;
Tqueue		threads;
Tqueue		ready;
Lock		readylock;
char		printbuf[Printsize];
int		blocked;
int		pending;
int		nonotes;
uint		nextID;
Proc		*next;
void		*arg;
char		str[ERRMAX];
void*		wdata;
void*		udata;
char		threadint;
};
struct Pqueue {
Lock		lock;
Proc		*head;
Proc		**tail;
};
struct Ioproc
{
int tid;
Channel *c, *creply;
int inuse;
long (*op)(va_list*);
va_list arg;
long ret;
char err[ERRMAX];
Ioproc *next;
};
void	_freeproc(Proc*);
void	_freethread(Thread*);
Proc*	_newproc(void(*)(void*), void*, uint, char*, int, int);
int	_procsplhi(void);
void	_procsplx(int);
void	_sched(void);
int	_schedexec(Execargs*);
void	_schedexecwait(void);
void	_schedexit(Proc*);
int	_schedfork(Proc*);
void	_schedinit(void*);
void	_systhreadinit(void);
void	_threadassert(char*);
void	_threadbreakrendez(void);
void	_threaddebug(ulong, char*, ...);
void	_threadexitsall(char*);
void	_threadflagrendez(Thread*);
Proc*	_threadgetproc(void);
void	_threadsetproc(Proc*);
void	_threadinitstack(Thread*, void(*)(void*), void*);
void*	_threadmalloc(long, int);
void	_threadnote(void*, char*);
void	_threadready(Thread*);
void*	_threadrendezvous(void*, void*);
void	_threadsignal(void);
void	_threadsysfatal(char*, va_list);
void**	_workerdata(void);
void	_xinc(long*);
long	_xdec(long*);
extern int			_threaddebuglevel;
extern char*		_threadexitsallstatus;
extern Pqueue		_threadpq;
extern Channel*	_threadwaitchan;
extern Rgrp		_threadrgrp;
#define DBGAPPL	(1 << 0)
#define DBGSCHED	(1 << 16)
#define DBGCHAN	(1 << 17)
#define DBGREND	(1 << 18)
#define DBGNOTE	(1 << 20)
#define DBGEXEC	(1 << 21)
#define ioproc_arg(io, type)	(va_arg((io)->arg, type))