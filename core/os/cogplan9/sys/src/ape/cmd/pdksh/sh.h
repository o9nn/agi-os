#include "config.h"
#ifdef HAVE_PROTOTYPES
# define	ARGS(args)	args
#else
# define	ARGS(args)	()
#endif
#include <stdio.h>
#include <sys/types.h>
#include <setjmp.h>
#ifdef HAVE_STDDEF_H
# include <stddef.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#else
extern char * getenv  ARGS((const char *));
extern void * malloc  ARGS((size_t));
extern void * realloc ARGS((void *, size_t));
extern int    free    ARGS((void *));
extern int    exit    ARGS((int));
extern int    rand    ARGS((void));
extern void   srand   ARGS((unsigned int));
extern int    atoi    ARGS((const char *));
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
extern int access ARGS((const char *, int));
extern int open ARGS((const char *, int, ...));
extern int creat ARGS((const char *, mode_t));
extern int read ARGS((int, char *, unsigned));
extern int write ARGS((int, const char *, unsigned));
extern off_t lseek ARGS((int, off_t, int));
extern int close ARGS((int));
extern int pipe ARGS((int []));
extern int dup2 ARGS((int, int));
extern int unlink ARGS((const char *));
extern int fork ARGS((void));
extern int execve ARGS((const char *, char * const[], char * const[]));
extern int chdir ARGS((const char *));
extern int kill ARGS((pid_t, int));
extern char *getcwd();
extern int geteuid ARGS((void));
extern int readlink ARGS((const char *, char *, int));
extern int getegid ARGS((void));
extern int getpid ARGS((void));
extern int getppid ARGS((void));
extern unsigned int sleep ARGS((unsigned int));
extern int isatty ARGS((int));
# ifdef POSIX_PGRP
extern int getpgrp ARGS((void));
extern int setpgid ARGS((pid_t, pid_t));
# endif
# ifdef BSD_PGRP
extern int getpgrp ARGS((pid_t));
extern int setpgrp ARGS((pid_t, pid_t));
# endif
# ifdef SYSV_PGRP
extern int getpgrp ARGS((void));
extern int setpgrp ARGS((void));
# endif
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#else
# include <strings.h>
# define strchr index
# define strrchr rindex
#endif
#ifndef HAVE_STRSTR
char *strstr ARGS((const char *s, const char *p));
#endif
#ifndef HAVE_STRCASECMP
int strcasecmp ARGS((const char *s1, const char *s2));
int strncasecmp ARGS((const char *s1, const char *s2, int n));
#endif
#ifdef HAVE_MEMORY_H
# include <memory.h>
#endif
#ifndef HAVE_MEMSET
# define memcpy(d, s, n)	bcopy(s, d, n)
# define memcmp(s1, s2, n)	bcmp(s1, s2, n)
void *memset ARGS((void *d, int c, size_t n));
#endif
#ifndef HAVE_MEMMOVE
# ifdef HAVE_BCOPY
#  define memmove(d, s, n)	bcopy(s, d, n)
# else
void *memmove ARGS((void *d, const void *s, size_t n));
# endif
#endif
#ifdef HAVE_PROTOTYPES
# include <stdarg.h>
# define SH_VA_START(va, argn) va_start(va, argn)
#else
# include <varargs.h>
# define SH_VA_START(va, argn) va_start(va)
#endif
#include <errno.h>
extern int errno;
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#else
# include <sys/file.h>
#endif
#ifndef O_ACCMODE
# define O_ACCMODE	(O_RDONLY|O_WRONLY|O_RDWR)
#endif
#ifndef F_OK
# define F_OK 0
# define X_OK 1
# define W_OK 2
# define R_OK 4
#endif
#ifndef SEEK_SET
# ifdef L_SET
#  define SEEK_SET L_SET
#  define SEEK_CUR L_INCR
#  define SEEK_END L_XTND
# else
#  define SEEK_SET 0
#  define SEEK_CUR 1
#  define SEEK_END 2
# endif
#endif
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
#include <signal.h>
#ifdef	NSIG
# define SIGNALS	NSIG
#else
# ifdef	_MINIX
#  define SIGNALS	(_NSIG+1)
# else
#  ifdef _SIGMAX
#   define SIGNALS	_SIGMAX
#  else
#   define SIGNALS	32
#  endif
# endif
#endif
#ifndef SIGCHLD
# define SIGCHLD SIGCLD
#endif
#ifdef SA_INTERRUPT
# define KSH_SA_FLAGS	SA_INTERRUPT
#else
# define KSH_SA_FLAGS	0
#endif
typedef	RETSIGTYPE (*handler_t) ARGS((int));
#ifdef USE_FAKE_SIGACT
# include "sigact.h"
#endif
#ifdef HAVE_PATHS_H
# include <paths.h>
#endif
#ifdef _PATH_DEFPATH
# define DEFAULT__PATH _PATH_DEFPATH
#else
# define DEFAULT__PATH DEFAULT_PATH
#endif
#ifndef offsetof
# define offsetof(type,id) ((size_t)&((type*)NULL)->id)
#endif
#ifndef HAVE_KILLPG
# define killpg(p, s)	kill(-(p), (s))
#endif
#ifdef OS2
extern int ksh_execve(char *cmd, char **args, char **env, int flags);
#else
# if defined(OS_ISC) && defined(_POSIX_SOURCE)
#  define ksh_execve(p, av, ev, flags) \
do { \
__setostype(0); \
execve(p, av, ev); \
__setostype(1); \
} while (0)
# else
#  define ksh_execve(p, av, ev, flags)	execve(p, av, ev)
# endif
#endif
#define ksh_dupbase(fd, base) fcntl(fd, F_DUPFD, base)
#ifdef HAVE_SIGSETJMP
# define ksh_sigsetjmp(env,sm)	sigsetjmp((env), (sm))
# define ksh_siglongjmp(env,v)	siglongjmp((env), (v))
# define ksh_jmp_buf		sigjmp_buf
#else
# ifdef HAVE__SETJMP
#  define ksh_sigsetjmp(env,sm)	_setjmp(env)
#  define ksh_siglongjmp(env,v)	_longjmp((env), (v))
# else
#  define ksh_sigsetjmp(env,sm)	setjmp(env)
#  define ksh_siglongjmp(env,v)	longjmp((env), (v))
# endif
# define ksh_jmp_buf		jmp_buf
#endif
#ifndef HAVE_DUP2
extern int dup2 ARGS((int, int));
#endif
#ifdef __OLD__
#if SIZEOF_INT >= 4
# define INT32	long
# if SIZEOF_LONG >= 4
#  define INT32	long
# else
#error cannot find 32 bit type...
# endif
#endif
#endif
#define INT32 long
#if defined(__GNUC__) || defined(lint)
# define UNINITIALIZED(var)	var = 0
#else
# define UNINITIALIZED(var)	var
#endif
#ifdef EXTERN
# define I__(i) = i
#else
# define I__(i)
# define EXTERN extern
# define EXTERN_DEFINED
#endif
#ifdef OS2
# define inDOS() (!(_emx_env & 0x200))
#endif
#ifndef EXECSHELL
# ifdef OS2
#  define EXECSHELL	(inDOS() ? "c:\\command.com" : "c:\\os2\\cmd.exe")
#  define EXECSHELL_STR	(inDOS() ? "COMSPEC" : "OS2_SHELL")
# else
#  define EXECSHELL	"/bin/sh"
#  define EXECSHELL_STR	"EXECSHELL"
# endif
#endif
#ifdef OS2
# define PATHSEP        ';'
# define DIRSEP         '/'
# define DIRSEPSTR      "\\"
# define ISDIRSEP(c)    ((c) == '\\' || (c) == '/')
# define ISABSPATH(s)	(((s)[0] && (s)[1] == ':' && ISDIRSEP((s)[2])))
# define ISROOTEDPATH(s) (ISDIRSEP((s)[0]) || ISABSPATH(s))
# define ISRELPATH(s)	(!(s)[0] || ((s)[1] != ':' && !ISDIRSEP((s)[0])))
# define FILECHCONV(c)	(isascii(c) && isupper(c) ? tolower(c) : c)
# define FILECMP(s1, s2) stricmp(s1, s2)
# define FILENCMP(s1, s2, n) strnicmp(s1, s2, n)
extern char *ksh_strchr_dirsep(const char *path);
extern char *ksh_strrchr_dirsep(const char *path);
# define chdir          _chdir2
# define getcwd         _getcwd2
#else
# define PATHSEP        ':'
# define DIRSEP         '/'
# define DIRSEPSTR      "/"
# define ISDIRSEP(c)    ((c) == '/')
#ifdef __CYGWIN__
#  define ISABSPATH(s) \
(((s)[0] && (s)[1] == ':' && ISDIRSEP((s)[2])) || ISDIRSEP((s)[0]))
#  define ISRELPATH(s) (!(s)[0] || ((s)[1] != ':' && !ISDIRSEP((s)[0])))
#else
# define ISABSPATH(s)	ISDIRSEP((s)[0])
# define ISRELPATH(s)	(!ISABSPATH(s))
#endif
# define ISROOTEDPATH(s) ISABSPATH(s)
# define FILECHCONV(c)	c
# define FILECMP(s1, s2) strcmp(s1, s2)
# define FILENCMP(s1, s2, n) strncmp(s1, s2, n)
# define ksh_strchr_dirsep(p)   strchr(p, DIRSEP)
# define ksh_strrchr_dirsep(p)  strrchr(p, DIRSEP)
#endif
typedef int bool_t;
#define	FALSE	0
#define	TRUE	1
#define	NELEM(a) (sizeof(a) / sizeof((a)[0]))
#define	sizeofN(type, n) (sizeof(type) * (n))
#define	BIT(i)	(1<<(i))
typedef INT32 Tflag;
#define	NUFILE	10
#define	FDBASE	10
#define	eaccess(path, mode)	access(path, mode)
#define	MAGIC		(7)
#define ISMAGIC(c)	((unsigned char)(c) == MAGIC)
#define	NOT		'!'
#define	LINE	1024
#define	PATH	1024
#define ARRAYMAX 1023
EXTERN	const char *kshname;
EXTERN	pid_t	kshpid;
EXTERN	pid_t	procpid;
EXTERN	int	ksheuid;
EXTERN	int	exstat;
EXTERN	int	subst_exstat;
EXTERN	const char *safe_prompt;
typedef struct Area {
struct Block *freelist;
} Area;
EXTERN	Area	aperm;
#define	APERM	&aperm
#define	ATEMP	&e->area
#ifdef MEM_DEBUG
# include "chmem.h"
#endif
#ifdef KSH_DEBUG
# define kshdebug_init()	kshdebug_init_()
# define kshdebug_printf(a)	kshdebug_printf_ a
# define kshdebug_dump(a)	kshdebug_dump_ a
#else
# define kshdebug_init()
# define kshdebug_printf(a)
# define kshdebug_dump(a)
#endif
EXTERN	struct env {
short	type;
short	flags;
Area	area;
struct	block *loc;
short  *savefd;
struct	env *oenv;
ksh_jmp_buf jbuf;
struct temp *temps;
} *e;
#define	E_NONE	0
#define	E_PARSE	1
#define	E_FUNC	2
#define	E_INCL	3
#define	E_EXEC	4
#define	E_LOOP	5
#define	E_ERRH	6
#define EF_FUNC_PARSE	BIT(0)
#define EF_BRKCONT_PASS	BIT(1)
#define EF_FAKE_SIGDIE	BIT(2)
#define STOP_BRKCONT(t)	((t) == E_NONE || (t) == E_PARSE \
|| (t) == E_FUNC || (t) == E_INCL)
#define STOP_RETURN(t)	((t) == E_FUNC || (t) == E_INCL)
#define LRETURN	1
#define	LEXIT	2
#define LERROR	3
#define LLEAVE	4
#define LINTR	5
#define	LBREAK	6
#define	LCONTIN	7
#define LSHELL	8
#define LAEXPR	9
#define OF_CMDLINE	0x01
#define OF_SET		0x02
#define OF_SPECIAL	0x04
#define OF_INTERNAL	0x08
#define OF_ANY		(OF_CMDLINE | OF_SET | OF_SPECIAL | OF_INTERNAL)
struct option {
const char	*name;
char	c;
short	flags;
};
extern const struct option options[];
enum sh_flag {
FEXPORT = 0,
#ifdef BRACE_EXPAND
FBRACEEXPAND,
#endif
FBGNICE,
FCOMMAND,
#ifdef EMACS
FEMACS,
#endif
FERREXIT,
#ifdef EMACS
FGMACS,
#endif
FIGNOREEOF,
FTALKING,
FKEYWORD,
FLOGIN,
FMARKDIRS,
FMONITOR,
FNOCLOBBER,
FNOEXEC,
FNOGLOB,
FNOHUP,
FNOTTALKING,
FNOLOG,
#ifdef	JOBS
FNOTIFY,
#endif
FNOUNSET,
FPHYSICAL,
FPOSIX,
FPRIVILEGED,
FRESTRICTED,
FSTDIN,
FTRACKALL,
FVERBOSE,
#ifdef VI
FVI,
FVIRAW,
FVISHOW8,
FVITABCOMPLETE,
FVIESCCOMPLETE,
#endif
FXTRACE,
FTALKING_I,
FNFLAGS
};
#define Flag(f)	(shell_flags[(int) (f)])
EXTERN	char shell_flags [FNFLAGS];
EXTERN	char	null [] I__("");
EXTERN	char	space [] I__(" ");
EXTERN	char	newline [] I__("\n");
EXTERN	char	slash [] I__("/");
enum temp_type {
TT_HEREDOC_EXP,
TT_HIST_EDIT
};
typedef enum temp_type Temp_type;
struct temp {
struct temp	*next;
struct shf	*shf;
int		pid;
Temp_type	type;
char		*name;
};
#define shl_spare	(&shf_iob[0])
#define shl_stdout	(&shf_iob[1])
#define shl_out		(&shf_iob[2])
EXTERN int shl_stdout_ok;
typedef struct trap {
int	signal;
const char *name;
const char *mess;
char   *trap;
int	volatile set;
int	flags;
handler_t cursig;
handler_t shtrap;
} Trap;
#define TF_SHELL_USES	BIT(0)
#define TF_USER_SET	BIT(1)
#define TF_ORIG_IGN	BIT(2)
#define TF_ORIG_DFL	BIT(3)
#define TF_EXEC_IGN	BIT(4)
#define TF_EXEC_DFL	BIT(5)
#define TF_DFL_INTR	BIT(6)
#define TF_TTY_INTR	BIT(7)
#define TF_CHANGED	BIT(8)
#define TF_FATAL	BIT(9)
#define SS_RESTORE_MASK	0x3
#define SS_RESTORE_CURR	0
#define SS_RESTORE_ORIG	1
#define SS_RESTORE_DFL	2
#define SS_RESTORE_IGN	3
#define SS_FORCE	BIT(3)
#define SS_USER		BIT(4)
#define SS_SHTRAP	BIT(5)
#define SIGEXIT_	0
#define SIGERR_		SIGNALS
EXTERN	int volatile trap;
EXTERN	int volatile intrsig;
EXTERN	int volatile fatal_trap;
#ifndef FROM_TRAP_C
extern	Trap	sigtraps[SIGNALS+1];
#endif
#ifdef KSH
enum tmout_enum {
TMOUT_EXECUTING	= 0,
TMOUT_READING,
TMOUT_LEAVING
};
EXTERN unsigned int ksh_tmout;
EXTERN enum tmout_enum ksh_tmout_state I__(TMOUT_EXECUTING);
#endif
EXTERN int really_exit;
#define	C_ALPHA	 BIT(0)
#define	C_DIGIT	 BIT(1)
#define	C_LEX1	 BIT(2)
#define	C_VAR1	 BIT(3)
#define	C_IFSWS	 BIT(4)
#define	C_SUBOP1 BIT(5)
#define	C_SUBOP2 BIT(6)
#define	C_IFS	 BIT(7)
#define	C_QUOTE	 BIT(8)
extern	short ctypes [];
#define	ctype(c, t)	!!(ctypes[(unsigned char)(c)]&(t))
#define	letter(c)	ctype(c, C_ALPHA)
#define	digit(c)	ctype(c, C_DIGIT)
#define	letnum(c)	ctype(c, C_ALPHA|C_DIGIT)
EXTERN int ifs0 I__(' ');
#define GF_ERROR	BIT(0)
#define GF_PLUSOPT	BIT(1)
#define GF_NONAME	BIT(2)
#define GI_MINUS	BIT(0)
#define GI_PLUS		BIT(1)
#define GI_MINUSMINUS	BIT(2)
typedef struct {
int		optind;
int		uoptind;
char		*optarg;
int		flags;
int		info;
unsigned int	p;
char		buf[2];
} Getopt;
EXTERN Getopt builtin_opt;
EXTERN Getopt user_opt;
#ifdef KSH
typedef INT32 Coproc_id;
struct coproc {
int	read;
int	readw;
int	write;
Coproc_id id;
int	njobs;
void    *job;
};
EXTERN struct coproc coproc;
#endif
#ifdef JOB_SIGS
EXTERN sigset_t		sm_default, sm_sigchld;
#endif
extern const char ksh_version[];
EXTERN char	*builtin_argv0;
EXTERN Tflag	builtin_flag;
EXTERN char	*current_wd;
EXTERN int	current_wd_size;
#ifdef EDIT
# define MIN_EDIT_SPACE	7
# define MIN_COLS	(2 + MIN_EDIT_SPACE + 3)
EXTERN	int	x_cols I__(80);
#else
# define x_cols 80
#endif
#define OPAREN	'('
#define CPAREN	')'
#define OBRACK	'['
#define CBRACK	']'
#define OBRACE	'{'
#define CBRACE	'}'
#ifndef KSH_SYSTEM_PROFILE
# ifdef __NeXT
#  define KSH_SYSTEM_PROFILE "/etc/profile.std"
# else
#  define KSH_SYSTEM_PROFILE "/etc/profile"
# endif
#endif
#define KSH_UNWIND_ERROR	0
#define KSH_RETURN_ERROR	1
#include "shf.h"
#include "table.h"
#include "tree.h"
#include "expand.h"
#include "lex.h"
#include "proto.h"
#ifdef EXTERN_DEFINED
# undef EXTERN_DEFINED
# undef EXTERN
#endif
#undef I__