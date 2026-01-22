struct table {
Area   *areap;
short	size, nfree;
struct	tbl **tbls;
};
struct tbl {
Tflag	flag;
int	type;
Area	*areap;
union {
char *s;
long i;
int (*f) ARGS((char **));
struct op *t;
} val;
int	index;
union {
int	field;
int errno_;
} u2;
union {
struct tbl *array;
char *fpath;
} u;
char	name[4];
};
#define	ALLOC		BIT(0)
#define	DEFINED		BIT(1)
#define	ISSET		BIT(2)
#define	EXPORT		BIT(3)
#define	TRACE		BIT(4)
#define	SPECIAL		BIT(8)
#define	INTEGER		BIT(9)
#define	RDONLY		BIT(10)
#define	LOCAL		BIT(11)
#define ARRAY		BIT(13)
#define LJUST		BIT(14)
#define RJUST		BIT(15)
#define ZEROFIL		BIT(16)
#define LCASEV		BIT(17)
#define UCASEV_AL	BIT(18)
#define INT_U		BIT(19)
#define INT_L		BIT(20)
#define IMPORT		BIT(21)
#define LOCAL_COPY	BIT(22)
#define EXPRINEVAL	BIT(23)
#define EXPRLVALUE	BIT(24)
#define KEEPASN		BIT(8)
#define FINUSE		BIT(9)
#define FDELETE		BIT(10)
#define FKSH		BIT(11)
#define SPEC_BI		BIT(12)
#define REG_BI		BIT(13)
#define USERATTRIB	(EXPORT|INTEGER|RDONLY|LJUST|RJUST|ZEROFIL\
|LCASEV|UCASEV_AL|INT_U|INT_L)
#define	CNONE	0
#define	CSHELL	1
#define	CFUNC	2
#define	CEXEC	4
#define	CALIAS	5
#define	CKEYWD	6
#define CTALIAS	7
#define FC_SPECBI	BIT(0)
#define FC_FUNC		BIT(1)
#define FC_REGBI	BIT(2)
#define FC_UNREGBI	BIT(3)
#define FC_BI		(FC_SPECBI|FC_REGBI|FC_UNREGBI)
#define FC_PATH		BIT(4)
#define FC_DEFPATH	BIT(5)
#define AF_ARGV_ALLOC	0x1
#define AF_ARGS_ALLOCED	0x2
#define AI_ARGV(a, i)	((i) == 0 ? (a).argv[0] : (a).argv[(i) - (a).skip])
#define AI_ARGC(a)	((a).argc_ - (a).skip)
struct arg_info {
int flags;
char **argv;
int argc_;
int skip;
};
struct block {
Area	area;
char	**argv;
int	argc;
int	flags;
struct	table vars;
struct	table funs;
Getopt	getopts_state;
#if 1
char *	error;
char *	exit;
#else
Trap	error, exit;
#endif
struct	block *next;
};
#define BF_DOGETOPTS	BIT(0)
struct tstate {
int left;
struct tbl **next;
};
EXTERN	struct table taliases;
EXTERN	struct table builtins;
EXTERN	struct table aliases;
EXTERN	struct table keywords;
EXTERN	struct table homedirs;
struct builtin {
const char   *name;
int  (*func) ARGS((char **));
};
extern const struct builtin shbuiltins [], kshbuiltins [];
#define	V_NONE			0
#define	V_PATH			1
#define	V_IFS			2
#define	V_SECONDS		3
#define	V_OPTIND		4
#define	V_MAIL			5
#define	V_MAILPATH		6
#define	V_MAILCHECK		7
#define	V_RANDOM		8
#define V_HISTSIZE		9
#define V_HISTFILE		10
#define V_VISUAL		11
#define V_EDITOR		12
#define V_COLUMNS		13
#define V_POSIXLY_CORRECT	14
#define V_TMOUT			15
#define V_TMPDIR		16
#define V_LINENO		17
#define PS1	0
#define PS2	1
EXTERN char *path;
EXTERN const char *def_path;
EXTERN char *tmpdir;
EXTERN const char *prompt;
EXTERN int cur_prompt;
EXTERN int current_lineno;