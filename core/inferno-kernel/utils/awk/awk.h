typedef double	Awkfloat;
typedef	unsigned char uschar;
#define	xfree(a)	{ if ((a) != NULL) { free((char *) a); a = NULL; } }
#define	DEBUG
#ifdef	DEBUG
#	define	dprintf(x)	if (dbg) printf x
#else
#	define	dprintf(x)
#endif
extern	char	errbuf[];
extern int	compile_time;
extern int	safe;
#define	RECSIZE	(8 * 1024)
extern int	recsize;
extern char	**FS;
extern char	**RS;
extern char	**ORS;
extern char	**OFS;
extern char	**OFMT;
extern Awkfloat *NR;
extern Awkfloat *FNR;
extern Awkfloat *NF;
extern char	**FILENAME;
extern char	**SUBSEP;
extern Awkfloat *RSTART;
extern Awkfloat *RLENGTH;
extern char	*record;
extern int	lineno;
extern int	errorflag;
extern int	donefld;
extern int	donerec;
extern char	inputFS[];
extern int	dbg;
extern	char	*patbeg;
extern	int	patlen;
typedef struct Cell {
uschar	ctype;
uschar	csub;
char	*nval;
char	*sval;
Awkfloat fval;
int	 tval;
struct Cell *cnext;
} Cell;
typedef struct Array {
int	nelem;
int	size;
Cell	**tab;
} Array;
#define	NSYMTAB	50
extern Array	*symtab;
extern Cell	*nrloc;
extern Cell	*fnrloc;
extern Cell	*nfloc;
extern Cell	*rstartloc;
extern Cell	*rlengthloc;
#define	NUM	01
#define	STR	02
#define DONTFREE 04
#define	CON	010
#define	ARR	020
#define	FCN	040
#define FLD	0100
#define	REC	0200
#define	FLENGTH	1
#define	FSQRT	2
#define	FEXP	3
#define	FLOG	4
#define	FINT	5
#define	FSYSTEM	6
#define	FRAND	7
#define	FSRAND	8
#define	FSIN	9
#define	FCOS	10
#define	FATAN	11
#define	FTOUPPER 12
#define	FTOLOWER 13
#define	FFLUSH	14
typedef struct Node {
int	ntype;
struct	Node *nnext;
int	lineno;
int	nobj;
struct	Node *narg[1];
} Node;
#define	NIL	((Node *) 0)
extern Node	*winner;
extern Node	*nullstat;
extern Node	*nullnode;
#define OCELL	1
#define OBOOL	2
#define OJUMP	3
#define	CFREE	7
#define CCOPY	6
#define CCON	5
#define CTEMP	4
#define CNAME	3
#define CVAR	2
#define CFLD	1
#define	CUNK	0
#define BTRUE	11
#define BFALSE	12
#define JEXIT	21
#define JNEXT	22
#define	JBREAK	23
#define	JCONT	24
#define	JRET	25
#define	JNEXTFILE	26
#define NVALUE	1
#define NSTAT	2
#define NEXPR	3
extern	int	pairstack[], paircnt;
#define notlegal(n)	(n <= FIRSTTOKEN || n >= LASTTOKEN || proctab[n-FIRSTTOKEN] == nullproc)
#define isvalue(n)	((n)->ntype == NVALUE)
#define isexpr(n)	((n)->ntype == NEXPR)
#define isjump(n)	((n)->ctype == OJUMP)
#define isexit(n)	((n)->csub == JEXIT)
#define	isbreak(n)	((n)->csub == JBREAK)
#define	iscont(n)	((n)->csub == JCONT)
#define	isnext(n)	((n)->csub == JNEXT || (n)->csub == JNEXTFILE)
#define	isret(n)	((n)->csub == JRET)
#define isrec(n)	((n)->tval & REC)
#define isfld(n)	((n)->tval & FLD)
#define isstr(n)	((n)->tval & STR)
#define isnum(n)	((n)->tval & NUM)
#define isarr(n)	((n)->tval & ARR)
#define isfcn(n)	((n)->tval & FCN)
#define istrue(n)	((n)->csub == BTRUE)
#define istemp(n)	((n)->csub == CTEMP)
#define	isargument(n)	((n)->nobj == ARG)
#define freeable(p)	( ((p)->tval & (STR|DONTFREE)) == STR )
#define NCHARS	(256+1)
#define NSTATES	32
typedef struct rrow {
long	ltype;
union {
int i;
Node *np;
uschar *up;
} lval;
int	*lfollow;
} rrow;
typedef struct fa {
uschar	gototab[NSTATES][NCHARS];
uschar	out[NSTATES];
uschar	*restr;
int	*posns[NSTATES];
int	anchor;
int	use;
int	initstat;
int	curstat;
int	accept;
int	reset;
struct	rrow re[1];
} fa;
#include "proto.h"