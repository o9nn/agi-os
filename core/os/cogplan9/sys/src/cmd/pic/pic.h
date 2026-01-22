#ifndef PI
#define PI 3.1415926535897932384626433832795028841971693993751
#endif
#define	MAXWID	8.5
#define	MAXHT	11
#define	dprintf	if(dbg)printf
extern	void	yyerror(char *);
extern	char	errbuf[200];
#define	ERROR	sprintf(errbuf,
#define	FATAL	), yyerror(errbuf), exit(1)
#define	WARNING	), yyerror(errbuf)
#define	DEFAULT	0
#define	HEAD1	1
#define	HEAD2	2
#define	HEAD12	(HEAD1+HEAD2)
#define	INVIS	4
#define	CW_ARC	8
#define	DOTBIT	16
#define	DASHBIT	32
#define	FILLBIT	64
#define NOEDGEBIT 128
#define	CENTER	01
#define	LJUST	02
#define	RJUST	04
#define	ABOVE	010
#define	BELOW	020
#define	SPREAD	040
#define	SCALE	1.0
#define	WID	0.75
#define	WID2	0.375
#define	HT	0.5
#define	HT2	(HT/2)
#define	HT5	(HT/5)
#define	HT10	(HT/10)
#define	H	0
#define	V	1
#define	R_DIR	0
#define	U_DIR	1
#define	L_DIR	2
#define	D_DIR	3
#define	ishor(n)	(((n) & V) == 0)
#define	isvert(n)	(((n) & V) != 0)
#define	isright(n)	((n) == R_DIR)
#define	isleft(n)	((n) == L_DIR)
#define	isdown(n)	((n) == D_DIR)
#define	isup(n)		((n) == U_DIR)
typedef	float	ofloat;
typedef struct obj {
int	o_type;
int	o_count;
int	o_nobj;
int	o_mode;
float	o_x;
float	o_y;
int	o_nt1;
int	o_nt2;
int	o_attr;
int	o_size;
int	o_nhead;
struct symtab *o_symtab;
float	o_ddval;
float	o_fillval;
ofloat	o_val[1];
} obj;
typedef union {
int	i;
char	*p;
obj	*o;
double	f;
struct symtab *st;
} YYSTYPE;
extern	YYSTYPE	yylval, yyval;
struct symtab {
char	*s_name;
int	s_type;
YYSTYPE	s_val;
struct symtab *s_next;
};
typedef struct {
int	a_type;
int	a_sub;
YYSTYPE	a_val;
} Attr;
typedef struct {
int	t_type;
char	t_op;
char	t_size;
char	*t_val;
} Text;
#define	String	01
#define	Macro	02
#define	File	04
#define	Char	010
#define	Thru	020
#define	Free	040
typedef struct {
int	type;
char	*sp;
} Src;
extern	Src	src[], *srcp;
typedef struct {
FILE	*fin;
char	*fname;
int	lineno;
} Infile;
extern	Infile	infile[], *curfile;
#define	MAXARGS	20
typedef struct {
char	*argstk[MAXARGS];
char	*argval;
} Arg;
extern	int	dbg;
extern	obj	**objlist;
extern	int	nobj, nobjlist;
extern	Attr	*attr;
extern	int	nattr, nattrlist;
extern	Text	*text;
extern	int	ntextlist;
extern	int	ntext;
extern	int	ntext1;
extern	double	curx, cury;
extern	int	hvmode;
extern	int	codegen;
extern	char	*PEstring;
char	*tostring(char *);
char	*grow(char *, char *, int, int);
double	getfval(char *), getcomp(obj *, int), getblkvar(obj *, char *);
YYSTYPE	getvar(char *);
struct	symtab *lookup(char *), *makevar(char *, int, YYSTYPE);
char	*ifstat(double, char *, char *), *delimstr(char *), *sprintgen(char *);
void	forloop(char *var, double from, double to, int op, double by, char *_str);
int	setdir(int), curdir(void);
void	resetvar(void);
void	checkscale(char *);
void	pushsrc(int, char *);
void	copy(void);
void	copyuntil(char *);
void	copyfile(char *);
void	copydef(struct symtab *);
void	definition(char *);
struct symtab *copythru(char *);
int	input(void);
int	unput(int);
void	extreme(double, double);
extern	double	deltx, delty;
extern	int	lineno;
extern	int	synerr;
extern	double	xmin, ymin, xmax, ymax;
obj	*leftthing(int), *boxgen(void), *circgen(int), *arcgen(int);
obj	*linegen(int), *splinegen(void), *movegen(void);
obj	*textgen(void), *plotgen(void);
obj	*troffgen(char *), *rightthing(obj *, int), *blockgen(obj *, obj *);
obj	*makenode(int, int), *makepos(double, double);
obj	*fixpos(obj *, double, double);
obj	*addpos(obj *, obj *), *subpos(obj *, obj *);
obj	*makebetween(double, obj *, obj *);
obj	*getpos(obj *, int), *gethere(void), *getfirst(int, int);
obj	*getlast(int, int), *getblock(obj *, char *);
void	savetext(int, char *);
void	makeiattr(int, int);
void	makevattr(char *);
void	makefattr(int type, int sub, double f);
void	maketattr(int, char *);
void	makeoattr(int, obj *);
void	makeattr(int type, int sub, YYSTYPE val);
void	printexpr(double);
void	printpos(obj *);
void	exprsave(double);
void	addtattr(int);
void	printlf(int, char *);
struct pushstack {
double	p_x;
double	p_y;
int	p_hvmode;
double	p_xmin;
double	p_ymin;
double	p_xmax;
double	p_ymax;
struct symtab *p_symtab;
};
extern	struct pushstack stack[];
extern	int	nstack;
extern	int	cw;
extern	double	errcheck(double, char *);
#define	Log10(x) errcheck(log10(x), "log")
#define	Exp(x)	errcheck(exp(x), "exp")
#define	Sqrt(x)	errcheck(sqrt(x), "sqrt")