#ifndef SEEN_SPIN_H
#define SEEN_SPIN_H
#include <stdio.h>
#include <string.h>
#include <ctype.h>
enum	    { INIV, PUTV, LOGV };
enum btypes { NONE, N_CLAIM, I_PROC, A_PROC, P_PROC, E_TRACE, N_TRACE };
typedef struct Lextok {
unsigned short	ntyp;
short	ismtyp;
int	val;
int	ln;
int	indstep;
int	uiid;
struct Symbol	*fn;
struct Symbol	*sym;
struct Sequence *sq;
struct SeqList	*sl;
struct Lextok	*lft, *rgt;
} Lextok;
typedef struct Slicer {
Lextok	*n;
short	code;
short	used;
struct Slicer *nxt;
} Slicer;
typedef struct Access {
struct Symbol	*who;
struct Symbol	*what;
int	cnt, typ;
struct Access	*lnk;
} Access;
typedef struct Symbol {
char	*name;
int	Nid;
unsigned short	type;
unsigned char	hidden;
unsigned char	colnr;
unsigned char	isarray;
unsigned char	*bscp;
int	nbits;
int	nel;
int	setat;
int	*val;
Lextok	**Sval;
int	xu;
struct Symbol	*xup[2];
struct Access	*access;
Lextok	*ini;
Lextok	*Slst;
struct Symbol	*Snm;
struct Symbol	*owner;
struct Symbol	*context;
struct Symbol	*next;
} Symbol;
typedef struct Ordered {
struct Symbol	*entry;
struct Ordered	*next;
} Ordered;
typedef struct Queue {
short	qid;
int	qlen;
int	nslots, nflds;
int	setat;
int	*fld_width;
int	*contents;
int	*stepnr;
struct Queue	*nxt;
} Queue;
typedef struct FSM_state {
int from;
int seen;
int in;
int cr;
int scratch;
unsigned long *dom, *mod;
struct FSM_trans *t;
struct FSM_trans *p;
struct FSM_state *nxt;
} FSM_state;
typedef struct FSM_trans {
int to;
short	relevant;
short	round;
struct FSM_use *Val[2];
struct Element *step;
struct FSM_trans *nxt;
} FSM_trans;
typedef struct FSM_use {
Lextok *n;
Symbol *var;
int special;
struct FSM_use *nxt;
} FSM_use;
typedef struct Element {
Lextok	*n;
int	Seqno;
int	seqno;
int	merge;
int	merge_start;
int	merge_single;
short	merge_in;
short	merge_mark;
unsigned int	status;
struct FSM_use	*dead;
struct SeqList	*sub;
struct SeqList	*esc;
struct Element	*Nxt;
struct Element	*nxt;
} Element;
typedef struct Sequence {
Element	*frst;
Element	*last;
Element *extent;
int	maxel;
} Sequence;
typedef struct SeqList {
Sequence	*this;
struct SeqList	*nxt;
} SeqList;
typedef struct Label {
Symbol	*s;
Symbol	*c;
Element	*e;
int	uiid;
int	visible;
struct Label	*nxt;
} Label;
typedef struct Lbreak {
Symbol	*l;
struct Lbreak	*nxt;
} Lbreak;
typedef struct RunList {
Symbol	*n;
int	tn;
int	pid;
int	priority;
enum btypes b;
Element	*pc;
Sequence *ps;
Lextok	*prov;
Symbol	*symtab;
struct RunList	*nxt;
} RunList;
typedef struct ProcList {
Symbol	*n;
Lextok	*p;
Sequence *s;
Lextok	*prov;
enum btypes b;
short	tn;
unsigned char	det;
unsigned char   unsafe;
struct ProcList	*nxt;
} ProcList;
typedef	Lextok *Lexptr;
#define YYSTYPE	Lexptr
#define ZN	(Lextok *)0
#define ZS	(Symbol *)0
#define ZE	(Element *)0
#define DONE	  1
#define ATOM	  2
#define L_ATOM	  4
#define I_GLOB    8
#define DONE2	 16
#define D_ATOM	 32
#define ENDSTATE 64
#define CHECK2	128
#define CHECK3	256
#define Nhash	255
#define XR	  	1
#define XS	  	2
#define XX	  	4
#define CODE_FRAG	2
#define CODE_DECL	4
#define PREDEF	  	3
#define UNSIGNED  5
#define BIT	  1
#define BYTE	  8
#define SHORT	 16
#define INT	 32
#define	CHAN	 64
#define STRUCT	128
#define SOMETHINGBIG	65536
#define RATHERSMALL	512
#define MAXSCOPESZ	1024
#ifndef max
#define max(a,b) (((a)<(b)) ? (b) : (a))
#endif
#ifdef PC
#define MFLAGS	"wb"
#else
#define MFLAGS	"w"
#endif
Element	*eval_sub(Element *);
Element	*get_lab(Lextok *, int);
Element	*huntele(Element *, int, int);
Element	*huntstart(Element *);
Element	*target(Element *);
Lextok	*do_unless(Lextok *, Lextok *);
Lextok	*expand(Lextok *, int);
Lextok	*getuname(Symbol *);
Lextok	*mk_explicit(Lextok *, int, int);
Lextok	*nn(Lextok *, int, Lextok *, Lextok *);
Lextok	*rem_lab(Symbol *, Lextok *, Symbol *);
Lextok	*rem_var(Symbol *, Lextok *, Symbol *, Lextok *);
Lextok	*tail_add(Lextok *, Lextok *);
ProcList *ready(Symbol *, Lextok *, Sequence *, int, Lextok *, enum btypes);
SeqList	*seqlist(Sequence *, SeqList *);
Sequence *close_seq(int);
Symbol	*break_dest(void);
Symbol	*findloc(Symbol *);
Symbol	*has_lab(Element *, int);
Symbol	*lookup(char *);
Symbol	*prep_inline(Symbol *, Lextok *);
char	*emalloc(size_t);
long	Rand(void);
int	any_oper(Lextok *, int);
int	any_undo(Lextok *);
int	c_add_sv(FILE *);
int	cast_val(int, int, int);
int	checkvar(Symbol *, int);
int	Cnt_flds(Lextok *);
int	cnt_mpars(Lextok *);
int	complete_rendez(void);
int	enable(Lextok *);
int	Enabled0(Element *);
int	eval(Lextok *);
int	find_lab(Symbol *, Symbol *, int);
int	find_maxel(Symbol *);
int	full_name(FILE *, Lextok *, Symbol *, int);
int	getlocal(Lextok *);
int	getval(Lextok *);
int	glob_inline(char *);
int	has_typ(Lextok *, int);
int	in_bound(Symbol *, int);
int	interprint(FILE *, Lextok *);
int	printm(FILE *, Lextok *);
int	is_inline(void);
int	ismtype(char *);
int	isproctype(char *);
int	isutype(char *);
int	Lval_struct(Lextok *, Symbol *, int, int);
int	main(int, char **);
int	pc_value(Lextok *);
int	pid_is_claim(int);
int	proper_enabler(Lextok *);
int	putcode(FILE *, Sequence *, Element *, int, int, int);
int	q_is_sync(Lextok *);
int	qlen(Lextok *);
int	qfull(Lextok *);
int	qmake(Symbol *);
int	qrecv(Lextok *, int);
int	qsend(Lextok *);
int	remotelab(Lextok *);
int	remotevar(Lextok *);
int	Rval_struct(Lextok *, Symbol *, int);
int	setlocal(Lextok *, int);
int	setval(Lextok *, int);
int	sputtype(char *, int);
int	Sym_typ(Lextok *);
int	tl_main(int, char *[]);
int	Width_set(int *, int, Lextok *);
int	yyparse(void);
int	yywrap(void);
int	yylex(void);
void	AST_track(Lextok *, int);
void	add_seq(Lextok *);
void	alldone(int);
void	announce(char *);
void	c_state(Symbol *, Symbol *, Symbol *);
void	c_add_def(FILE *);
void	c_add_loc(FILE *, char *);
void	c_add_locinit(FILE *, int, char *);
void	c_add_use(FILE *);
void	c_chandump(FILE *);
void	c_preview(void);
void	c_struct(FILE *, char *, Symbol *);
void	c_track(Symbol *, Symbol *, Symbol *);
void	c_var(FILE *, char *, Symbol *);
void	c_wrapper(FILE *);
void	chanaccess(void);
void	check_param_count(int, Lextok *);
void	checkrun(Symbol *, int);
void	comment(FILE *, Lextok *, int);
void	cross_dsteps(Lextok *, Lextok *);
void	disambiguate(void);
void	doq(Symbol *, int, RunList *);
void	dotag(FILE *, char *);
void	do_locinits(FILE *);
void	do_var(FILE *, int, char *, Symbol *, char *, char *, char *);
void	dump_struct(Symbol *, char *, RunList *);
void	dumpclaims(FILE *, int, char *);
void	dumpglobals(void);
void	dumplabels(void);
void	dumplocal(RunList *);
void	dumpsrc(int, int);
void	fatal(char *, char *);
void	fix_dest(Symbol *, Symbol *);
void	genaddproc(void);
void	genaddqueue(void);
void	gencodetable(FILE *);
void	genheader(void);
void	genother(void);
void	gensrc(void);
void	gensvmap(void);
void	genunio(void);
void	ini_struct(Symbol *);
void	loose_ends(void);
void	make_atomic(Sequence *, int);
void	match_trail(void);
void	no_side_effects(char *);
void	nochan_manip(Lextok *, Lextok *, int);
void	non_fatal(char *, char *);
void	ntimes(FILE *, int, int, char *c[]);
void	open_seq(int);
void	p_talk(Element *, int);
void	pickup_inline(Symbol *, Lextok *);
void	plunk_c_decls(FILE *);
void	plunk_c_fcts(FILE *);
void	plunk_expr(FILE *, char *);
void	plunk_inline(FILE *, char *, int, int);
void	prehint(Symbol *);
void	preruse(FILE *, Lextok *);
void	prune_opts(Lextok *);
void	pstext(int, char *);
void	pushbreak(void);
void	putname(FILE *, char *, Lextok *, int, char *);
void	putremote(FILE *, Lextok *, int);
void	putskip(int);
void	putsrc(Element *);
void	putstmnt(FILE *, Lextok *, int);
void	putunames(FILE *);
void	rem_Seq(void);
void	runnable(ProcList *, int, int);
void	sched(void);
void	setaccess(Symbol *, Symbol *, int, int);
void	set_lab(Symbol *, Element *);
void	setmtype(Lextok *);
void	setpname(Lextok *);
void	setptype(Lextok *, int, Lextok *);
void	setuname(Lextok *);
void	setutype(Lextok *, Symbol *, Lextok *);
void	setxus(Lextok *, int);
void	show_lab(void);
void	Srand(unsigned);
void	start_claim(int);
void	struct_name(Lextok *, Symbol *, int, char *);
void	symdump(void);
void	symvar(Symbol *);
void	sync_product(void);
void	trackchanuse(Lextok *, Lextok *, int);
void	trackvar(Lextok *, Lextok *);
void	trackrun(Lextok *);
void	trapwonly(Lextok * );
void	typ2c(Symbol *);
void	typ_ck(int, int, char *);
void	undostmnt(Lextok *, int);
void	unrem_Seq(void);
void	unskip(int);
void	varcheck(Element *, Element *);
void	whoruns(int);
void	wrapup(int);
void	yyerror(char *, ...);
#endif