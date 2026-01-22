#include "spin.h"
#include "y.tab.h"
extern Ordered *all_names;
extern FSM_use *use_free;
extern FSM_state **fsm_tbl;
extern FSM_state *fsm;
extern int verbose, o_max;
static FSM_trans *cur_t;
static FSM_trans *expl_par;
static FSM_trans *expl_var;
static FSM_trans *explicit;
extern void rel_use(FSM_use *);
#define ulong unsigned long
typedef struct Pair {
FSM_state *h;
int b;
struct Pair *nxt;
} Pair;
typedef struct AST {
ProcList *p;
int i_st;
int nstates, nwords;
int relevant;
Pair *pairs;
FSM_state *fsm;
struct AST *nxt;
} AST;
typedef struct RPN {
Symbol *rn;
struct RPN *nxt;
} RPN;
typedef struct ALIAS {
Lextok *cnm;
int origin;
struct ALIAS *alias;
struct ALIAS *nxt;
} ALIAS;
typedef struct ChanList {
Lextok *s;
Lextok *n;
struct ChanList *nxt;
} ChanList;
#define USE 1
#define DEF 2
#define DEREF_DEF 4
#define DEREF_USE 8
static AST *ast;
static ALIAS *chalcur;
static ALIAS *chalias;
static ChanList *chanlist;
static Slicer *slicer;
static Slicer *rel_vars;
static int AST_Changes;
static int AST_Round;
static RPN *rpn;
static int in_recv = 0;
static int AST_mutual(Lextok *, Lextok *, int);
static void AST_dominant(void);
static void AST_hidden(void);
static void AST_setcur(Lextok *);
static void check_slice(Lextok *, int);
static void curtail(AST *);
static void def_use(Lextok *, int);
static void name_AST_track(Lextok *, int);
static void show_expl(void);
static int
AST_isini(Lextok *n)
{ Symbol *s;
if (!n || !n->sym) return 0;
s = n->sym;
if (s->type == CHAN)
return (s->ini->ntyp == CHAN);
if (s->type == STRUCT && n->rgt)
return AST_isini(n->rgt->lft);
return 0;
}
static void
AST_var(Lextok *n, Symbol *s, int toplevel)
{
if (!s) return;
if (toplevel)
{ if (s->context && s->type)
printf(":%s:L:", s->context->name);
else
printf("G:");
}
printf("%s", s->name);
if (s->type == STRUCT && n && n->rgt && n->rgt->lft)
{ printf(":");
AST_var(n->rgt->lft, n->rgt->lft->sym, 0);
}
}
static void
name_def_indices(Lextok *n, int code)
{
if (!n || !n->sym) return;
if (n->sym->nel > 1 || n->sym->isarray)
def_use(n->lft, code);
if (n->sym->type == STRUCT
&& n->rgt)
name_def_indices(n->rgt->lft, code);
}
static void
name_def_use(Lextok *n, int code)
{ FSM_use *u;
if (!n) return;
if ((code&USE)
&& cur_t->step
&& cur_t->step->n)
{ switch (cur_t->step->n->ntyp) {
case 'c':
n->sym->colnr |= 2;
break;
default:
n->sym->colnr |= 1;
break;
}
}
for (u = cur_t->Val[0]; u; u = u->nxt)
if (AST_mutual(n, u->n, 1)
&& u->special == code)
return;
if (use_free)
{ u = use_free;
use_free = use_free->nxt;
} else
u = (FSM_use *) emalloc(sizeof(FSM_use));
u->n = n;
u->special = code;
u->nxt = cur_t->Val[0];
cur_t->Val[0] = u;
name_def_indices(n, USE|(code&(~DEF)));
}
static void
def_use(Lextok *now, int code)
{ Lextok *v;
if (now)
switch (now->ntyp) {
case '!':
case UMIN:
case '~':
case 'c':
case ENABLED:
case ASSERT:
case EVAL:
def_use(now->lft, USE|code);
break;
case LEN:
case FULL:
case EMPTY:
case NFULL:
case NEMPTY:
def_use(now->lft, DEREF_USE|USE|code);
break;
case '/':
case '*':
case '-':
case '+':
case '%':
case '&':
case '^':
case '|':
case LE:
case GE:
case GT:
case LT:
case NE:
case EQ:
case OR:
case AND:
case LSHIFT:
case RSHIFT:
def_use(now->lft, USE|code);
def_use(now->rgt, USE|code);
break;
case ASGN:
def_use(now->lft, DEF|code);
def_use(now->rgt, USE|code);
break;
case TYPE:
name_def_use(now, code);
break;
case NAME:
name_def_use(now, code);
break;
case RUN:
name_def_use(now, USE);
for (v = now->lft; v; v = v->rgt)
def_use(v->lft, USE);
break;
case 's':
def_use(now->lft, DEREF_DEF|DEREF_USE|USE|code);
for (v = now->rgt; v; v = v->rgt)
def_use(v->lft, USE|code);
break;
case 'r':
def_use(now->lft, DEREF_DEF|DEREF_USE|USE|code);
for (v = now->rgt; v; v = v->rgt)
{ if (v->lft->ntyp == EVAL)
def_use(v->lft, code);
else if (v->lft->ntyp != CONST)
def_use(v->lft, DEF|code);
}
break;
case 'R':
def_use(now->lft, DEREF_USE|USE|code);
for (v = now->rgt; v; v = v->rgt)
{ if (v->lft->ntyp == EVAL)
def_use(v->lft, code);
}
break;
case '?':
def_use(now->lft, USE|code);
if (now->rgt)
{ def_use(now->rgt->lft, code);
def_use(now->rgt->rgt, code);
}
break;
case PRINT:
for (v = now->lft; v; v = v->rgt)
def_use(v->lft, USE|code);
break;
case PRINTM:
def_use(now->lft, USE);
break;
case CONST:
case ELSE:
case NONPROGRESS:
case PC_VAL:
case 'p':
case 'q':
break;
case '.':
case GOTO:
case BREAK:
case '@':
case D_STEP:
case ATOMIC:
case NON_ATOMIC:
case IF:
case DO:
case UNLESS:
case TIMEOUT:
case C_CODE:
case C_EXPR:
default:
break;
}
}
static int
AST_add_alias(Lextok *n, int nr)
{ ALIAS *ca;
int res;
for (ca = chalcur->alias; ca; ca = ca->nxt)
if (AST_mutual(ca->cnm, n, 1))
{ res = (ca->origin&nr);
ca->origin |= nr;
return (res == 0);
}
ca = (ALIAS *) emalloc(sizeof(ALIAS));
ca->cnm = n;
ca->origin = nr;
ca->nxt = chalcur->alias;
chalcur->alias = ca;
return 1;
}
static void
AST_run_alias(char *pn, char *s, Lextok *t, int parno)
{ Lextok *v;
int cnt;
if (!t) return;
if (t->ntyp == RUN)
{ if (strcmp(t->sym->name, s) == 0)
for (v = t->lft, cnt = 1; v; v = v->rgt, cnt++)
if (cnt == parno)
{ AST_add_alias(v->lft, 1);
break;
}
} else
{ AST_run_alias(pn, s, t->lft, parno);
AST_run_alias(pn, s, t->rgt, parno);
}
}
static void
AST_findrun(char *s, int parno)
{ FSM_state *f;
FSM_trans *t;
AST *a;
for (a = ast; a; a = a->nxt)
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
{ if (t->step)
AST_run_alias(a->p->n->name, s, t->step->n, parno);
}
}
static void
AST_par_chans(ProcList *p)
{ Ordered *walk;
Symbol *sp;
for (walk = all_names; walk; walk = walk->next)
{ sp = walk->entry;
if (sp
&& sp->context
&& strcmp(sp->context->name, p->n->name) == 0
&& sp->Nid >= 0
&& sp->type == CHAN
&& sp->ini->ntyp == NAME)
{ Lextok *x = nn(ZN, 0, ZN, ZN);
x->sym = sp;
AST_setcur(x);
AST_add_alias(sp->ini, 2);
} }
}
static void
AST_para(ProcList *p)
{ Lextok *f, *t, *c;
int cnt = 0;
AST_par_chans(p);
for (f = p->p; f; f = f->rgt)
for (t = f->lft; t; t = t->rgt)
{ if (t->ntyp != ',')
c = t;
else
c = t->lft;
cnt++;
if (Sym_typ(c) == CHAN)
{ ALIAS *na = (ALIAS *) emalloc(sizeof(ALIAS));
na->cnm = c;
na->nxt = chalias;
chalcur = chalias = na;
#if 0
printf("%s -- (par) -- ", p->n->name);
AST_var(c, c->sym, 1);
printf(" => <<");
#endif
AST_findrun(p->n->name, cnt);
#if 0
printf(">>\n");
#endif
}
}
}
static void
AST_haschan(Lextok *c)
{
if (!c) return;
if (Sym_typ(c) == CHAN)
{ AST_add_alias(c, 2);
#if 0
printf("<<");
AST_var(c, c->sym, 1);
printf(">>\n");
#endif
} else
{ AST_haschan(c->rgt);
AST_haschan(c->lft);
}
}
static int
AST_nrpar(Lextok *n)
{ Lextok *m;
int j = 0;
for (m = n->rgt; m; m = m->rgt)
j++;
return j;
}
static int
AST_ord(Lextok *n, Lextok *s)
{ Lextok *m;
int j = 0;
for (m = n->rgt; m; m = m->rgt)
{ j++;
if (s->sym == m->lft->sym)
return j;
}
return 0;
}
#if 0
static void
AST_ownership(Symbol *s)
{
if (!s) return;
printf("%s:", s->name);
AST_ownership(s->owner);
}
#endif
static int
AST_mutual(Lextok *a, Lextok *b, int toplevel)
{ Symbol *as, *bs;
if (!a && !b) return 1;
if (!a || !b) return 0;
as = a->sym;
bs = b->sym;
if (!as || !bs) return 0;
if (toplevel && as->context != bs->context)
return 0;
if (as->type != bs->type)
return 0;
if (strcmp(as->name, bs->name) != 0)
return 0;
if (as->type == STRUCT && a->rgt && b->rgt)
return AST_mutual(a->rgt->lft, b->rgt->lft, 0);
return 1;
}
static void
AST_setcur(Lextok *n)
{ ALIAS *ca;
for (ca = chalias; ca; ca = ca->nxt)
if (AST_mutual(ca->cnm, n, 1))
{ chalcur = ca;
return;
}
ca = (ALIAS *) emalloc(sizeof(ALIAS));
ca->cnm = n;
ca->nxt = chalias;
chalcur = chalias = ca;
}
static void
AST_other(AST *a)
{ FSM_state *f;
FSM_trans *t;
FSM_use *u;
ChanList *cl;
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
for (u = t->Val[0]; u; u = u->nxt)
if (Sym_typ(u->n) == CHAN
&& (u->special&DEF))
{ AST_setcur(u->n);
switch (t->step->n->ntyp) {
case ASGN:
AST_haschan(t->step->n->rgt);
break;
case 'r':
for (cl = chanlist; cl; cl = cl->nxt)
{ int aa = AST_nrpar(cl->s);
int bb = AST_nrpar(t->step->n);
if (aa != bb)
continue;
aa = AST_ord(cl->s, cl->n);
bb = AST_ord(t->step->n, u->n);
if (aa != bb)
continue;
AST_add_alias(cl->n, 4);
}
break;
default:
printf("type = %d\n", t->step->n->ntyp);
non_fatal("unexpected chan def type", (char *) 0);
break;
} }
}
static void
AST_aliases(void)
{ ALIAS *na, *ca;
for (na = chalias; na; na = na->nxt)
{ printf("\npossible aliases of ");
AST_var(na->cnm, na->cnm->sym, 1);
printf("\n\t");
for (ca = na->alias; ca; ca = ca->nxt)
{ if (!ca->cnm->sym)
printf("no valid name ");
else
AST_var(ca->cnm, ca->cnm->sym, 1);
printf("<");
if (ca->origin & 1) printf("RUN ");
if (ca->origin & 2) printf("ASGN ");
if (ca->origin & 4) printf("RCV ");
printf("[%s]", AST_isini(ca->cnm)?"Initzd":"Name");
printf(">");
if (ca->nxt) printf(", ");
}
printf("\n");
}
printf("\n");
}
static void
AST_indirect(FSM_use *uin, FSM_trans *t, char *cause, char *pn)
{ FSM_use *u;
if (!(t->relevant&1)) AST_Changes++;
t->round = AST_Round;
t->relevant = 1;
if ((verbose&32) && t->step)
{ printf("\tDR %s [[ ", pn);
comment(stdout, t->step->n, 0);
printf("]]\n\t\tfully relevant %s", cause);
if (uin) { printf(" due to "); AST_var(uin->n, uin->n->sym, 1); }
printf("\n");
}
for (u = t->Val[0]; u; u = u->nxt)
if (u != uin
&& (u->special&(USE|DEREF_USE)))
{ if (verbose&32)
{ printf("\t\t\tuses(%d): ", u->special);
AST_var(u->n, u->n->sym, 1);
printf("\n");
}
name_AST_track(u->n, u->special);
}
}
static void
def_relevant(char *pn, FSM_trans *t, Lextok *n, int ischan)
{ FSM_use *u;
ALIAS *na, *ca;
int chanref;
if (n->ntyp != ELSE)
for (u = t->Val[0]; u; u = u->nxt)
{ chanref = (Sym_typ(u->n) == CHAN);
if (ischan != chanref
|| !(u->special&(DEF|DEREF_DEF)))
continue;
if (AST_mutual(u->n, n, 1))
{ AST_indirect(u, t, "(exact match)", pn);
continue;
}
if (chanref)
for (na = chalias; na; na = na->nxt)
{ if (!AST_mutual(u->n, na->cnm, 1))
continue;
for (ca = na->alias; ca; ca = ca->nxt)
if (AST_mutual(ca->cnm, n, 1)
&& AST_isini(ca->cnm))
{ AST_indirect(u, t, "(alias match)", pn);
break;
}
if (ca) break;
} }
}
static void
AST_relevant(Lextok *n)
{ AST *a;
FSM_state *f;
FSM_trans *t;
int ischan;
if (!n) return;
ischan = (Sym_typ(n) == CHAN);
if (verbose&32)
{ printf("<<ast_relevant (ntyp=%d) ", n->ntyp);
AST_var(n, n->sym, 1);
printf(">>\n");
}
for (t = expl_par; t; t = t->nxt)
{ if (!(t->relevant&1))
def_relevant(":params:", t, n, ischan);
}
for (t = expl_var; t; t = t->nxt)
{ if (!(t->relevant&1))
def_relevant(":vars:", t, n, ischan);
}
for (a = ast; a; a = a->nxt)
{ if (a->p->b != N_CLAIM && a->p->b != E_TRACE && a->p->b != N_TRACE)
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
{ if (!(t->relevant&1))
def_relevant(a->p->n->name, t, n, ischan);
} }
}
static int
AST_relpar(char *s)
{ FSM_trans *t, *T;
FSM_use *u;
for (T = expl_par; T; T = (T == expl_par)?expl_var: (FSM_trans *) 0)
for (t = T; t; t = t->nxt)
{ if (t->relevant&1)
for (u = t->Val[0]; u; u = u->nxt)
{ if (u->n->sym->type
&& u->n->sym->context
&& strcmp(u->n->sym->context->name, s) == 0)
{
if (verbose&32)
{ printf("proctype %s relevant, due to symbol ", s);
AST_var(u->n, u->n->sym, 1);
printf("\n");
}
return 1;
} } }
return 0;
}
static void
AST_dorelevant(void)
{ AST *a;
RPN *r;
for (r = rpn; r; r = r->nxt)
{ for (a = ast; a; a = a->nxt)
if (strcmp(a->p->n->name, r->rn->name) == 0)
{ a->relevant |= 1;
break;
}
if (!a)
fatal("cannot find proctype %s", r->rn->name);
}
}
static void
AST_procisrelevant(Symbol *s)
{ RPN *r;
for (r = rpn; r; r = r->nxt)
if (strcmp(r->rn->name, s->name) == 0)
return;
r = (RPN *) emalloc(sizeof(RPN));
r->rn = s;
r->nxt = rpn;
rpn = r;
}
static int
AST_proc_isrel(char *s)
{ AST *a;
for (a = ast; a; a = a->nxt)
if (strcmp(a->p->n->name, s) == 0)
return (a->relevant&1);
non_fatal("cannot happen, missing proc in ast", (char *) 0);
return 0;
}
static int
AST_scoutrun(Lextok *t)
{
if (!t) return 0;
if (t->ntyp == RUN)
return AST_proc_isrel(t->sym->name);
return (AST_scoutrun(t->lft) || AST_scoutrun(t->rgt));
}
static void
AST_tagruns(void)
{ AST *a;
FSM_state *f;
FSM_trans *t;
for (a = ast; a; a = a->nxt)
{ if (a->p->b == N_CLAIM || a->p->b == I_PROC
|| a->p->b == E_TRACE || a->p->b == N_TRACE)
{ a->relevant |= 1;
continue;
}
if (AST_relpar(a->p->n->name))
a->relevant |= 1;
else
{ for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
if (t->relevant)
goto yes;
yes: if (f)
a->relevant |= 1;
}
}
for (a = ast; a; a = a->nxt)
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
if (t->step
&& AST_scoutrun(t->step->n))
{ AST_indirect((FSM_use *)0, t, ":run:", a->p->n->name);
}
}
static void
AST_report(AST *a, Element *e)
{
if (!(a->relevant&2))
{ a->relevant |= 2;
printf("spin: redundant in proctype %s (for given property):\n",
a->p->n->name);
}
printf("      %s:%d (state %d)",
e->n?e->n->fn->name:"-",
e->n?e->n->ln:-1,
e->seqno);
printf("	[");
comment(stdout, e->n, 0);
printf("]\n");
}
static int
AST_always(Lextok *n)
{
if (!n) return 0;
if (n->ntyp == '@'
|| n->ntyp == 'p')
return 1;
return AST_always(n->lft) || AST_always(n->rgt);
}
static void
AST_edge_dump(AST *a, FSM_state *f)
{ FSM_trans *t;
FSM_use *u;
for (t = f->t; t; t = t->nxt)
{
if (t->step && AST_always(t->step->n))
t->relevant |= 1;
if (verbose&32)
{ switch (t->relevant) {
case 0: printf("     "); break;
case 1: printf("*%3d ", t->round); break;
case 2: printf("+%3d ", t->round); break;
case 3: printf("#%3d ", t->round); break;
default: printf("? "); break;
}
printf("%d\t->\t%d\t", f->from, t->to);
if (t->step)
comment(stdout, t->step->n, 0);
else
printf("Unless");
for (u = t->Val[0]; u; u = u->nxt)
{ printf(" <");
AST_var(u->n, u->n->sym, 1);
printf(":%d>", u->special);
}
printf("\n");
} else
{ if (t->relevant)
continue;
if (t->step)
switch(t->step->n->ntyp) {
case ASGN:
case 's':
case 'r':
case 'c':
if (t->step->n->lft->ntyp != CONST)
AST_report(a, t->step);
break;
case PRINT:
case PRINTM:
case ASSERT:
case C_CODE:
case C_EXPR:
default:
break;
} } }
}
static void
AST_dfs(AST *a, int s, int vis)
{ FSM_state *f;
FSM_trans *t;
f = fsm_tbl[s];
if (f->seen) return;
f->seen = 1;
if (vis) AST_edge_dump(a, f);
for (t = f->t; t; t = t->nxt)
AST_dfs(a, t->to, vis);
}
static void
AST_dump(AST *a)
{ FSM_state *f;
for (f = a->fsm; f; f = f->nxt)
{ f->seen = 0;
fsm_tbl[f->from] = f;
}
if (verbose&32)
printf("AST_START %s from %d\n", a->p->n->name, a->i_st);
AST_dfs(a, a->i_st, 1);
}
static void
AST_sends(AST *a)
{ FSM_state *f;
FSM_trans *t;
FSM_use *u;
ChanList *cl;
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
{ if (t->step
&& t->step->n
&& t->step->n->ntyp == 's')
for (u = t->Val[0]; u; u = u->nxt)
{ if (Sym_typ(u->n) == CHAN
&& ((u->special&USE) && !(u->special&DEREF_USE)))
{
#if 0
printf("%s -- (%d->%d) -- ",
a->p->n->name, f->from, t->to);
AST_var(u->n, u->n->sym, 1);
printf(" -> chanlist\n");
#endif
cl = (ChanList *) emalloc(sizeof(ChanList));
cl->s = t->step->n;
cl->n = u->n;
cl->nxt = chanlist;
chanlist = cl;
} } } }
static ALIAS *
AST_alfind(Lextok *n)
{ ALIAS *na;
for (na = chalias; na; na = na->nxt)
if (AST_mutual(na->cnm, n, 1))
return na;
return (ALIAS *) 0;
}
static void
AST_trans(void)
{ ALIAS *na, *ca, *da, *ea;
int nchanges;
do {
nchanges = 0;
for (na = chalias; na; na = na->nxt)
{ chalcur = na;
for (ca = na->alias; ca; ca = ca->nxt)
{ da = AST_alfind(ca->cnm);
if (da)
for (ea = da->alias; ea; ea = ea->nxt)
{ nchanges += AST_add_alias(ea->cnm,
ea->origin|ca->origin);
} } }
} while (nchanges > 0);
chalcur = (ALIAS *) 0;
}
static void
AST_def_use(AST *a)
{ FSM_state *f;
FSM_trans *t;
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
{ cur_t = t;
rel_use(t->Val[0]);
rel_use(t->Val[1]);
t->Val[0] = t->Val[1] = (FSM_use *) 0;
if (!t->step) continue;
def_use(t->step->n, 0);
}
cur_t = (FSM_trans *) 0;
}
static void
name_AST_track(Lextok *n, int code)
{ extern int nr_errs;
#if 0
printf("AST_name: ");
AST_var(n, n->sym, 1);
printf(" -- %d\n", code);
#endif
if (in_recv && (code&DEF) && (code&USE))
{ printf("spin: error: DEF and USE of same var in rcv stmnt: ");
AST_var(n, n->sym, 1);
printf(" -- %d\n", code);
nr_errs++;
}
check_slice(n, code);
}
void
AST_track(Lextok *now, int code)
{ Lextok *v; extern int export_ast;
if (!export_ast) return;
if (now)
switch (now->ntyp) {
case LEN:
case FULL:
case EMPTY:
case NFULL:
case NEMPTY:
AST_track(now->lft, DEREF_USE|USE|code);
break;
case '/':
case '*':
case '-':
case '+':
case '%':
case '&':
case '^':
case '|':
case LE:
case GE:
case GT:
case LT:
case NE:
case EQ:
case OR:
case AND:
case LSHIFT:
case RSHIFT:
AST_track(now->rgt, USE|code);
case '!':
case UMIN:
case '~':
case 'c':
case ENABLED:
case ASSERT:
AST_track(now->lft, USE|code);
break;
case EVAL:
AST_track(now->lft, USE|(code&(~DEF)));
break;
case NAME:
name_AST_track(now, code);
if (now->sym->nel > 1 || now->sym->isarray)
AST_track(now->lft, USE|code);
break;
case 'R':
AST_track(now->lft, DEREF_USE|USE|code);
for (v = now->rgt; v; v = v->rgt)
AST_track(v->lft, code);
break;
case '?':
AST_track(now->lft, USE|code);
if (now->rgt)
{ AST_track(now->rgt->lft, code);
AST_track(now->rgt->rgt, code);
}
break;
case TYPE:
name_AST_track(now, code);
break;
case ASGN:
AST_track(now->lft, DEF|code);
AST_track(now->rgt, USE|code);
break;
case RUN:
name_AST_track(now, USE);
for (v = now->lft; v; v = v->rgt)
AST_track(v->lft, USE|code);
break;
case 's':
AST_track(now->lft, DEREF_DEF|DEREF_USE|USE|code);
for (v = now->rgt; v; v = v->rgt)
AST_track(v->lft, USE|code);
break;
case 'r':
AST_track(now->lft, DEREF_DEF|DEREF_USE|USE|code);
for (v = now->rgt; v; v = v->rgt)
{ in_recv++;
AST_track(v->lft, DEF|code);
in_recv--;
}
break;
case PRINT:
for (v = now->lft; v; v = v->rgt)
AST_track(v->lft, USE|code);
break;
case PRINTM:
AST_track(now->lft, USE);
break;
case 'p':
#if 0
'p' -sym-> _p
/
'?' -sym-> a (proctype)
/
b (pid expr)
#endif
AST_track(now->lft->lft, USE|code);
AST_procisrelevant(now->lft->sym);
break;
case CONST:
case ELSE:
case NONPROGRESS:
case PC_VAL:
case 'q':
break;
case '.':
case GOTO:
case BREAK:
case '@':
case D_STEP:
case ATOMIC:
case NON_ATOMIC:
case IF:
case DO:
case UNLESS:
case TIMEOUT:
case C_CODE:
case C_EXPR:
break;
default:
printf("AST_track, NOT EXPECTED ntyp: %d\n", now->ntyp);
break;
}
}
static int
AST_dump_rel(void)
{ Slicer *rv;
Ordered *walk;
char buf[64];
int banner=0;
if (verbose&32)
{ printf("Relevant variables:\n");
for (rv = rel_vars; rv; rv = rv->nxt)
{ printf("\t");
AST_var(rv->n, rv->n->sym, 1);
printf("\n");
}
return 1;
}
for (rv = rel_vars; rv; rv = rv->nxt)
rv->n->sym->setat = 1;
for (walk = all_names; walk; walk = walk->next)
{ Symbol *s;
s = walk->entry;
if (!s->setat
&& (s->type != MTYPE || s->ini->ntyp != CONST)
&& s->type != STRUCT
&& s->type != PROCTYPE
&& !s->owner
&& sputtype(buf, s->type))
{ if (!banner)
{ banner = 1;
printf("spin: redundant vars (for given property):\n");
}
printf("\t");
symvar(s);
} }
return banner;
}
static void
AST_suggestions(void)
{ Symbol *s;
Ordered *walk;
FSM_state *f;
FSM_trans *t;
AST *a;
int banner=0;
int talked=0;
for (walk = all_names; walk; walk = walk->next)
{ s = walk->entry;
if (s->colnr == 2
&& (s->type == BYTE
|| s->type == SHORT
|| s->type == INT
|| s->type == MTYPE))
{ if (!banner)
{ banner = 1;
printf("spin: consider using predicate");
printf(" abstraction to replace:\n");
}
printf("\t");
symvar(s);
} }
for (a = ast; a; a = a->nxt)
{ banner = 0;
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
{ if (t->step)
switch (t->step->n->ntyp) {
case 's':
banner |= 1;
break;
case 'r':
banner |= 2;
break;
case '.':
case D_STEP:
case ATOMIC:
case NON_ATOMIC:
case IF:
case DO:
case UNLESS:
case '@':
case GOTO:
case BREAK:
case PRINT:
case PRINTM:
case ASSERT:
case C_CODE:
case C_EXPR:
break;
default:
banner |= 4;
goto no_good;
}
}
no_good: if (banner == 1 || banner == 2)
{ printf("spin: proctype %s defines a %s process\n",
a->p->n->name,
banner==1?"source":"sink");
talked |= banner;
} else if (banner == 3)
{ printf("spin: proctype %s mimics a buffer\n",
a->p->n->name);
talked |= 4;
}
}
if (talked&1)
{ printf("\tto reduce complexity, consider merging the code of\n");
printf("\teach source process into the code of its target\n");
}
if (talked&2)
{ printf("\tto reduce complexity, consider merging the code of\n");
printf("\teach sink process into the code of its source\n");
}
if (talked&4)
printf("\tto reduce complexity, avoid buffer processes\n");
}
static void
AST_preserve(void)
{ Slicer *sc, *nx, *rv;
for (sc = slicer; sc; sc = nx)
{ if (!sc->used)
break;
nx = sc->nxt;
for (rv = rel_vars; rv; rv = rv->nxt)
if (AST_mutual(sc->n, rv->n, 1))
break;
if (!rv)
{ sc->nxt = rel_vars;
rel_vars = sc;
} }
slicer = sc;
}
static void
check_slice(Lextok *n, int code)
{ Slicer *sc;
for (sc = slicer; sc; sc = sc->nxt)
if (AST_mutual(sc->n, n, 1)
&& sc->code == code)
return;
sc = (Slicer *) emalloc(sizeof(Slicer));
sc->n = n;
sc->code = code;
sc->used = 0;
sc->nxt = slicer;
slicer = sc;
}
static void
AST_data_dep(void)
{ Slicer *sc;
for (sc = slicer; sc; sc = sc->nxt)
{ sc->used = 1;
if (verbose&32)
{ printf("spin: slice criterion ");
AST_var(sc->n, sc->n->sym, 1);
printf(" type=%d\n", Sym_typ(sc->n));
}
AST_relevant(sc->n);
}
AST_tagruns();
}
static int
AST_blockable(AST *a, int s)
{ FSM_state *f;
FSM_trans *t;
f = fsm_tbl[s];
for (t = f->t; t; t = t->nxt)
{ if (t->relevant&2)
return 1;
if (t->step && t->step->n)
switch (t->step->n->ntyp) {
case IF:
case DO:
case ATOMIC:
case NON_ATOMIC:
case D_STEP:
if (AST_blockable(a, t->to))
{ t->round = AST_Round;
t->relevant |= 2;
return 1;
}
default:
break;
}
else if (AST_blockable(a, t->to))
{ t->round = AST_Round;
t->relevant |= 2;
return 1;
}
}
return 0;
}
static void
AST_spread(AST *a, int s)
{ FSM_state *f;
FSM_trans *t;
f = fsm_tbl[s];
for (t = f->t; t; t = t->nxt)
{ if (t->relevant&2)
continue;
if (t->step && t->step->n)
switch (t->step->n->ntyp) {
case IF:
case DO:
case ATOMIC:
case NON_ATOMIC:
case D_STEP:
AST_spread(a, t->to);
default:
t->round = AST_Round;
t->relevant |= 2;
break;
}
else
{ AST_spread(a, t->to);
t->round = AST_Round;
t->relevant |= 2;
}
}
}
static int
AST_notrelevant(Lextok *n)
{ Slicer *s;
for (s = rel_vars; s; s = s->nxt)
if (AST_mutual(s->n, n, 1))
return 0;
for (s = slicer; s; s = s->nxt)
if (AST_mutual(s->n, n, 1))
return 0;
return 1;
}
static int
AST_withchan(Lextok *n)
{
if (!n) return 0;
if (Sym_typ(n) == CHAN)
return 1;
return AST_withchan(n->lft) || AST_withchan(n->rgt);
}
static int
AST_suspect(FSM_trans *t)
{ FSM_use *u;
if (!t || !t->step || !AST_withchan(t->step->n))
return 0;
for (u = t->Val[0]; u; u = u->nxt)
if (AST_notrelevant(u->n))
return 1;
return 0;
}
static void
AST_shouldconsider(AST *a, int s)
{ FSM_state *f;
FSM_trans *t;
f = fsm_tbl[s];
for (t = f->t; t; t = t->nxt)
{ if (t->step && t->step->n)
switch (t->step->n->ntyp) {
case IF:
case DO:
case ATOMIC:
case NON_ATOMIC:
case D_STEP:
AST_shouldconsider(a, t->to);
break;
default:
AST_track(t->step->n, 0);
if (AST_suspect(t))
{ printf("spin: possibly redundant parameters in: ");
comment(stdout, t->step->n, 0);
printf("\n");
}
break;
}
else
AST_shouldconsider(a, t->to);
}
}
static int
FSM_critical(AST *a, int s)
{ FSM_state *f;
FSM_trans *t;
f = fsm_tbl[s];
if (f->seen)
goto done;
f->seen = 1;
f->cr = 0;
for (t = f->t; t; t = t->nxt)
if ((t->relevant&1)
|| FSM_critical(a, t->to))
{ f->cr = 1;
if (verbose&32)
{ printf("\t\t\t\tcritical(%d) ", t->relevant);
comment(stdout, t->step->n, 0);
printf("\n");
}
break;
}
#if 0
else {
if (verbose&32)
{ printf("\t\t\t\tnot-crit ");
comment(stdout, t->step->n, 0);
printf("\n");
}
}
#endif
done:
return f->cr;
}
static void
AST_ctrl(AST *a)
{ FSM_state *f;
FSM_trans *t;
int hit;
if (verbose&32)
printf("CTL -- %s\n", a->p->n->name);
for (f = a->fsm; f; f = f->nxt)
{ if (!(f->scratch&2))
for (t = f->t; t; t = t->nxt)
{ if (t->step && t->step->n)
switch (t->step->n->ntyp) {
case 'r':
case 's':
case 'c':
case ELSE:
t->round = AST_Round;
t->relevant |= 2;
if (verbose&32)
{ printf("\tpremark ");
comment(stdout, t->step->n, 0);
printf("\n");
}
break;
default:
break;
} } }
for (f = a->fsm; f; f = f->nxt)
{ fsm_tbl[f->from] = f;
f->seen = 0;
}
for (f = a->fsm; f; f = f->nxt)
{ if (!FSM_critical(a, f->from))
for (t = f->t; t; t = t->nxt)
if (t->relevant&2)
{ t->relevant &= ~2;
if (verbose&32)
{ printf("\t\tnomark ");
if (t->step && t->step->n)
comment(stdout, t->step->n, 0);
printf("\n");
} } }
for (f = a->fsm; f; f = f->nxt)
{ hit = 0;
for (t = f->t; t; t = t->nxt)
{ if (t->step && t->step->n)
switch (t->step->n->ntyp) {
case IF:
case DO:
case ATOMIC:
case NON_ATOMIC:
case D_STEP:
if (AST_blockable(a, t->to))
hit = 1;
break;
default:
break;
}
else if (AST_blockable(a, t->to))
hit = 1;
if (hit) break;
}
if (hit)
for (t = f->t; t; t = t->nxt)
{ t->round = AST_Round;
t->relevant |= 2;
if (verbose&32)
{ printf("\t\t\tliftmark ");
if (t->step && t->step->n)
comment(stdout, t->step->n, 0);
printf("\n");
}
AST_spread(a, t->to);
} }
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
if (t->relevant&2)
{ AST_shouldconsider(a, f->from);
break;
}
}
static void
AST_control_dep(void)
{ AST *a;
for (a = ast; a; a = a->nxt)
{ if (a->p->b != N_CLAIM && a->p->b != E_TRACE && a->p->b != N_TRACE)
{ AST_ctrl(a);
} }
}
static void
AST_prelabel(void)
{ AST *a;
FSM_state *f;
FSM_trans *t;
for (a = ast; a; a = a->nxt)
{ if (a->p->b != N_CLAIM && a->p->b != E_TRACE && a->p->b != N_TRACE)
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
{ if (t->step
&& t->step->n
&& t->step->n->ntyp == ASSERT
)
{ t->relevant |= 1;
} } }
}
static void
AST_criteria(void)
{
AST_Changes = 1;
for (AST_Round = 1; slicer && AST_Changes; AST_Round++)
{ AST_Changes = 0;
AST_data_dep();
AST_preserve();
AST_dominant();
AST_control_dep();
if (verbose&32)
printf("\n\nROUND %d -- changes %d\n",
AST_Round, AST_Changes);
}
}
static void
AST_alias_analysis(void)
{ AST *a;
for (a = ast; a; a = a->nxt)
AST_sends(a);
for (a = ast; a; a = a->nxt)
AST_para(a->p);
for (a = ast; a; a = a->nxt)
AST_other(a);
AST_trans();
if (verbose&32)
AST_aliases();
}
void
AST_slice(void)
{ AST *a;
int spurious = 0;
if (!slicer)
{ printf("spin: warning: no slice criteria found (no assertions and no claim)\n");
spurious = 1;
}
AST_dorelevant();
for (a = ast; a; a = a->nxt)
AST_def_use(a);
AST_hidden();
AST_alias_analysis();
AST_prelabel();
AST_criteria();
if (!spurious || (verbose&32))
{ spurious = 1;
for (a = ast; a; a = a->nxt)
{ AST_dump(a);
if (a->relevant&2)
spurious = 0;
}
if (!AST_dump_rel()
&& spurious)
printf("spin: no redundancies found (for given property)\n");
}
AST_suggestions();
if (verbose&32)
show_expl();
}
void
AST_store(ProcList *p, int start_state)
{ AST *n_ast;
if (p->b != N_CLAIM && p->b != E_TRACE && p->b != N_TRACE)
{ n_ast = (AST *) emalloc(sizeof(AST));
n_ast->p = p;
n_ast->i_st = start_state;
n_ast->relevant = 0;
n_ast->fsm = fsm;
n_ast->nxt = ast;
ast = n_ast;
}
fsm = (FSM_state *) 0;
}
static void
AST_add_explicit(Lextok *d, Lextok *u)
{ FSM_trans *e = (FSM_trans *) emalloc(sizeof(FSM_trans));
e->to = 0;
e->relevant = 0;
e->step = (Element *) 0;
e->Val[0] = e->Val[1] = (FSM_use *) 0;
cur_t = e;
def_use(u, USE);
def_use(d, DEF);
cur_t = (FSM_trans *) 0;
e->nxt = explicit;
explicit = e;
}
static void
AST_fp1(char *s, Lextok *t, Lextok *f, int parno)
{ Lextok *v;
int cnt;
if (!t) return;
if (t->ntyp == RUN)
{ if (strcmp(t->sym->name, s) == 0)
for (v = t->lft, cnt = 1; v; v = v->rgt, cnt++)
if (cnt == parno)
{ AST_add_explicit(f, v->lft);
break;
}
} else
{ AST_fp1(s, t->lft, f, parno);
AST_fp1(s, t->rgt, f, parno);
}
}
static void
AST_mk1(char *s, Lextok *c, int parno)
{ AST *a;
FSM_state *f;
FSM_trans *t;
for (a = ast; a; a = a->nxt)
for (f = a->fsm; f; f = f->nxt)
for (t = f->t; t; t = t->nxt)
{ if (t->step)
AST_fp1(s, t->step->n, c, parno);
}
}
static void
AST_par_init(void)
{ AST *a;
Lextok *f, *t, *c;
int cnt;
for (a = ast; a; a = a->nxt)
{ if (a->p->b == N_CLAIM || a->p->b == I_PROC
|| a->p->b == E_TRACE || a->p->b == N_TRACE)
{ continue;
}
cnt = 0;
for (f = a->p->p; f; f = f->rgt)
for (t = f->lft; t; t = t->rgt)
{ cnt++;
c = (t->ntyp != ',')? t : t->lft;
AST_mk1(a->p->n->name, c, cnt);
} }
}
static void
AST_var_init(void)
{ Ordered *walk;
Lextok *x;
Symbol *sp;
AST *a;
for (walk = all_names; walk; walk = walk->next)
{ sp = walk->entry;
if (sp
&& !sp->context
&& sp->type != PROCTYPE
&& sp->ini
&& (sp->type != MTYPE || sp->ini->ntyp != CONST)
&& sp->ini->ntyp != CHAN)
{ x = nn(ZN, TYPE, ZN, ZN);
x->sym = sp;
AST_add_explicit(x, sp->ini);
} }
for (a = ast; a; a = a->nxt)
{ if (a->p->b != N_CLAIM
&& a->p->b != E_TRACE && a->p->b != N_TRACE)
for (walk = all_names; walk; walk = walk->next)
{ sp = walk->entry;
if (sp
&& sp->context
&& strcmp(sp->context->name, a->p->n->name) == 0
&& sp->Nid >= 0
&& sp->type != LABEL
&& sp->ini
&& sp->ini->ntyp != CHAN)
{ x = nn(ZN, TYPE, ZN, ZN);
x->sym = sp;
AST_add_explicit(x, sp->ini);
} } }
}
static void
show_expl(void)
{ FSM_trans *t, *T;
FSM_use *u;
printf("\nExplicit List:\n");
for (T = expl_par; T; T = (T == expl_par)?expl_var: (FSM_trans *) 0)
{ for (t = T; t; t = t->nxt)
{ if (!t->Val[0]) continue;
printf("%s", t->relevant?"*":" ");
printf("%3d", t->round);
for (u = t->Val[0]; u; u = u->nxt)
{ printf("\t<");
AST_var(u->n, u->n->sym, 1);
printf(":%d>, ", u->special);
}
printf("\n");
}
printf("==\n");
}
printf("End\n");
}
static void
AST_hidden(void)
{
AST_par_init();
expl_par = explicit;
explicit = (FSM_trans *) 0;
AST_var_init();
expl_var = explicit;
explicit = (FSM_trans *) 0;
}
#define BPW (8*sizeof(ulong))
static int
bad_scratch(FSM_state *f, int upto)
{ FSM_trans *t;
#if 0
1. all internal branch-points have else-s
2. all non-branchpoints have non-blocking out-edge
3. all internal edges are non-relevant
subgraphs like this need NOT contribute control-dependencies
#endif
if (!f->seen
|| (f->scratch&4))
return 0;
if (f->scratch&8)
return 1;
f->scratch |= 4;
if (verbose&32) printf("X[%d:%d:%d] ", f->from, upto, f->scratch);
if (f->scratch&1)
{ if (verbose&32)
printf("\tbad scratch: %d\n", f->from);
bad: f->scratch &= ~4;
return 1;
}
if (f->from != upto)
for (t = f->t; t; t = t->nxt)
if (bad_scratch(fsm_tbl[t->to], upto))
goto bad;
return 0;
}
static void
mark_subgraph(FSM_state *f, int upto)
{ FSM_trans *t;
if (f->from == upto
|| !f->seen
|| (f->scratch&2))
return;
f->scratch |= 2;
for (t = f->t; t; t = t->nxt)
mark_subgraph(fsm_tbl[t->to], upto);
}
static void
AST_pair(AST *a, FSM_state *h, int y)
{ Pair *p;
for (p = a->pairs; p; p = p->nxt)
if (p->h == h
&& p->b == y)
return;
p = (Pair *) emalloc(sizeof(Pair));
p->h = h;
p->b = y;
p->nxt = a->pairs;
a->pairs = p;
}
static void
AST_checkpairs(AST *a)
{ Pair *p;
for (p = a->pairs; p; p = p->nxt)
{ if (verbose&32)
printf("	inspect pair %d %d\n", p->b, p->h->from);
if (!bad_scratch(p->h, p->b))
{ if (verbose&32)
printf("subgraph: %d .. %d\n", p->b, p->h->from);
mark_subgraph(p->h, p->b);
}
}
}
static void
subgraph(AST *a, FSM_state *f, int out)
{ FSM_state *h;
int i, j;
ulong *g;
#if 0
reverse dominance suggests that this is a possible
entry and exit node for a proper subgraph
#endif
h = fsm_tbl[out];
i = f->from / BPW;
j = f->from % BPW;
g = h->mod;
if (verbose&32)
printf("possible pair %d %d -- %d\n",
f->from, h->from, (g[i]&(1<<j))?1:0);
if (g[i]&(1<<j))
AST_pair(a, h, f->from);
}
static void
act_dom(AST *a)
{ FSM_state *f;
FSM_trans *t;
int i, j, cnt;
for (f = a->fsm; f; f = f->nxt)
{ if (!f->seen) continue;
#if 0
f->from is the exit-node of a proper subgraph, with
the dominator its entry-node, if:
a. this node has more than 1 reachable predecessor
b. the dominator has more than 1 reachable successor
(need reachability - in case of reverse dominance)
d. the dominator is reachable, and not equal to this node
#endif
for (t = f->p, i = 0; t; t = t->nxt)
i += fsm_tbl[t->to]->seen;
if (i <= 1) continue;
for (cnt = 1; cnt < a->nstates; cnt++)
{ if (cnt == f->from
|| !fsm_tbl[cnt]->seen)
continue;
i = cnt / BPW;
j = cnt % BPW;
if (!(f->dom[i]&(1<<j)))
continue;
for (t = fsm_tbl[cnt]->t, i = 0; t; t = t->nxt)
i += fsm_tbl[t->to]->seen;
if (i <= 1)
continue;
if (f->mod)
subgraph(a, f, cnt);
}
}
}
static void
reachability(AST *a)
{ FSM_state *f;
for (f = a->fsm; f; f = f->nxt)
f->seen = 0;
AST_dfs(a, a->i_st, 0);
}
static int
see_else(FSM_state *f)
{ FSM_trans *t;
for (t = f->t; t; t = t->nxt)
{ if (t->step
&& t->step->n)
switch (t->step->n->ntyp) {
case ELSE:
return 1;
case IF:
case DO:
case ATOMIC:
case NON_ATOMIC:
case D_STEP:
if (see_else(fsm_tbl[t->to]))
return 1;
default:
break;
}
}
return 0;
}
static int
is_guard(FSM_state *f)
{ FSM_state *g;
FSM_trans *t;
for (t = f->p; t; t = t->nxt)
{ g = fsm_tbl[t->to];
if (!g->seen)
continue;
if (t->step
&& t->step->n)
switch(t->step->n->ntyp) {
case IF:
case DO:
return 1;
case ATOMIC:
case NON_ATOMIC:
case D_STEP:
if (is_guard(g))
return 1;
default:
break;
}
}
return 0;
}
static void
curtail(AST *a)
{ FSM_state *f, *g;
FSM_trans *t;
int i, haselse, isrel, blocking;
#if 0
mark nodes that do not satisfy these requirements:
1. all internal branch-points have else-s
2. all non-branchpoints have non-blocking out-edge
3. all internal edges are non-data-relevant
#endif
if (verbose&32)
printf("Curtail %s:\n", a->p->n->name);
for (f = a->fsm; f; f = f->nxt)
{ if (!f->seen
|| (f->scratch&(1|2)))
continue;
isrel = haselse = i = blocking = 0;
for (t = f->t; t; t = t->nxt)
{ g = fsm_tbl[t->to];
isrel |= (t->relevant&1);
i += g->seen;
if (t->step
&& t->step->n)
{ switch (t->step->n->ntyp) {
case IF:
case DO:
haselse |= see_else(g);
break;
case 'c':
case 's':
case 'r':
blocking = 1;
break;
} } }
#if 0
if (verbose&32)
printf("prescratch %d -- %d %d %d %d -- %d\n",
f->from, i, isrel, blocking, haselse, is_guard(f));
#endif
if (isrel
|| (i == 1 && blocking)
|| (i > 1 && !haselse))
{ if (!is_guard(f))
{ f->scratch |= 1;
if (verbose&32)
printf("scratch %d -- %d %d %d %d\n",
f->from, i, isrel, blocking, haselse);
}
}
}
}
static void
init_dom(AST *a)
{ FSM_state *f;
int i, j, cnt;
#if 0
(1) D(s0) = {s0}
(2) for s in S - {s0} do D(s) = S
#endif
for (f = a->fsm; f; f = f->nxt)
{ if (!f->seen) continue;
f->dom = (ulong *)
emalloc(a->nwords * sizeof(ulong));
if (f->from == a->i_st)
{ i = a->i_st / BPW;
j = a->i_st % BPW;
f->dom[i] = (1<<j);
} else
{ for (i = 0; i < a->nwords; i++)
f->dom[i] = (ulong) ~0;
if (a->nstates % BPW)
for (i = (a->nstates % BPW); i < (int) BPW; i++)
f->dom[a->nwords-1] &= ~(1<<i);
for (cnt = 0; cnt < a->nstates; cnt++)
if (!fsm_tbl[cnt]->seen)
{ i = cnt / BPW;
j = cnt % BPW;
f->dom[i] &= ~(1<<j);
} } }
}
static int
dom_perculate(AST *a, FSM_state *f)
{ static ulong *ndom = (ulong *) 0;
static int on = 0;
int i, j, cnt = 0;
FSM_state *g;
FSM_trans *t;
if (on < a->nwords)
{ on = a->nwords;
ndom = (ulong *)
emalloc(on * sizeof(ulong));
}
for (i = 0; i < a->nwords; i++)
ndom[i] = (ulong) ~0;
for (t = f->p; t; t = t->nxt)
{ g = fsm_tbl[t->to];
if (g->seen)
for (i = 0; i < a->nwords; i++)
ndom[i] &= g->dom[i];
}
i = f->from / BPW;
j = f->from % BPW;
ndom[i] |= (1<<j);
for (i = 0; i < a->nwords; i++)
if (f->dom[i] != ndom[i])
{ cnt++;
f->dom[i] = ndom[i];
}
return cnt;
}
static void
dom_forward(AST *a)
{ FSM_state *f;
int cnt;
init_dom(a);
do {
cnt = 0;
for (f = a->fsm; f; f = f->nxt)
{ if (f->seen
&& f->from != a->i_st)
cnt += dom_perculate(a, f);
}
} while (cnt);
dom_perculate(a, fsm_tbl[a->i_st]);
}
static void
AST_dominant(void)
{ FSM_state *f;
FSM_trans *t;
AST *a;
int oi;
static FSM_state no_state;
#if 0
find dominators
Aho, Sethi, & Ullman, Compilers - principles, techniques, and tools
Addison-Wesley, 1986, p.671.
(1) D(s0) = {s0}
(2) for s in S - {s0} do D(s) = S
(3) while any D(s) changes do
(4) for s in S - {s0} do
(5) D(s) = {s} union with intersection of all D(p)
where p are the immediate predecessors of s
the purpose is to find proper subgraphs
(one entry node, one exit node)
#endif
if (AST_Round == 1)
for (a = ast; a; a = a->nxt)
{ a->nstates = 0;
for (f = a->fsm; f; f = f->nxt)
{ a->nstates++;
fsm_tbl[f->from] = f;
f->scratch = 0;
}
for (oi = 0; oi < a->nstates; oi++)
if (!fsm_tbl[oi])
fsm_tbl[oi] = &no_state;
a->nwords = (a->nstates + BPW - 1) / BPW;
if (verbose&32)
{ printf("%s (%d): ", a->p->n->name, a->i_st);
printf("states=%d (max %d), words = %d, bpw %d, overflow %d\n",
a->nstates, o_max, a->nwords,
(int) BPW, (int) (a->nstates % BPW));
}
reachability(a);
dom_forward(a);
curtail(a);
for (f = a->fsm; f; f = f->nxt)
{ t = f->p;
f->p = f->t;
f->t = t;
f->mod = f->dom;
f->dom = (ulong *) 0;
}
oi = a->i_st;
if (fsm_tbl[0]->seen)
a->i_st = 0;
dom_forward(a);
act_dom(a);
AST_checkpairs(a);
for (f = a->fsm; f; f = f->nxt)
{ t = f->p;
f->p = f->t;
f->t = t;
}
a->i_st = oi;
} else
for (a = ast; a; a = a->nxt)
{ for (f = a->fsm; f; f = f->nxt)
{ fsm_tbl[f->from] = f;
f->scratch &= 1;
}
for (oi = 0; oi < a->nstates; oi++)
if (!fsm_tbl[oi])
fsm_tbl[oi] = &no_state;
curtail(a);
for (f = a->fsm; f; f = f->nxt)
{ t = f->p;
f->p = f->t;
f->t = t;
}
AST_checkpairs(a);
for (f = a->fsm; f; f = f->nxt)
{ t = f->p;
f->p = f->t;
f->t = t;
} }
}