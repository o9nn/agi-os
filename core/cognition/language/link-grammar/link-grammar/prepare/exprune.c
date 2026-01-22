#include <inttypes.h>
#include "api-structures.h"
#include "connectors.h"
#include "dict-common/dict-structures.h"
#include "dict-common/dict-utils.h"
#include "print/print-util.h"
#include "string-set.h"
#include "tokenize/word-structures.h"
#include "exprune.h"
#define D_EXPRUNE 9
#define D_PRINT_NUM_DISJUNCTS 5
#ifdef DEBUG
#define DBG(p, w, X) \
if (verbosity_level(+D_EXPRUNE))\
{\
err_msg(lg_Trace, "pass%d w%zu: ", p, w);\
err_msg(lg_Trace, X ": %s\n", exp_stringify(x->exp));\
}
#else
#define DBG(p, w, X)
#endif
#define DBG_EXPSIZES(...) \
if (verbosity_level(+D_EXPRUNE))\
{\
char *e = print_expression_sizes(sent);\
err_msg(lg_Trace, __VA_ARGS__);\
free(e);\
}
typedef struct connector_table_s connector_table;
struct connector_table_s
{
condesc_t *condesc;
connector_table *next;
int farthest_word;
};
#define CT_BLKSIZE 512
typedef struct exprune_context_s exprune_context;
struct exprune_context_s
{
connector_table **ct;
size_t ct_size;
Parse_Options opts;
connector_table *current_element;
connector_table *end_current_block;
connector_table connector_table_element[CT_BLKSIZE];
int N_deleted;
};
static connector_table *ct_element_new(exprune_context *ctxt)
{
if (ctxt->current_element == ctxt->end_current_block)
{
if (ctxt->end_current_block->next == NULL)
{
connector_table *newblock =
malloc(CT_BLKSIZE * sizeof(*ctxt->current_element));
newblock[CT_BLKSIZE-1].next = NULL;
ctxt->end_current_block->next = newblock;
}
ctxt->current_element = ctxt->end_current_block->next;
ctxt->end_current_block = &ctxt->current_element[CT_BLKSIZE-1];
}
return ctxt->current_element++;
}
static void free_connector_table(exprune_context *ctxt)
{
connector_table *x;
connector_table *t = ctxt->connector_table_element[CT_BLKSIZE-1].next;
while (t != NULL)
{
x = t[CT_BLKSIZE-1].next;
free(t);
t = x;
}
free(ctxt->ct);
ctxt->ct = NULL;
ctxt->ct_size = 0;
}
static inline unsigned int hash_S(condesc_t * c)
{
return c->uc_num;
}
static inline bool matches_S(connector_table **ct, int w, condesc_t * c)
{
connector_table *e;
for (e = ct[hash_S(c)]; e != NULL; e = e->next)
{
if (w > e->farthest_word) continue;
if (easy_match_desc(e->condesc, c)) return true;
}
return false;
}
static Exp* purge_Exp(exprune_context *ctxt, int, Exp *, char);
static bool or_purge_operands(exprune_context *ctxt, int w, Exp *e, char dir)
{
#if NOTYET
const float nullexp_nonexistence = -9999;
float nullexp_mincost = nullexp_nonexistence;
int nullexp_count = 0;
#endif
for (Exp **opdp = &e->operand_first; *opdp != NULL; )
{
Exp *opd = *opdp;
#if NOTYET
if ((opd->type == AND_type) && (opd->operand_first == NULL))
{
if (opd->cost > nullexp_mincost) nullexp_mincost = opd->cost;
nullexp_count++;
}
else
#endif
if (purge_Exp(ctxt, w, opd, dir) == NULL)
{
*opdp = opd->operand_next;
continue;
}
opdp = &opd->operand_next;
};
#if NOTYET
if ((nullexp_count > 1) && (nullexp_mincost != nullexp_nonexistence))
{
bool nullexp_retained = false;
for (Exp **opdp = &e->operand_first; *opdp != NULL; )
{
Exp *opd = *opdp;
if ((opd->type == AND_type) && (opd->operand_first == NULL))
{
if (!nullexp_retained && opd->cost == nullexp_mincost)
{
nullexp_retained = true;
}
else
{
*opdp = opd->operand_next;
continue;
}
}
opdp = &opd->operand_next;
}
}
#endif
return (e->operand_first != NULL);
}
static bool and_purge_operands(exprune_context *ctxt, int w, Exp *e, char dir)
{
for (Exp **opdp = &e->operand_first; *opdp != NULL; )
{
Exp *opd = *opdp;
#ifdef NOTYET
if ((opd->type == AND_type) && (opd->operand_first == NULL))
{
e->cost += opd->cost;
*opdp = opd->operand_next;
}
#endif
if (purge_Exp(ctxt, w, opd, dir) == NULL) return false;
opdp = &opd->operand_next;
}
return true;
}
static Exp* purge_Exp(exprune_context *ctxt, int w, Exp *e, char dir)
{
if (e->type == CONNECTOR_type)
{
if (e->dir == dir)
{
if (!matches_S(ctxt->ct, (dir == '-') ? w : -w, e->condesc))
{
ctxt->N_deleted++;
return NULL;
}
}
return e;
}
if (e->type == AND_type)
{
if (!and_purge_operands(ctxt, w, e, dir)) return NULL;
}
else
{
if (!or_purge_operands(ctxt, w, e, dir)) return NULL;
}
if ((e->operand_first != NULL) && (e->operand_first->operand_next == NULL))
{
Exp *opd = e->operand_first;
opd->cost += e->cost;
opd->operand_next = e->operand_next;
*e = *opd;
}
return e;
}
static void zero_connector_table(exprune_context *ctxt)
{
memset(ctxt->ct, 0, sizeof(*ctxt->ct) * ctxt->ct_size);
ctxt->current_element = ctxt->connector_table_element;
ctxt->end_current_block = &ctxt->connector_table_element[CT_BLKSIZE-1];
}
static void insert_connector(exprune_context *ctxt, int farthest_word,
condesc_t *c)
{
unsigned int h;
connector_table *e;
h = hash_S(c);
for (e = ctxt->ct[h]; e != NULL; e = e->next)
{
if (c == e->condesc)
{
if (e->farthest_word < farthest_word) e->farthest_word = farthest_word;
return;
}
}
e = ct_element_new(ctxt);
e->condesc = c;
e->farthest_word = farthest_word;
e->next = ctxt->ct[h];
ctxt->ct[h] = e;
}
static void insert_connectors(exprune_context *ctxt, int w, Exp * e, int dir)
{
if (e->type == CONNECTOR_type)
{
if (e->dir == dir)
{
assert(NULL != e->condesc, "NULL connector");
int farthest_word = (dir == '-') ? -e->farthest_word : e->farthest_word;
insert_connector(ctxt, farthest_word, e->condesc);
}
}
else
{
for (Exp *opd = e->operand_first; opd != NULL; opd = opd->operand_next)
{
insert_connectors(ctxt, w, opd, dir);
}
}
}
static char *print_expression_sizes(Sentence sent)
{
X_node * x;
size_t w, size;
dyn_str *e = dyn_str_new();
for (w=0; w<sent->length; w++) {
size = 0;
for (x=sent->word[w].x; x!=NULL; x = x->next) {
size += size_of_expression(x->exp);
}
append_string(e, "%s[%zu] ", sent->word[w].alternatives[0], size);
}
append_string(e, "\n\n");
return dyn_str_take(e);
}
static void print_expression_disjunct_count(Sentence sent)
{
uint64_t dcnt, t = 0;
for (WordIdx i = 0; i < sent->length; i++)
{
dcnt = 0;
for (const X_node *x = sent->word[i].x; x != NULL; x = x->next)
dcnt += count_clause(x->exp);
prt_error("%s(%"PRIu64") ", sent->word[i].alternatives[0], dcnt);
t += dcnt;
}
prt_error("\n\\");
prt_error("Total: %"PRIu64" disjuncts\n\n", t);
}
void expression_prune(Sentence sent, Parse_Options opts)
{
size_t w;
exprune_context ctxt;
ctxt.opts = opts;
ctxt.ct_size = sent->dict->contable.num_uc;
ctxt.ct = malloc(ctxt.ct_size * sizeof(*ctxt.ct));
zero_connector_table(&ctxt);
ctxt.end_current_block->next = NULL;
ctxt.N_deleted = 1;
DBG_EXPSIZES("Initial expression sizes\n%s", e);
if (verbosity_level(D_PRINT_NUM_DISJUNCTS))
{
prt_error("Debug: Before expression_prune():\n\\");
print_expression_disjunct_count(sent);
}
for (int pass = 0; ; pass++)
{
for (w = 0; w < sent->length; w++)
{
for (X_node **xp = &sent->word[w].x; *xp != NULL; )
{
X_node *x = *xp;
DBG(pass, w, "l->r pass before purging");
x->exp = purge_Exp(&ctxt, w, x->exp, '-');
DBG(pass, w, "l->r pass after purging");
if (x->exp == NULL)
{
*xp = x->next;
}
else
{
xp = &x->next;
}
}
for (X_node *x = sent->word[w].x; x != NULL; x = x->next)
{
insert_connectors(&ctxt, w, x->exp, '+');
}
}
DBG_EXPSIZES("l->r pass removed %d\n%s", ctxt.N_deleted, e);
if (ctxt.N_deleted == 0) break;
zero_connector_table(&ctxt);
ctxt.N_deleted = 0;
for (w = sent->length-1; w != (size_t) -1; w--)
{
for (X_node **xp = &sent->word[w].x; *xp != NULL; )
{
X_node *x = *xp;
DBG(pass, w, "r->l pass before purging");
x->exp = purge_Exp(&ctxt, w, x->exp, '+');
DBG(pass, w, "r->l pass after purging");
if (x->exp == NULL)
{
*xp = x->next;
}
else
{
xp = &x->next;
}
}
for (X_node *x = sent->word[w].x; x != NULL; x = x->next)
{
insert_connectors(&ctxt, w, x->exp, '-');
}
}
DBG_EXPSIZES("r->l pass removed %d\n%s", ctxt.N_deleted, e);
if (ctxt.N_deleted == 0) break;
zero_connector_table(&ctxt);
ctxt.N_deleted = 0;
}
free_connector_table(&ctxt);
if (verbosity_level(D_PRINT_NUM_DISJUNCTS))
{
prt_error("Debug: After expression_prune():\n\\");
print_expression_disjunct_count(sent);
}
}
#if 0
static int string_hash(disjunct_dup_table *dt, const char * s, int i)
{
for(;*s != '\0';s++) i = i + (i<<1) + randtable[(*s + i) & (RTSIZE-1)];
return (i & (dt->dup_table_size-1));
}
static bool connector_matches_alam(Connector * a, Connector * b)
{
char * s, * t, *u;
if (((!a->multi) && b->multi) ||
(a->label != b->label)) return false;
s = a->string;
t = b->string;
while (isupper(*s) || isupper(*t))
{
if (*s == *t) {
s++;
t++;
} else return false;
}
while ((*s != '\0') && (*t != '\0')) {
if ((*s == *t) || (*s == '*')) {
s++;
t++;
} else return false;
}
while ((*s != '\0') && (*s == '*')) s++;
return (*s == '\0');
}
static int conn_hash(Connector * c, int i)
{
int nb;
const char * s;
s = c->string;
i = i + (i<<1) + randtable[(c->label + i) & (RTSIZE-1)];
nb = is_utf8_upper(s);
while (nb)
{
i = i + (i<<1) + randtable[(*s + i) & (RTSIZE-1)];
s += nb;
nb = is_utf8_upper(s);
}
return i;
}
static inline int pconnector_hash(disjunct_dup_table *dt, Connector * c, int i)
{
i = conn_hash(c, i);
return (i & (ct->dup_table_size-1));
}
static int hash_disjunct(disjunct_dup_table *dt, Disjunct * d)
{
int i;
Connector *e;
i = 0;
for (e = d->left; e != NULL; e = e->next)
{
i = pconnector_hash(dt, e, i);
}
for (e = d->right; e != NULL; e = e->next)
{
i = pconnector_hash(dt, e, i);
}
return string_hash(dt, d->string, i);
}
static bool disjunct_matches_alam(Disjunct * d1, Disjunct * d2)
{
Connector *e1, *e2;
if (d1->cost > d2->cost) return false;
e1 = d1->left;
e2 = d2->left;
while ((e1!=NULL) && (e2!=NULL))
{
if (!connector_matches_alam(e1,e2)) break;
e1 = e1->next;
e2 = e2->next;
}
if ((e1!=NULL) || (e2!=NULL)) return false;
e1 = d1->right;
e2 = d2->right;
while ((e1!=NULL) && (e2!=NULL))
{
if (!connector_matches_alam(e1,e2)) break;
e1 = e1->next;
e2 = e2->next;
}
if ((e1!=NULL) || (e2!=NULL)) return false;
return (strcmp(d1->string, d2->string) == 0);
}
Disjunct * eliminate_duplicate_disjuncts(Disjunct * d)
{
int i, h, count;
Disjunct *dn, *dx, *dxn, *front;
count = 0;
disjunct_dup_table *dt;
dt = disjunct_dup_table_new(next_power_of_two_up(2 * count_disjuncts(d)));
for (;d!=NULL; d = dn)
{
dn = d->next;
h = hash_disjunct(d);
front = NULL;
for (dx = dt->dup_table[h]; dx != NULL; dx = dxn)
{
dxn = dx->next;
if (disjunct_matches_alam(dx,d))
{
d->next = NULL;
free_disjuncts(d);
count++;
front = catenate_disjuncts(front, dx);
break;
} else if (disjunct_matches_alam(d,dx)) {
dx->next = NULL;
free_disjuncts(dx);
count++;
} else {
dx->next = front;
front = dx;
}
}
if (dx == NULL) {
d->next = front;
front = d;
}
dt->dup_table[h] = front;
}
for (i = 0; i < dt->dup_table_size; i++)
{
for (dx = dt->dup_table[i]; dx != NULL; dx = dxn)
{
dxn = dx->next;
dx->next = d;
d = dx;
}
}
if ((verbosity > 2) && (count != 0)) printf("killed %d duplicates\n", count);
disjunct_dup_table_delete(dt);
return d;
}
#endif