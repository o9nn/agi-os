#include <limits.h>
#ifdef __APPLE__
#include <malloc/malloc.h>
#else
#include <malloc.h>
#endif
#include <math.h>
#include "connectors.h"
#include "count.h"
#include "disjunct-utils.h"
#include "extract-links.h"
#include "fast-match.h"
#include "memory-pool.h"
#include "utilities.h"
#include "linkage/linkage.h"
#include "tokenize/word-structures.h"
#define D_EXTRACT 5
#ifdef DEBUG
#define DEBUG_X_TABLE
#endif
typedef struct Parse_choice_struct Parse_choice;
typedef struct Parse_set_struct Parse_set;
struct Parse_choice_struct
{
Parse_choice * next;
Parse_set * set[2];
Disjunct    *md;
int32_t     l_id, r_id;
#ifdef PC_DISPLAY
bool done;
bool dolr;
#endif
};
struct Parse_set_struct
{
Connector      *le, *re;
Parse_choice   *first;
unsigned int   num_pc;
uint8_t        lw, rw;
uint8_t        null_count;
count_t count;
#ifdef RECOUNT
count_t recount;
count_t cut_count;
#undef RECOUNT
#define RECOUNT(X) X
#else
#define RECOUNT(X)
#endif
};
typedef struct Pset_bucket_struct Pset_bucket;
struct Pset_bucket_struct
{
Parse_set set;
Pset_bucket *next;
};
struct extractor_s
{
unsigned int   x_table_size;
unsigned int   log2_x_table_size;
Pset_bucket ** x_table;
Parse_set *    parse_set;
Word           *words;
Pool_desc *    Pset_bucket_pool;
Pool_desc *    Parse_choice_pool;
bool           islands_ok;
unsigned int rand_state;
};
static Parse_choice *
make_choice(Parse_set *lset, Connector * lrc,
Parse_set *rset, Connector * rlc,
Disjunct *md, extractor_t* pex)
{
Parse_choice *pc = pool_alloc(pex->Parse_choice_pool);
pc->next = NULL;
pc->set[0] = lset;
pc->set[1] = rset;
pc->l_id = (lrc == NULL) ? -1 : lrc->tracon_id;
pc->r_id = (rlc == NULL) ? -1 : rlc->tracon_id;
pc->md = md;
#ifdef PC_DISPLAY
pc->done = false;
pc->dolr = false;
#endif
return pc;
}
static void record_choice(
Parse_set *lset, Connector * lrc,
Parse_set *rset, Connector * rlc,
Disjunct *md, Parse_set *s, extractor_t* pex)
{
Parse_choice *pc = make_choice(lset, lrc, rset, rlc, md, pex);
pc->next = s->first;
s->first = pc;
s->num_pc++;
}
static int estimate_log2_table_size(Sentence sent)
{
double lscale = log2((double)sent->num_disjuncts + 1.0) -
0.5 * log2((double)sent->length);
double lo_est = lscale + 4.0;
double hi_est = 1.5 * lscale;
double dj_est = fmax(lo_est, hi_est);
double ntracon = (double)pool_num_elements_issued(sent->Table_tracon_pool);
double ltra = log2(ntracon) + 1.0;
int log2_table_size = (int)floor(fmax(dj_est, ltra));
if (log2_table_size < 4) log2_table_size = 4;
if (24 < log2_table_size) log2_table_size = 24;
#if LATER
if (IS_GENERATION(sent->dict))
log2_table_size = 28;
#endif
return log2_table_size;
}
static size_t estimate_parse_choice_allocations(Sentence sent)
{
size_t expsz = pool_num_elements_issued(sent->Exp_pool);
size_t pcsze = (expsz * expsz) / 100000;
if (pcsze < 1020) pcsze = 1020;
#define MAX_PC_ELTS (16*1024*1024 - 10)
if (MAX_PC_ELTS < pcsze) pcsze = MAX_PC_ELTS;
return pcsze;
}
extractor_t * extractor_new(Sentence sent)
{
extractor_t * pex = (extractor_t *) xalloc(sizeof(extractor_t));
memset(pex, 0, sizeof(extractor_t));
pex->rand_state = sent->rand_state;
int log2_table_size = estimate_log2_table_size(sent);
pex->log2_x_table_size = log2_table_size;
pex->x_table_size = (1 << log2_table_size);
pex->x_table = (Pset_bucket**) xalloc(pex->x_table_size * sizeof(Pset_bucket*));
memset(pex->x_table, 0, pex->x_table_size * sizeof(Pset_bucket*));
size_t pbsze = pex->x_table_size / 4;
pex->Pset_bucket_pool =
pool_new(__func__, "Pset_bucket",
pbsze, sizeof(Pset_bucket),
false, false, false);
size_t pcsze = estimate_parse_choice_allocations(sent);
pex->Parse_choice_pool =
pool_new(__func__, "Parse_choice",
pcsze, sizeof(Parse_choice),
false, false, false);
return pex;
}
void free_extractor(extractor_t * pex)
{
if (!pex) return;
#ifdef DEBUG_X_TABLE
if (verbosity_level(D_EXTRACT))
{
unsigned int num_entries = 0;
for (unsigned int i = 0; i < pex->x_table_size; i++)
{
if (pex->x_table[i] == NULL) continue;
num_entries++;
}
printf("x_table: used=%u/%u (%.2f%%) pset_bucket=%zu (avg chain %.2f) "
"parse_choice=%zu\n",
num_entries, pex->x_table_size,
100.0f*num_entries / pex->x_table_size,
pool_num_elements_issued(pex->Pset_bucket_pool),
1.0f*pool_num_elements_issued(pex->Pset_bucket_pool) / pex->x_table_size,
pool_size(pex->Parse_choice_pool));
}
#endif
pex->parse_set = NULL;
xfree((void *) pex->x_table, pex->x_table_size * sizeof(Pset_bucket*));
pex->x_table_size = 0;
pex->x_table = NULL;
#if HAVE_MALLOC_TRIM
bool trim = false;
if (3012012 < pool_size(pex->Parse_choice_pool)) trim = true;
#endif
pool_delete(pex->Pset_bucket_pool);
pool_delete(pex->Parse_choice_pool);
xfree((void *) pex, sizeof(extractor_t));
#if HAVE_MALLOC_TRIM
if (trim) malloc_trim(0);
#endif
}
static Connector *dummy_null_tracon(int w)
{
static Connector dnt[MAX_SENTENCE+1+1];
if (dnt[w+1].tracon_id != w) dnt[w+1].tracon_id = w;
return &dnt[w+1];
}
static Pset_bucket *x_table_pointer(int lw, int rw,
Connector *le, Connector *re,
unsigned int null_count, extractor_t * pex)
{
int l_id = (NULL != le) ? le->tracon_id : lw;
int r_id = (NULL != re) ? re->tracon_id : rw;
unsigned int hash = pair_hash(lw, rw, l_id, r_id, null_count);
Pset_bucket *t = pex->x_table[hash & (pex->x_table_size-1)];
for (; t != NULL; t = t->next)
{
if ((t->set.le->tracon_id == l_id) && (t->set.re->tracon_id == r_id) &&
(t->set.null_count == null_count)) return t;
}
return NULL;
}
static Pset_bucket * x_table_store(int lw, int rw,
Connector *le, Connector *re,
unsigned int null_count, extractor_t * pex)
{
int32_t l_id = (NULL != le) ? le->tracon_id : lw;
int32_t r_id = (NULL != re) ? re->tracon_id : rw;
unsigned int h = pair_hash(lw, rw, l_id, r_id, null_count);
Pset_bucket **t = &pex->x_table[h & (pex->x_table_size -1)];
Pset_bucket *n = pool_alloc(pex->Pset_bucket_pool);
n->set.lw = lw;
n->set.rw = rw;
n->set.null_count = null_count;
n->set.le = (NULL != le) ? le : dummy_null_tracon(lw);
n->set.re = (NULL != re) ? re : dummy_null_tracon(rw);
n->set.count = 0;
n->set.first = NULL;
n->set.num_pc = 0;
n->next = *t;
*t = n;
return n;
}
static Parse_set* dummy_set(int lw, int rw,
unsigned int null_count, extractor_t * pex)
{
Pset_bucket *dummy;
dummy = x_table_pointer(lw, rw, NULL, NULL, null_count, pex);
if (dummy) return &dummy->set;
dummy = x_table_store(lw, rw, NULL, NULL, null_count, pex);
dummy->set.count = 1;
return &dummy->set;
}
static count_t table_count(count_context_t * ctxt,
int lw, int rw, Connector *le, Connector *re,
unsigned int null_count)
{
if ((le != NULL) && (re != NULL) && (le->nearest_word > re->nearest_word))
return 0;
Count_bin *count = table_lookup(ctxt, lw, rw, le, re, null_count, NULL);
if (NULL == count) return 0;
return hist_total(count);
}
static bool fetch_counts(count_context_t *ctxt, count_t count[4],
int ew, int w, Connector *e, Connector *c,
unsigned int null_count)
{
count[0] = table_count(ctxt, ew, w, e->next, c->next, null_count);
if (e->multi)
count[1] = table_count(ctxt, ew, w, e, c->next, null_count);
if (c->multi)
count[2] = table_count(ctxt, ew, w, e->next, c, null_count);
if (e->multi && c->multi)
count[3] = table_count(ctxt, ew, w, e, c, null_count);
return (count[0] > 0) || (count[1] > 0) || (count[2] > 0) || (count[3] > 0);
}
static
Parse_set *mk_parse_set(fast_matcher_t *mchxt,
count_context_t *ctxt, count_t count,
int lw, int rw,
Connector *le, Connector *re, unsigned int null_count,
extractor_t *pex);
static
bool smk_parse_set(fast_matcher_t *mchxt,
count_context_t *ctxt, count_t count[4],
int lw, int rw,
Connector *le, Connector *re, unsigned int null_count,
extractor_t *pex, Parse_set *s[4])
{
s[0] = mk_parse_set(mchxt, ctxt, count[0], lw, rw, le->next, re->next,
null_count, pex);
if (le->multi)
s[1] = mk_parse_set(mchxt, ctxt, count[1], lw, rw, le, re->next,
null_count, pex);
if (re->multi)
s[2] = mk_parse_set(mchxt, ctxt, count[2], lw, rw, le->next, re,
null_count, pex);
if (le->multi && re->multi)
s[3] = mk_parse_set(mchxt, ctxt, count[3], lw, rw, le, re,
null_count, pex);
return ((s[0] != NULL) || (s[1] != NULL) || (s[2] != NULL) || (s[3] != NULL));
}
static
Parse_set * mk_parse_set(fast_matcher_t *mchxt,
count_context_t * ctxt, count_t count,
int lw, int rw,
Connector *le, Connector *re, unsigned int null_count,
extractor_t * pex)
{
if (!valid_nearest_words(le, re, lw, rw)) return NULL;
assert(null_count < 0x7fff, "Called with null_count < 0.");
if (count < 0)
count = table_count(ctxt, lw, rw, le, re, null_count);
if (count == 0) return NULL;
Pset_bucket *xtp = x_table_pointer(lw, rw, le, re, null_count, pex);
if (xtp != NULL) return &xtp->set;
xtp = x_table_store(lw, rw, le, re, null_count, pex);
xtp->set.count = count;
RECOUNT({xtp->set.recount = 1;})
if (lw + 1 == rw) return &xtp->set;
if ((le == NULL) && (re == NULL))
{
Parse_set* pset;
Parse_set* dummy;
Disjunct* dis;
if (!pex->islands_ok && (lw != -1) && (pex->words[lw].d != NULL))
return &xtp->set;
if (null_count == 0) return &xtp->set;
RECOUNT({xtp->set.recount = 0;})
int w = lw + 1;
for (int opt = 0; opt <= (int)pex->words[w].optional; opt++)
{
null_count += opt;
for (dis = pex->words[w].d; dis != NULL; dis = dis->next)
{
if (dis->left == NULL)
{
pset = mk_parse_set(mchxt, ctxt, -1,
w, rw, dis->right, NULL,
null_count-1, pex);
if (pset == NULL) continue;
dummy = dummy_set(lw, w, null_count-1, pex);
record_choice(dummy, NULL,
pset, dis->right,
dis, &xtp->set, pex);
RECOUNT({xtp->set.recount += pset->recount;})
}
}
pset = mk_parse_set(mchxt, ctxt, -1,
w, rw, NULL, NULL,
null_count-1, pex);
if (pset != NULL)
{
dummy = dummy_set(lw, w, null_count-1, pex);
record_choice(dummy, NULL,
pset, NULL,
NULL, &xtp->set, pex);
RECOUNT({xtp->set.recount += pset->recount;})
}
}
return &xtp->set;
}
int start_word;
if (le == NULL)
{
start_word = MAX(lw+1, re->farthest_word);
}
else
{
start_word = le->nearest_word;
}
int end_word;
if (re == NULL)
{
end_word = MIN(rw, le->farthest_word+1);
}
else
{
if ((le != NULL) && (re->nearest_word > le->farthest_word))
end_word = le->farthest_word + 1;
else
end_word = re->nearest_word + 1;
}
if (UINT_MAX == null_count) return NULL;
RECOUNT({xtp->set.recount = 0;})
for (int w = start_word; w < end_word; w++)
{
Connector *fml_re = re;
if (le != NULL)
{
if (no_count(ctxt, 0, le, w - le->nearest_word, null_count)) continue;
if ((re != NULL) && (re->farthest_word <= w))
{
if (no_count(ctxt, 1, re, w - re->farthest_word, null_count))
fml_re = NULL;
}
}
else
{
if (no_count(ctxt, 1, re, w - re->farthest_word, null_count)) continue;
}
match_list_cache *mlcl = NULL, *mlcr = NULL;
if (le != NULL)
mlcl = get_cached_match_list(ctxt, 0, w, le);
if (fml_re != NULL && ((le == NULL) || (re->farthest_word <= w)))
mlcr = get_cached_match_list(ctxt, 1, w, re);
size_t mlb = form_match_list(mchxt, w, le, lw, fml_re, rw, mlcl, mlcr);
for (size_t mle = mlb; get_match_list_element(mchxt, mle) != NULL; mle++)
{
Disjunct *d = get_match_list_element(mchxt, mle);
for (unsigned int lnull_count = 0; lnull_count <= null_count; lnull_count++)
{
bool Lmatch = d->match_left;
bool Rmatch = d->match_right;
unsigned int rnull_count = null_count - lnull_count;
count_t lcount[4] = { 0 }, rcount[4] = { 0 };
if (Lmatch)
Lmatch = fetch_counts(ctxt, lcount, lw, w, le, d->left, lnull_count);
if (Rmatch && (Lmatch || (le == NULL)))
Rmatch = fetch_counts(ctxt, rcount, w, rw, d->right, re, rnull_count);
count_t l_bnr = 0, r_bnl = 0;
if (Lmatch)
l_bnr = table_count(ctxt, w, rw, d->right, re, rnull_count);
else
{
if (!Rmatch) continue;
if (le == NULL)
r_bnl = table_count(ctxt, lw, w, le, d->left, lnull_count);
}
Parse_set *ls[4] = { NULL };
bool ls_exists = false;
if (Lmatch && (Rmatch || (l_bnr > 0)))
{
ls_exists = smk_parse_set(mchxt, ctxt, lcount,
lw, w, le, d->left,
lnull_count, pex, ls);
if (ls_exists)
{
Parse_set* rset = mk_parse_set(mchxt, ctxt, l_bnr,
w, rw, d->right, re,
rnull_count, pex);
if (rset != NULL)
{
for (int i = 0; i < 4; i++)
{
if (ls[i] == NULL) continue;
record_choice(ls[i], d->left,
rset, NULL ,
d, &xtp->set, pex);
RECOUNT({xtp->set.recount += (w_count_t)ls[i]->recount * rset->recount;})
}
}
}
}
if (Rmatch && (ls_exists || (r_bnl > 0)))
{
Parse_set *rs[4] = { NULL };
bool rs_exists = smk_parse_set(mchxt, ctxt, rcount,
w, rw, d->right, re,
rnull_count, pex, rs);
if (rs_exists)
{
if (le == NULL)
{
Parse_set* lset = mk_parse_set(mchxt, ctxt, r_bnl,
lw, w, le, d->left,
lnull_count, pex);
if (lset != NULL)
{
for (int j = 0; j < 4; j++)
{
if (rs[j] == NULL) continue;
record_choice(lset,
d->left,
rs[j], d->right,
d, &xtp->set, pex);
RECOUNT({xtp->set.recount += lset->recount * rs[j]->recount;})
}
}
}
else
{
for (int i = 0; i < 4; i++)
{
if (ls[i] == NULL) continue;
for (int j = 0; j < 4; j++)
{
if (rs[j] == NULL) continue;
record_choice(ls[i], d->left,
rs[j], d->right,
d, &xtp->set, pex);
RECOUNT({xtp->set.recount += ls[i]->recount * rs[j]->recount;})
}
}
}
}
}
}
}
pop_match_list(mchxt, mlb);
}
return &xtp->set;
}
static bool set_node_overflowed(Parse_set *set)
{
Parse_choice *pc;
w_count_t n = 0;
if (set == NULL || set->first == NULL) return false;
for (pc = set->first; pc != NULL; pc = pc->next)
{
n += (w_count_t)pc->set[0]->count * pc->set[1]->count;
if (PARSE_NUM_OVERFLOW < n) return true;
}
return false;
}
static bool set_overflowed(extractor_t * pex)
{
unsigned int i;
assert(pex->x_table != NULL, "called set_overflowed with x_table==NULL");
for (i=0; i<pex->x_table_size; i++)
{
Pset_bucket *t;
for (t = pex->x_table[i]; t != NULL; t = t->next)
{
if (set_node_overflowed(&t->set)) return true;
}
}
return false;
}
bool build_parse_set(extractor_t* pex, Sentence sent,
fast_matcher_t *mchxt,
count_context_t *ctxt,
unsigned int null_count, Parse_Options opts)
{
pex->words = sent->word;
pex->islands_ok = opts->islands_ok;
pex->parse_set =
mk_parse_set(mchxt, ctxt, -1,
-1, sent->length, NULL, NULL, null_count+1, pex);
return set_overflowed(pex);
}
static Connector *get_tracon_by_id(const Disjunct *d, int32_t tracon_id,
int dir)
{
if (tracon_id < 0) return NULL;
for (Connector *c = dir ? d->right : d->left; c != NULL; c = c->next)
if (tracon_id == c->tracon_id) return c;
assert(0, "tracon_id %d not found on disjunct %p in direction %d\n",
tracon_id, d, dir);
}
static bool is_zero_tracon(Connector *c)
{
return (c == NULL) || (c->tracon_id < NULL_TRACON_BLOCK);
}
static void issue_link(Linkage lkg, int lr, Parse_choice *pc,
const Parse_set *set)
{
Connector *lc = lr ? get_tracon_by_id(pc->md, pc->r_id, 1) : set->le;
if (is_zero_tracon(lc)) return;
lkg->chosen_disjuncts[lr ? pc->set[1]->lw : pc->set[0]->rw] = pc->md;
Connector *rc = lr ? set->re : get_tracon_by_id(pc->md, pc->l_id, 0);
if (is_zero_tracon(rc)) return;
assert(lkg->num_links < lkg->lasz, "Linkage array too small!");
Link *link = &lkg->link_array[lkg->num_links];
link->lw = pc->set[lr]->lw;
link->rw = pc->set[lr]->rw;
link->lc = lc;
link->rc = rc;
lkg->num_links++;
}
static void issue_links_for_choice(Linkage lkg, Parse_choice *pc,
const Parse_set *set)
{
issue_link(lkg, 0, pc, set);
issue_link(lkg, 1, pc, set);
}
static void list_links(Linkage lkg, Parse_set * set, int index)
{
Parse_choice *pc;
count_t n;
assert(set != NULL, "Unexpected NULL Parse_set");
if (set->first == NULL) return;
for (pc = set->first; pc != NULL; pc = pc->next) {
n = pc->set[0]->count * pc->set[1]->count;
if (index < n) break;
index -= n;
}
assert(pc != NULL, "walked off the end in list_links");
issue_links_for_choice(lkg, pc, set);
list_links(lkg, pc->set[0], index % pc->set[0]->count);
list_links(lkg, pc->set[1], index / pc->set[0]->count);
}
static void list_random_links(Linkage lkg, unsigned int *rand_state,
Parse_set * set)
{
assert(set != NULL, "Unexpected NULL Parse_set");
if (set->first == NULL) return;
unsigned int new_index = (set->num_pc == 1) ? 0 :
rand_r(rand_state) % set->num_pc;
Parse_choice *pc;
for (pc = set->first; new_index > 0; pc = pc->next)
new_index--;
issue_links_for_choice(lkg, pc, set);
list_random_links(lkg, rand_state, pc->set[0]);
list_random_links(lkg, rand_state, pc->set[1]);
}
void extract_links(extractor_t * pex, Linkage lkg)
{
int index = lkg->lifo.index;
if (index < 0)
{
bool repeatable = false;
if (0 == pex->rand_state) repeatable = true;
if (repeatable) pex->rand_state = index;
list_random_links(lkg, &pex->rand_state, pex->parse_set);
if (repeatable)
pex->rand_state = 0;
else
lkg->sent->rand_state = pex->rand_state;
}
else {
list_links(lkg, pex->parse_set, index);
}
}
static void mark_used_disjunct(Parse_set *set, bool *disjunct_used)
{
if (set == NULL || set->first == NULL) return;
for (Parse_choice *pc = set->first; pc != NULL; pc = pc->next)
{
if (pc->md->ordinal != -1)
disjunct_used[pc->md->ordinal] = true;
}
}
void mark_used_disjuncts(extractor_t *pex, bool *disjunct_used)
{
assert(pex->x_table != NULL, "x_table==NULL");
for (unsigned int i = 0; i < pex->x_table_size; i++)
{
for (Pset_bucket *t = pex->x_table[i]; t != NULL; t = t->next)
mark_used_disjunct(&t->set, disjunct_used);
}
}
#ifdef PC_DISPLAY
#include "pc-display.c"
#endif