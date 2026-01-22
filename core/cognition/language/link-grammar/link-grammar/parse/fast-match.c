#include "api-structures.h"
#include "connectors.h"
#include "disjunct-utils.h"
#include "fast-match.h"
#include "string-set.h"
#include "dict-common/dict-common.h"
#include "tokenize/word-structures.h"
#include "tokenize/wordgraph.h"
#include "tokenize/tok-structures.h"
#include "utilities.h"
#define D_FAST_MATCHER 9
#define MATCH_LIST_SIZE_INIT 4096
#define MATCH_LIST_SIZE_INC 2
#ifndef ML_COMPAT
#define ML_COMPAT 0
#endif
typedef struct sortbin_s sortbin;
struct sortbin_s
{
Match_node *head;
#if ML_COMPAT
Match_node *tail;
#endif
};
static void push_match_list_element(fast_matcher_t *ctxt, uint16_t id, Disjunct *d)
{
if (ctxt->match_list_end >= ctxt->match_list_size)
{
ctxt->match_list_size *= MATCH_LIST_SIZE_INC;
ctxt->match_list = realloc(ctxt->match_list,
ctxt->match_list_size * sizeof(*ctxt->match_list));
}
#ifdef VERIFY_MATCH_LIST
if (d) d->match_id = id;
#endif
ctxt->match_list[ctxt->match_list_end++] = d;
}
void free_fast_matcher(Sentence sent, fast_matcher_t *mchxt)
{
if (NULL == mchxt) return;
free(mchxt->l_table[0]);
xfree(mchxt->match_list, mchxt->match_list_size * sizeof(*mchxt->match_list));
lgdebug(+6, "Sentence length %zu, match_list_size %zu\n",
mchxt->size, mchxt->match_list_size);
xfree(mchxt->l_table_size, mchxt->size * sizeof(unsigned int));
xfree(mchxt->l_table, mchxt->size * sizeof(Match_node **));
xfree(mchxt, sizeof(fast_matcher_t));
}
static Match_node *match_list_not_found = NULL;
static Match_node **get_match_table_entry(unsigned int size, Match_node **t,
Connector * c, int dir)
{
unsigned int h, s;
s = h = connector_uc_hash(c) & (size-1);
if (dir == 1) {
while (NULL != t[h])
{
if (connector_uc_eq(t[h]->d->right, c)) break;
h = (h + 1) & (size-1);
if (h == s) return &match_list_not_found;
}
}
else
{
while (NULL != t[h])
{
if (connector_uc_eq(t[h]->d->left, c)) break;
h = (h + 1) & (size-1);
if (h == s) return &match_list_not_found;
}
}
return &t[h];
}
static void add_to_table_entry(unsigned int tsize, Match_node **table,
int dir, sortbin *sbin)
{
Match_node *m_next;
for (Match_node *m = sbin->head; NULL != m; m = m_next)
{
Connector *c = (0 == dir) ? m->d->left : m->d->right;
assert(NULL != c, "NULL connector");
Match_node **xl = get_match_table_entry(tsize, table, c, dir);
assert(&match_list_not_found != xl, "get_match_table_entry: Overflow");
m_next = m->next;
m->next = *xl;
*xl = m;
}
}
static void put_into_match_table(unsigned int tsize, Match_node **table,
int w, int dir, sortbin *sbin,
size_t sent_length)
{
if (0 == dir)
{
for (WordIdx sbw = 0; sbw < (WordIdx)w; sbw++)
{
add_to_table_entry(tsize, table, dir, &sbin[sbw]);
}
}
else
{
for (WordIdx sbw = sent_length-1; sbw > (WordIdx)w; sbw--)
{
add_to_table_entry(tsize, table, dir, &sbin[sbw]);
}
}
}
static void clean_sortbin(sortbin *sbin, size_t sent_length)
{
for (unsigned int i = 0; i < sent_length; i++)
sbin[i].head = NULL;
}
static void sort_by_nearest_word(Match_node *m, sortbin *sbin, int nearest_word)
{
sbin = &sbin[nearest_word];
#if ML_COMPAT
if (NULL == sbin->head)
{
sbin->head = m;
}
else
{
sbin->tail->next = m;
}
sbin->tail = m;
m->next = NULL;
#else
m->next = sbin->head;
sbin->head = m;
#endif
}
fast_matcher_t* alloc_fast_matcher(const Sentence sent, unsigned int *ncu[])
{
assert(sent->length > 0, "Sentence length is 0");
fast_matcher_t *ctxt;
ctxt = (fast_matcher_t *) xalloc(sizeof(fast_matcher_t));
ctxt->size = sent->length;
ctxt->l_table_size = xalloc(2 * sent->length * sizeof(unsigned int));
ctxt->r_table_size = ctxt->l_table_size + sent->length;
ctxt->l_table = xalloc(2 * sent->length * sizeof(Match_node **));
ctxt->r_table = ctxt->l_table + sent->length;
memset(ctxt->l_table, 0, 2 * sent->length * sizeof(Match_node **));
ctxt->match_list_size = MATCH_LIST_SIZE_INIT;
ctxt->match_list = xalloc(ctxt->match_list_size * sizeof(*ctxt->match_list));
ctxt->match_list_end = 0;
if (NULL != sent->Match_node_pool)
{
pool_reuse(sent->Match_node_pool);
}
else
{
sent->Match_node_pool =
pool_new(__func__, "Match_node",
2048, sizeof(Match_node),
false, true, false);
}
sortbin *sbin = alloca(sent->length * sizeof(sortbin));
unsigned int num_headers = 0;
Match_node **memblock_headers;
Match_node **hash_table_header;
for (WordIdx w = 0; w < sent->length; w++)
{
for (int dir = 0; dir < 2; dir++)
{
unsigned int tsize;
unsigned int n = ncu[dir][w];
if (0 == n)
{
tsize = 1;
}
else
{
tsize = next_power_of_two_up(3 * n);
}
ncu[dir][w] = tsize;
num_headers += tsize;
}
}
memblock_headers = malloc(num_headers * sizeof(Match_node *));
memset(memblock_headers, 0, num_headers * sizeof(Match_node *));
hash_table_header = memblock_headers;
for (WordIdx w = 0; w < sent->length; w++)
{
clean_sortbin(sbin, sent->length);
for (Disjunct *d = sent->word[w].d; NULL != d; d = d->next)
{
if (d->left != NULL)
{
Match_node *m = pool_alloc(sent->Match_node_pool);
m->d = d;
sort_by_nearest_word(m, sbin, d->left->nearest_word);
}
}
for (Disjunct *d = sent->word[w].d; NULL != d; d = d->next)
{
if (d->right != NULL)
{
Match_node *m = pool_alloc(sent->Match_node_pool);
m->d = d;
sort_by_nearest_word(m, sbin, d->right->nearest_word);
}
}
for (int dir = 0; dir < 2; dir++)
{
unsigned int tsize = ncu[dir][w];
Match_node **t = hash_table_header;
hash_table_header += tsize;
if (0 == dir)
{
ctxt->l_table[w] = t;
ctxt->l_table_size[w] = tsize;
}
else
{
ctxt->r_table[w] = t;
ctxt->r_table_size[w] = tsize;
}
put_into_match_table(tsize, t, w, dir, sbin, sent->length);
}
}
assert(memblock_headers + num_headers == hash_table_header,
"Mismatch header sizes");
return ctxt;
}
#if 0
static void match_stats(Connector *c1, Connector *c2)
{
if (NULL == c1) printf("match_stats: cache\n");
if (NULL == c2) return;
if ((1 == c1->uc_start) && (1 == c2->uc_start) &&
(c1->string[0] == c2->string[0]))
{
printf("match_stats: h/d mismatch\n");
}
if (0 == c1->lc_start) printf("match_stats: no lc (c1)\n");
if (0 == c2->lc_start) printf("match_stats: no lc (c2)\n");
if (string_set_cmp(c1->string, c2->string)) printf("match_stats: same\n");
const char *a = &c1->string[c1->lc_start];
const char *b = &c2->string[c2->lc_start];
do
{
if (*a != *b && (*a != '*') && (*b != '*')) printf("match_stats: lc false\n");
a++;
b++;
} while (*a != '\0' && *b != '\0');
printf("match_stats: lc true\n");
}
#else
#define match_stats(a, b)
#endif
#ifdef DEBUG
#undef N
#define N(c) (c?connector_string(c):"")
static void print_match_list(fast_matcher_t *ctxt, uint16_t id, size_t mlb, int w,
Connector *lc, int lw,
Connector *rc, int rw,
match_list_cache *mlcl,
match_list_cache *mlcr)
{
if (!verbosity_level(D_FAST_MATCHER)) return;
Disjunct **m = &ctxt->match_list[mlb];
for (; NULL != *m; m++)
{
Disjunct *d = *m;
prt_error("MATCH_NODE %c%c %5d: %02d>%-9s %c %9s<%02d>%-9s %c %9s<%02d\n",
(mlcl == NULL) ? ' ' : 'L', (mlcr == NULL) ? ' ' : 'R',
id, lw , N(lc), d->match_left ? '=': ' ',
N(d->left), w, N(d->right),
d->match_right? '=' : ' ', N(rc), rw);
}
}
#else
#define print_match_list(...)
#endif
typedef struct
{
const condesc_t *desc;
bool match;
} match_cache;
static bool do_match_with_cache(Connector *a, Connector *b, match_cache *c_con)
{
match_stats(c_con->string == a->string ? NULL : a, NULL);
UNREACHABLE(connector_desc(a) == NULL);
if (c_con->desc == connector_desc(a))
{
PRAGMA_MAYBE_UNINITIALIZED
return c_con->match;
PRAGMA_END
}
c_con->match = lc_easy_match(connector_desc(a), connector_desc(b));
c_con->desc = connector_desc(a);
return c_con->match;
}
typedef struct
{
const Gword *gword;
bool same_alternative;
} gword_cache;
#define OPTIMIZE_EN
static bool alt_connection_possible(Connector *c1, Connector *c2,
gword_cache *c_con)
{
bool same_alternative = false;
#ifdef OPTIMIZE_EN
if ((c2->originating_gword->o_gword->hier_depth == 0) ||
(c1->originating_gword->o_gword->hier_depth == 0))
{
return true;
}
#endif
if (c1->originating_gword->o_gword == c_con->gword)
return c_con->same_alternative;
for (const gword_set *ga = c1->originating_gword; NULL != ga; ga = ga->next)
{
for (const gword_set *gb = c2->originating_gword; NULL != gb; gb = gb->next)
{
if (in_same_alternative(ga->o_gword, gb->o_gword))
{
same_alternative = true;
break;
}
}
if (same_alternative) break;
}
c_con->same_alternative = same_alternative;
c_con->gword = c1->originating_gword->o_gword;
return same_alternative;
}
static size_t terminate_match_list(fast_matcher_t *ctxt, uint16_t id,
size_t ml_start, int w,
Connector *lc, int lw,
Connector *rc, int rw,
match_list_cache *mlcl,
match_list_cache *mlcr)
{
push_match_list_element(ctxt, 0, NULL);
print_match_list(ctxt, id, ml_start, w, lc, lw, rc, rw, mlcl, mlcr);
return ml_start;
}
size_t
form_match_list(fast_matcher_t *ctxt, int w,
Connector *lc, int lw,
Connector *rc, int rw,
match_list_cache *mlcl, match_list_cache *mlcr)
{
Match_node *mx, *mr_end;
size_t front = get_match_list_position(ctxt);
Match_node *ml = NULL, *mr = NULL;
match_list_cache *cmx;
match_cache mc;
gword_cache gc = { .same_alternative = false };
if (mlcl == NULL)
{
if ((lc != NULL) )
{
ml = *get_match_table_entry(ctxt->l_table_size[w], ctxt->l_table[w], lc, 0);
}
if ((lc != NULL) && (ml == NULL))
return terminate_match_list(ctxt, -1, front, w, lc, lw, rc, rw, mlcl, mlcr);
}
if (mlcr == NULL)
{
if ((rc != NULL) && (w >= rc->farthest_word))
{
mr = *get_match_table_entry(ctxt->r_table_size[w], ctxt->r_table[w], rc, 1);
}
if ((ml == NULL) && (mlcl == NULL) && (mr == NULL))
return terminate_match_list(ctxt, -2, front, w, lc, lw, rc, rw, mlcl, mlcr);
}
#ifdef VERIFY_MATCH_LIST
static _Atomic(uint16_t) id = 0;
const uint16_t lid = ++id;
#else
const uint16_t lid = 0;
#endif
lgdebug(+D_FAST_MATCHER, "MATCH_LIST %c%c %5d mlb %zu\n",
(mlcl == NULL) ? ' ' : 'L', (mlcr == NULL) ? ' ' : 'R', lid, front);
if (mlcr == NULL)
{
for (mx = mr; mx != NULL; mx = mx->next)
{
if (mx->d->right->nearest_word > rw) break;
mx->d->match_left = false;
}
mr_end = mx;
}
else
{
for (cmx = mlcr; cmx->d != NULL; cmx++)
{
cmx->d->match_left = false;
}
mr_end = NULL;
}
if (mlcl == NULL)
{
mc.desc = NULL;
gc.gword = NULL;
for (mx = ml; mx != NULL; mx = mx->next)
{
if (mx->d->left->nearest_word < lw) break;
if (lw < mx->d->left->farthest_word) continue;
mx->d->match_left = do_match_with_cache(mx->d->left, lc, &mc) &&
alt_connection_possible(mx->d->left, lc, &gc);
if (!mx->d->match_left) continue;
mx->d->match_right = false;
push_match_list_element(ctxt, lid, mx->d);
}
if ((lc != NULL) && is_no_match_list(ctxt, front))
return terminate_match_list(ctxt, -3, front, w, lc, lw, rc, rw, mlcl, mlcr);
}
else
{
for (cmx = mlcl; cmx->d != NULL; cmx++)
{
cmx->d->match_left = true;
cmx->d->match_right = false;
push_match_list_element(ctxt, lid, cmx->d);
}
}
if (mlcr == NULL)
{
mc.desc = NULL;
gc.gword = NULL;
for (mx = mr; mx != mr_end; mx = mx->next)
{
if (rw > mx->d->right->farthest_word) continue;
if ((lc != NULL) && !mx->d->match_left) continue;
mx->d->match_right = do_match_with_cache(mx->d->right, rc, &mc) &&
alt_connection_possible(mx->d->right, rc, &gc);
if (!mx->d->match_right || mx->d->match_left) continue;
push_match_list_element(ctxt, lid, mx->d);
}
}
else
{
for (cmx = mlcr; cmx->d != NULL; cmx++)
{
if ((lc != NULL) && !cmx->d->match_left) continue;
cmx->d->match_right = true;
cmx->d->rcount_index = (uint32_t)(cmx - mlcr);
if (cmx->d->match_left) continue;
push_match_list_element(ctxt, lid, cmx->d);
}
}
return terminate_match_list(ctxt, lid, front, w, lc, lw, rc, rw, mlcl, mlcr);
}