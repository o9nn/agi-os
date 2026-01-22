#include <limits.h>
#include <inttypes.h>
#if HAVE_THREADS_H && !__EMSCRIPTEN__
#include <threads.h>
#endif
#include "link-includes.h"
#include "api-structures.h"
#include "connectors.h"
#include "count.h"
#include "dict-common/dict-common.h"
#include "disjunct-utils.h"
#include "fast-match.h"
#include "resources.h"
#include "tokenize/word-structures.h"
#include "utilities.h"
#define D_COUNT 5
typedef uint8_t null_count_m;
typedef uint8_t WordIdx_m;
const bool ENABLE_WORD_SKIP_VECTOR = true;
const bool ENABLE_MATCH_LIST_CACHE = true;
const bool ENABLE_TABLE_LRCNT = true;
const bool USE_TABLE_TRACON = true;
const bool USE_PSEUDOCOUNT = true;
typedef struct Table_tracon_s Table_tracon;
struct Table_tracon_s
{
Table_tracon     *next;
int              l_id, r_id;
Count_bin        count;
null_count_m     null_count;
size_t           hash;
};
typedef struct
{
match_list_cache *mlc0;
null_count_m null_count;
int8_t status;
WordIdx_m check_next;
} count_expectation;
static const size_t min_len_word_vector = 10;
#define ANY_NULL_COUNT (MAX_SENTENCE + 1)
#define INCREMENT_WORD ((uint8_t)(MAX_SENTENCE+1))
typedef count_expectation *wordvecp;
static count_expectation lrcnt_cache_zero;
#if defined DEBUG || DEBUG_COUNT_COST
#define COUNT_COST(...) __VA_ARGS__
#else
#define COUNT_COST(...)
#endif
typedef struct
{
wordvecp *tracon_wvp;
uint32_t num_tracon_id;
} Table_lrcnt;
struct count_context_s
{
fast_matcher_t *mchxt;
Sentence sent;
bool    islands_ok;
bool    exhausted;
uint8_t num_growth;
bool    is_short;
uint32_t checktimer;
size_t table_size;
size_t table_mask;
size_t table_available_count;
Table_tracon ** table;
Table_lrcnt table_lrcnt[2];
Pool_desc *mlc_pool;
Resources current_resources;
COUNT_COST(uint64_t count_cost[3];)
};
#define INV_LOAD_FACTOR 3
#define MAX_LOG2_TABLE_SIZE ((sizeof(size_t)==4) ? 25 : 34)
static size_t estimate_tracon_entries(Sentence sent)
{
unsigned int nwords = sent->length;
unsigned int log2_nwords = 0;
while (nwords) { log2_nwords++; nwords >>= 1; }
size_t tblsize = 3 * log2_nwords * sent->num_disjuncts;
if (tblsize < 512) tblsize = 512;
return tblsize;
}
#if HAVE_THREADS_H && !__EMSCRIPTEN__
static void free_tls_table(void* ptr_to_table)
{
if (NULL == ptr_to_table) return;
Table_tracon** kept_table = *((Table_tracon***)ptr_to_table);
if (NULL == kept_table) return;
free(kept_table);
*((Table_tracon***) ptr_to_table) = NULL;
}
static tss_t key;
static void make_key(void)
{
tss_create(&key, free_tls_table);
}
#endif
static void table_alloc(count_context_t *ctxt, unsigned int logsz)
{
static TLS Table_tracon **kept_table = NULL;
static TLS size_t kept_table_size = 0;
size_t reqsz = 1ULL << logsz;
if (0 < logsz && reqsz <= ctxt->table_size) return;
#if HAVE_THREADS_H && !__EMSCRIPTEN__
static once_flag flag = ONCE_FLAG_INIT;
call_once(&flag, make_key);
if (NULL == kept_table)
tss_set(key, &kept_table);
#endif
if (logsz == 0)
ctxt->table_size *= 2;
else
ctxt->table_size = reqsz;
if ((1ULL << MAX_LOG2_TABLE_SIZE) <= ctxt->table_size)
ctxt->table_size  = (1ULL << MAX_LOG2_TABLE_SIZE);
lgdebug(+D_COUNT, "Tracon table size %lu\n", ctxt->table_size);
if (kept_table_size < ctxt->table_size)
{
kept_table_size = ctxt->table_size;
if (kept_table) free(kept_table);
kept_table = malloc(sizeof(Table_tracon *) * ctxt->table_size);
}
ctxt->table = kept_table;
memset(ctxt->table, 0, sizeof(Table_tracon *) * ctxt->table_size);
ctxt->table_mask = ctxt->table_size - 1;
ctxt->table_available_count = ctxt->table_size / INV_LOAD_FACTOR;
}
static void init_table(count_context_t *ctxt)
{
size_t tblsz = estimate_tracon_entries(ctxt->sent);
tblsz *= INV_LOAD_FACTOR;
unsigned int logsz = 0;
while (tblsz) { logsz++; tblsz >>= 1; }
table_alloc(ctxt, logsz);
}
static void free_table_lrcnt(count_context_t *ctxt)
{
if (ctxt->is_short) return;
if (verbosity_level(D_COUNT))
{
unsigned int nonzero = 0, any_null = 0, zero = 0, non_max_null = 0;
unsigned int cml = 0, cml_disjunct = 0;
Pool_location loc = { 0 };
wordvecp t;
while ((t = pool_next(ctxt->sent->wordvec_pool, &loc)) != NULL)
{
if (t->status == -1) continue;
if (t->status == 1)
{
nonzero++;
if (t->mlc0 != NULL)
{
cml++;
for (Disjunct *d = t->mlc0->d; d != NULL; d++)
cml_disjunct++;
}
}
else if (t->null_count == ANY_NULL_COUNT)
any_null++;
else if (ctxt->sent->null_count > t->null_count)
non_max_null++;
else if (ctxt->sent->null_count == t->null_count)
zero++;
}
const unsigned int num_values =
pool_num_elements_issued(ctxt->sent->wordvec_pool);
lgdebug(+0, "Values %u (usage = non_max_null %u + other %u, "
"other = any_null_zero %u + zero %u + nonzero %u); "
"%u disjuncts in %u cache entries\n",
num_values, non_max_null, num_values-non_max_null,
any_null, zero, nonzero, cml_disjunct, cml);
for (unsigned int dir = 0; dir < 2; dir++)
{
unsigned int table_usage = 0;
for (size_t i = 0; i < ctxt->table_lrcnt[dir].num_tracon_id; i++)
{
if (ctxt->table_lrcnt[dir].tracon_wvp[i] != NULL) continue;
table_usage++;
}
lgdebug(+0, "Direction %u: Using %u/%u tracons %.2f%%\n\\",
dir, table_usage, ctxt->table_lrcnt[dir].num_tracon_id,
100.0f*table_usage / ctxt->table_lrcnt[dir].num_tracon_id);
}
}
pool_delete(ctxt->mlc_pool);
for (unsigned int dir = 0; dir < 2; dir++)
{
free(ctxt->table_lrcnt[dir].tracon_wvp);
ctxt->table_lrcnt[dir].tracon_wvp = NULL;
}
}
static size_t match_list_pool_size_estimate(Sentence sent)
{
size_t expsz = pool_num_elements_issued(sent->Exp_pool);
size_t mlpse = 2 * expsz;
if (mlpse < 4090) mlpse = 4090;
size_t maxndj = 0;
for (WordIdx w = 0; w < sent->length; w++)
if (maxndj < sent->word[w].num_disjuncts)
maxndj = sent->word[w].num_disjuncts;
if (512*1024 < maxndj) maxndj = 512*1024;
if (mlpse < maxndj) mlpse = maxndj;
return mlpse;
}
static void init_table_lrcnt(count_context_t *ctxt)
{
Sentence sent = ctxt->sent;
for (unsigned int dir = 0; dir < 2; dir++)
{
const size_t sz = sizeof(wordvecp) * ctxt->table_lrcnt[dir].num_tracon_id;
ctxt->table_lrcnt[dir].tracon_wvp = malloc(sz);
memset(ctxt->table_lrcnt[dir].tracon_wvp, 0, sz);
}
const size_t initial_size = MIN(sent->length/2, 16) *
(ctxt->table_lrcnt[0].num_tracon_id + ctxt->table_lrcnt[1].num_tracon_id);
if (NULL != sent->wordvec_pool)
{
pool_reuse(sent->wordvec_pool);
}
else
{
ctxt->sent->wordvec_pool =
pool_new(__func__, "count_expectation", initial_size,
sizeof(count_expectation), true,
false, false);
}
const size_t match_list_pool_size = match_list_pool_size_estimate(sent);
ctxt->mlc_pool =
pool_new(__func__, "Match list cache",
match_list_pool_size, sizeof(match_list_cache),
false, false, false);
}
#ifdef DEBUG
#define DEBUG_TABLE_STAT
#endif
#ifdef DEBUG_TABLE_STAT
static uint64_t hit, miss;
#define TABLE_STAT(...) __VA_ARGS__
#else
#define TABLE_STAT(...)
#endif
static void table_stat(count_context_t *ctxt)
{
#ifdef DEBUG_TABLE_STAT
if (!verbosity_level(+D_COUNT)) return;
size_t z = 0, nz = 0;
size_t c, total_c = 0;
size_t N = 0;
size_t null_count[256] = { 0 };
int chain_length[64] = { 0 };
bool table_stat_entries = test_enabled("count-table-entries");
for (size_t i = 0; i < ctxt->table_size; i++)
{
Table_tracon *t = ctxt->table[i];
c = 0;
if (t == NULL)
{
N++;
}
else
{
assert(t->hash != 0, "Invalid hash value: 0");
assert((hist_total(&t->count)>=0)&&(hist_total(&t->count) <= INT_MAX),
"Invalid count %"COUNT_FMT, hist_total(&t->count));
assert((ctxt->table_lrcnt[0].num_tracon_id == 0) ||
t->l_id < (int)ctxt->sent->length ||
((t->l_id >= 255)&&(t->l_id < (int)ctxt->table_lrcnt[0].num_tracon_id)),
"invalid l_id %d", t->l_id);
assert((ctxt->table_lrcnt[1].num_tracon_id == 0) ||
t->r_id <= (int)ctxt->sent->length ||
((t->r_id > 255)&&(t->r_id < (int)ctxt->table_lrcnt[1].num_tracon_id)),
"invalid r_id %d", t->r_id);
}
for (; t != NULL; t = t->next)
{
c++;
if (hist_total(&t->count) == 0)
z++;
else
nz++;
null_count[t->null_count]++;
}
if (c > 0)
{
chain_length[c >= (int)ARRAY_SIZE(chain_length) ? 0 : c]++;
total_c += c;
}
}
size_t used_slots = ctxt->table_size-N;
unsigned int logsz = 0;
size_t tblsz = ctxt->table_size;
while (tblsz) { logsz++; tblsz >>= 1; }
printf("Connector table: num_growth=%u msb=%u slots=%6zu/%6zu (%5.2f%%) "
"avg-chain=%4.2f values=%6zu (z=%5zu nz=%5zu N=%5zu) used=%5.2f%% "
"acc=%"PRIu64" (hit=%"PRIu64" miss=%"PRIu64") (sent_len=%zu dis=%u)\n",
ctxt->num_growth, logsz, used_slots, ctxt->table_size,
100.0f*used_slots/ctxt->table_size, 1.0f*total_c/used_slots,
z+nz, z, nz, N, 100.0f*(z+nz)/ctxt->table_size,
hit+miss, hit, miss, ctxt->sent->length, ctxt->sent->num_disjuncts);
printf("Chain length:\n");
for (size_t i = 1; i < ARRAY_SIZE(chain_length); i++)
if (chain_length[i] > 0) printf("%zu: %d\n", i, chain_length[i]);
if (chain_length[0] > 0) printf(">63: %d\n", chain_length[0]);
if (!((null_count[1] == 1) && (null_count[2] == 0)))
{
printf("Null count:\n");
for (unsigned int nc = 0; nc < ARRAY_SIZE(null_count); nc++)
{
if (0 != null_count[nc])
printf("%u: %zu\n", nc, null_count[nc]);
}
}
if (table_stat_entries)
{
for (unsigned int nc = 0; nc < ARRAY_SIZE(null_count); nc++)
{
if (0 == null_count[nc]) continue;
printf("Null count %u:\n", nc);
for (size_t i = 0; i < ctxt->table_size; i++)
{
for (Table_tracon *t = ctxt->table[i]; t != NULL; t = t->next)
{
if (t->null_count != nc) continue;
int n = printf("[%zu]", i);
printf("%*d %5d c=%"COUNT_FMT"\n",  15-n, t->l_id, t->r_id, t->count);
}
}
}
}
hit = miss = 0;
#endif
}
static void table_grow(count_context_t *ctxt)
{
if ((1ULL << MAX_LOG2_TABLE_SIZE) <= ctxt->table_size)
{
ctxt->table_available_count = UINT_MAX;
return;
}
table_alloc(ctxt, 0);
Table_tracon *oe;
Pool_location loc = { 0 };
while ((oe = pool_next(ctxt->sent->Table_tracon_pool, &loc)) != NULL)
{
size_t ni = oe->hash & ctxt->table_mask;
if (ctxt->table[ni] == NULL) ctxt->table_available_count--;
oe->next = ctxt->table[ni];
ctxt->table[ni] = oe;
}
ctxt->num_growth++;
}
static Count_bin table_store(count_context_t *ctxt,
int lw, int rw,
const Connector *le, const Connector *re,
unsigned int null_count,
size_t hash, w_Count_bin c)
{
if (ctxt->table_available_count == 0) table_grow(ctxt);
int l_id = (NULL != le) ? le->tracon_id : lw;
int r_id = (NULL != re) ? re->tracon_id : rw;
if (!USE_TABLE_TRACON)
{
Count_bin *e = table_lookup(ctxt, lw, rw, le, re, null_count, NULL);
if (e != NULL)
{
assert((hist_total(&c) == hist_total(e)),
"Inconsistent count for w(%d,%d) tracon_id(%d,%d): %zd != %zd",
lw, rw, l_id, r_id, (ssize_t)hist_total(&c), (ssize_t)hist_total(e));
return *e;
}
}
size_t i = hash & ctxt->table_mask;
Table_tracon *n = pool_alloc(ctxt->sent->Table_tracon_pool);
if (ctxt->table[i] == NULL)
ctxt->table_available_count--;
n->l_id = l_id;
n->r_id = r_id;
n->null_count = null_count;
n->next = ctxt->table[i];
n->count = (Count_bin)c;
n->hash = hash;
ctxt->table[i] = n;
return n->count;
}
inline Count_bin *
table_lookup(count_context_t *ctxt, int lw, int rw,
const Connector *le, const Connector *re,
unsigned int null_count, size_t *hash)
{
int l_id = (NULL != le) ? le->tracon_id : lw;
int r_id = (NULL != re) ? re->tracon_id : rw;
size_t h = pair_hash(lw, rw, l_id, r_id, null_count);
Table_tracon *t = ctxt->table[h & ctxt->table_mask];
if (!USE_TABLE_TRACON && (hash != NULL))
{
*hash = h;
return NULL;
}
for (; t != NULL; t = t->next)
{
if ((t->l_id == l_id) && (t->r_id == r_id) &&
(t->null_count == null_count))
{
TABLE_STAT(hit++);
return &t->count;
}
}
TABLE_STAT(miss++);
if (hash != NULL) *hash = h;
TABLE_STAT(miss++);
return NULL;
}
extern  Count_bin *
table_lookup(count_context_t *, int, int,
const Connector *, const Connector *,
unsigned int, size_t *);
static void generate_word_skip_vector(count_context_t *ctxt, wordvecp wv,
Connector *le, Connector *re,
int start_word, int end_word,
int lw, int rw)
{
if (!ENABLE_WORD_SKIP_VECTOR) return;
if (le != NULL)
{
int check_word = start_word;
int i;
if (wv == NULL) wv = ctxt->table_lrcnt[0].tracon_wvp[le->tracon_id];
unsigned int sent_nc = ctxt->sent->null_count;
for (i = start_word + 1; i < end_word; i++)
{
wordvecp e = &wv[i - le->nearest_word];
e->check_next = INCREMENT_WORD;
if((e->status != 0) || (sent_nc > e->null_count))
{
wv[check_word - le->nearest_word].check_next = i;
check_word = i;
}
}
if (check_word <= end_word - 1)
wv[check_word - le->nearest_word].check_next = end_word;
#if 0
printf("id %d w(%3d, %3d), se(%3d, %3d) sent_nc %u size %d\n",
le->tracon_id, lw, rw, start_word, end_word,
ctxt->sent->null_count, le->farthest_word-le->nearest_word+1);
for (i = start_word; i < end_word; i++)
{
Table_lrcnt *e = &wv[i - le->nearest_word];
printf("\tw%-3d idx %-3d status %d nc %-3u next %d\n",
i,  i - le->nearest_word, e->status, e->null_count,
e->check_next);
}
#endif
}
else
{
int check_word = start_word;
int i;
if (wv == NULL) wv = ctxt->table_lrcnt[1].tracon_wvp[re->tracon_id];
unsigned int sent_nc = ctxt->sent->null_count;
for (i = start_word + 1; i < end_word; i++)
{
wordvecp e = &wv[i - re->farthest_word];
e->check_next = INCREMENT_WORD;
if((e->status != 0) || (sent_nc > e->null_count))
{
wv[check_word - re->farthest_word].check_next = i;
check_word = i;
}
}
if (check_word <= end_word - 1)
wv[check_word - re->farthest_word].check_next = end_word;
#if 0
printf("id %d w(%3d, %3d), se(%3d, %3d) sent_nc %u size %d\n",
le->tracon_id, lw, rw, start_word, end_word,
ctxt->sent->null_count, re->nearest_word-re->farthest_word+1);
for (i = start_word; i < end_word; i++)
{
Table_lrcnt *e = &wv[i - re->farthest_word];
printf("\tw%-3d idx %-3d status %d nc %-3u next %d\n",
i,  i - re->farthest_word, e->status, e->null_count,
e->check_next);
}
#endif
}
}
static bool parse_count_clamp(w_Count_bin *total)
{
if (INT_MAX < hist_total(total))
{
#if PERFORM_COUNT_HISTOGRAMMING
total->total = INT_MAX;
#else
*total = INT_MAX;
#endif
return true;
}
return false;
}
static void lrcnt_keep_count(wordvecp lrcnt_cache, bool dir, Disjunct *d,
w_Count_bin leftcount, w_Count_bin rightcount)
{
#if PERFORM_COUNT_HISTOGRAMMING
return;
#endif
w_Count_bin count = dir ? rightcount : leftcount;
parse_count_clamp(&count);
d->lrcount = (Count_bin)count;
}
static void lrcnt_cache_match_list(wordvecp lrcnt_cache, count_context_t *ctxt,
size_t mlb, bool dir)
{
size_t dcnt = 0;
size_t i  = 0;
fast_matcher_t *mchxt = ctxt->mchxt;
if (!ENABLE_MATCH_LIST_CACHE) return;
for (i = mlb; get_match_list_element(mchxt, i) != NULL; i++)
{
Disjunct *d = get_match_list_element(mchxt, i);
dcnt += (int)(dir ? d->match_right : d->match_left);
}
dassert(dcnt > 0, "No disjuncts to cache");
#ifdef VERIFY_MATCH_LIST
lgdebug(+9, "MATCH_LIST %9d dir=%d mlb %zu cached %zu/%zu\n",
get_match_list_element(mchxt, mlb)->match_id, dir, mlb, dcnt, i-mlb);
#endif
match_list_cache *ml = pool_alloc_vec(ctxt->mlc_pool, dcnt + 1);
if (ml == NULL) return;
dcnt = 0;
for (i = mlb; get_match_list_element(mchxt, i) != NULL; i++)
{
Disjunct *d = get_match_list_element(mchxt, i);
if ((dir == 0) ? d->match_left : d->match_right)
{
ml[dcnt].d = d;
assert(d->lrcount > 0, "Invalid linkage count %d", d->lrcount);
ml[dcnt].count = d->lrcount;
dcnt++;
}
}
ml[dcnt].d = NULL;
lrcnt_cache->mlc0 = ml;
}
static wordvecp lrcnt_check(wordvecp wvp, unsigned int null_count,
unsigned int *null_start)
{
if (wvp->status == -1)
{
if (null_start != NULL) *null_start = 0;
return wvp;
}
if  (wvp->status == 1)
{
if (null_start != NULL)
*null_start = (null_count_m)(wvp->null_count + 1);
return NULL;
}
if (null_count <= wvp->null_count)
{
return &lrcnt_cache_zero;
}
if (null_start == NULL) return NULL;
*null_start = wvp->null_count + 1;
return wvp;
}
static wordvecp *get_lrcnt_wvpa(count_context_t *ctxt, Connector *le,
Connector *re)
{
if (ctxt->is_short) return NULL;
int dir = (int)(le == NULL);
int tracon_id = (le == NULL) ? re->tracon_id : le->tracon_id;
return &ctxt->table_lrcnt[dir].tracon_wvp[tracon_id];
}
static wordvecp alloc_lrcnt_wv(count_context_t *ctxt, wordvecp *wvp,
Connector *le, Connector *re)
{
if (*wvp == NULL)
{
Connector *c = (le == NULL) ? re : le;
const size_t wordvec_size = abs(c->farthest_word - c->nearest_word) + 1;
*wvp = pool_alloc_vec(ctxt->sent->wordvec_pool, wordvec_size);
for (size_t i = 0; i < wordvec_size; i++)
{
(*wvp)[i].status = -1;
(*wvp)[i].null_count = -1;
(*wvp)[i].check_next = -1;
}
}
return *wvp;
}
bool no_count(count_context_t *ctxt, int dir, Connector *c,
unsigned int wordvec_index, unsigned int null_count)
{
if (ctxt->is_short) return false;
wordvecp wvp = ctxt->table_lrcnt[dir].tracon_wvp[c->tracon_id];
if (wvp == NULL) return false;
wordvecp lrcnt_cache = &wvp[wordvec_index];
return (lrcnt_check(lrcnt_cache, null_count, NULL) == &lrcnt_cache_zero);
}
match_list_cache *get_cached_match_list(count_context_t *ctxt, int dir, int w,
Connector *c)
{
if (ctxt->sent->null_count != 0) return NULL;
if (ctxt->is_short) return NULL;
wordvecp wv = ctxt->table_lrcnt[dir].tracon_wvp[c->tracon_id];
if (wv == NULL) return NULL;
return wv[w - ((dir == 0) ? c->nearest_word : c->farthest_word)].mlc0;
}
static bool lrcnt_expectation_update(wordvecp wv, bool lrcnt_found,
bool match_list, unsigned int null_count)
{
bool lrcnt_status_changed = (wv->status != (int)lrcnt_found);
unsigned int prev_null_count = wv->null_count;
if (!lrcnt_found)
wv->null_count = match_list ? null_count : ANY_NULL_COUNT;
wv->status = (int)lrcnt_found;
return lrcnt_status_changed || (prev_null_count != wv->null_count);
}
static bool is_panic(count_context_t *ctxt)
{
if (ctxt->exhausted) return true;
ctxt->checktimer++;
if (((0 == ctxt->checktimer%(1<<18)) && (ctxt->current_resources != NULL) &&
resources_exhausted(ctxt->current_resources)))
{
ctxt->exhausted = true;
return true;
}
return false;
}
#define NO_COUNT -1
#if PERFORM_COUNT_HISTOGRAMMING
#define INIT_NO_COUNT (Count_bin){.total = NO_COUNT}
#else
#define INIT_NO_COUNT NO_COUNT
#endif
Count_bin count_unknown = INIT_NO_COUNT;
static Count_bin table_count(count_context_t * ctxt,
int lw, int rw, Connector *le, Connector *re,
unsigned int null_count)
{
if (!USE_TABLE_TRACON) return count_unknown;
if ((le != NULL) && (re != NULL) && (le->nearest_word > re->nearest_word))
return hist_zero();
Count_bin *count = table_lookup(ctxt, lw, rw, le, re, null_count, NULL);
if (NULL == count) return count_unknown;
return *count;
}
#ifdef USE_PSEUDOCOUNT
static bool pseudocount(count_context_t * ctxt, Count_bin *count,
int lw, int rw, Connector *le, Connector *re,
unsigned int null_count)
{
count[0] = table_count(ctxt, lw, rw, le->next, re->next, null_count);
if (hist_total(&count[0]) != 0) return true;
if (le->multi)
{
count[1] = table_count(ctxt, lw, rw, le, re->next, null_count);
if (hist_total(&count[1]) != 0) return true;
}
if (re->multi)
{
count[2] = table_count(ctxt, lw, rw, le->next, re, null_count);
if (hist_total(&count[2]) != 0) return true;;
}
if (le->multi && re->multi)
{
count[3] = table_count(ctxt, lw, rw, le, re, null_count);
if (hist_total(&count[3]) != 0) return true;;
}
return false;
}
#endif
static int num_optional_words(count_context_t *ctxt, int w1, int w2)
{
int n = 0;
for (int w = w1+1; w < w2; w++)
if (ctxt->sent->word[w].optional) n++;
return n;
}
#define CACHE_COUNT(c, how_to_count, do_count) \
{ \
Count_bin count = (hist_total(&c) == NO_COUNT) ? \
TRACE_LABEL(c, do_count) : c; \
how_to_count; \
}
#ifdef DEBUG
#define DO_COUNT_TRACE
#endif
#ifdef DO_COUNT_TRACE
#define D_COUNT_TRACE 8
#define LBLSZ 11
#define TRACE_LABEL(l, do_count) \
(verbosity_level(D_COUNT_TRACE, "do_count") ? \
prt_error("%-*s", LBLSZ, STRINGIFY(l)) : 0, do_count)
#else
#define TRACE_LABEL(l, do_count) (do_count)
#endif
static Count_bin do_count(const char dlabel[], count_context_t *ctxt,
int lw, int rw,
Connector *le, Connector *re,
unsigned int null_count);
static w_Count_bin scount(const char dlabel[], count_context_t *ctxt,
Count_bin ccount[4], int lw, int rw,
Connector *le, Connector *re,
unsigned int null_count)
{
w_Count_bin totcount;
CACHE_COUNT(ccount[0], totcount = count,
do_count(dlabel, ctxt, lw, rw, le->next, re->next, null_count));
if (le->multi)
CACHE_COUNT(ccount[1], hist_accumv(&totcount, d->cost, count),
do_count(dlabel, ctxt, lw, rw, le, re->next, null_count));
if (re->multi)
CACHE_COUNT(ccount[2], hist_accumv(&totcount, d->cost, count),
do_count(dlabel, ctxt, lw, rw, le->next, re, null_count));
if (re->multi && le->multi)
CACHE_COUNT(ccount[3], hist_accumv(&totcount, d->cost, count),
do_count(dlabel, ctxt, lw, rw, le, re, null_count));
return totcount;
}
#ifdef DO_COUNT_TRACE
#define V(c) (!c?"(nil)":connector_string(c))
#define ID(c,w) (!c?w:c->tracon_id)
static Count_bin do_count1(const char dlabel[], count_context_t *ctxt,
int lw, int rw,
Connector *le, Connector *re,
unsigned int null_count);
static Count_bin do_count(const char dlabel[], count_context_t *ctxt,
int lw, int rw,
Connector *le, Connector *re,
unsigned int null_count)
{
static int level;
if (!verbosity_level(D_COUNT_TRACE))
return do_count1(dlabel, ctxt, lw, rw, le, re, null_count);
Count_bin *c = table_lookup(ctxt, lw, rw, le, re, null_count, NULL);
char m_result[64] = "";
if (c != NULL)
snprintf(m_result, sizeof(m_result), "(M=%"COUNT_FMT")", hist_total(c));
level++;
prt_error("%*s%s do_count%s:%d lw=%d rw=%d le=%s(%d) re=%s(%d) null_count=%u\n\\",
level*2, "", dlabel, m_result, level, lw, rw, V(le),ID(le,lw), V(re),ID(re,rw), null_count);
Count_bin r = do_count1(dlabel, ctxt, lw, rw, le, re, null_count);
prt_error("%*s%s return%.*s:%d=%"COUNT_FMT"\n",
LBLSZ+level*2, "", dlabel, (!!c)*3, "(M)", level, hist_total(&r));
level--;
return r;
}
#define do_count do_count1
#endif
static Count_bin do_count(const char dlabel[], count_context_t *ctxt,
int lw, int rw,
Connector *le, Connector *re,
unsigned int null_count)
{
#ifdef DO_COUNT_TRACE
#undef do_count
#endif
w_Count_bin total = hist_zero();
int start_word, end_word, w;
if (!valid_nearest_words(le, re, lw, rw)) return hist_zero();
if (is_panic(ctxt)) return hist_zero();
assert (null_count < INT_MAX, "Bad null count %d", (int)null_count);
size_t h = 0;
{
Count_bin* const c = table_lookup(ctxt, lw, rw, le, re, null_count, &h);
if (c != NULL) return *c;
}
unsigned int unparseable_len = rw-lw-1;
#if 1
if (unparseable_len == 0)
{
if ((le == NULL) && (re == NULL) && (null_count == 0))
return table_store(ctxt, lw, rw, le, re, null_count, h, hist_one());
return table_store(ctxt, lw, rw, le, re, null_count, h, hist_zero());
}
#endif
if ((le == NULL) && (re == NULL))
{
int nopt_words = num_optional_words(ctxt, lw, rw);
if ((null_count == 0) ||
(!ctxt->islands_ok && (lw != -1) && (ctxt->sent->word[lw].d != NULL)))
{
if ((null_count <= unparseable_len) &&
(null_count >= unparseable_len - nopt_words))
return table_store(ctxt, lw, rw, le, re, null_count, h, hist_one());
return table_store(ctxt, lw, rw, le, re, null_count, h, hist_zero());
}
w = lw + 1;
for (int opt = 0; opt <= (int)ctxt->sent->word[w].optional; opt++)
{
unsigned int try_null_count = null_count + opt;
for (Disjunct *d = ctxt->sent->word[w].d; d != NULL; d = d->next)
{
if (d->left == NULL)
{
hist_accumv(&total, d->cost,
do_count("I", ctxt, w, rw, d->right, NULL, try_null_count-1));
}
}
hist_accumv(&total, 0.0,
do_count("N", ctxt, w, rw, NULL, NULL, try_null_count-1));
}
if (parse_count_clamp(&total))
{
#if 0
printf("OVERFLOW 1\n");
#endif
}
return table_store(ctxt, lw, rw, le, re, null_count, h, total);
}
if (le == NULL)
{
start_word = MAX(lw+1, re->farthest_word);
}
else
{
start_word = le->nearest_word;
}
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
fast_matcher_t *mchxt = ctxt->mchxt;
bool lrcnt_cache_changed = false;
int next_word = MAX_SENTENCE;
wordvecp wvp = NULL;
int woffset = 0;
if (!ctxt->is_short)
{
wordvecp *wvpa = get_lrcnt_wvpa(ctxt, le, re);
wvp = alloc_lrcnt_wv(ctxt, wvpa, le, re);
woffset = (le == NULL) ? re->farthest_word : le->nearest_word;
}
for (w = start_word; w < end_word; w = next_word)
{
COUNT_COST(ctxt->count_cost[0]++;)
wordvecp lrcnt_cache = NULL;
bool lrcnt_found = false;
bool lcnt_optimize = true;
bool rcnt_optimize = true;
unsigned int lnull_start = 0;
unsigned int lnull_end = null_count;
Connector *fml_re = re;
match_list_cache *mlcl = NULL, *mlcr = NULL;
bool using_cached_match_list = false;
unsigned int lcount_index = 0;
if (ctxt->is_short)
{
next_word = w + 1;
}
else
{
lrcnt_cache = &wvp[w - woffset];
next_word = lrcnt_cache->check_next;
if (next_word == INCREMENT_WORD) next_word = w + 1;
if (le != NULL)
{
lrcnt_cache = lrcnt_check(lrcnt_cache, null_count, &lnull_start);
if (lrcnt_cache == &lrcnt_cache_zero) continue;
if (lrcnt_cache != NULL)
{
lcnt_optimize = false;
}
if ((re != NULL) && (re->farthest_word <= w))
{
if (no_count(ctxt, 1, re, w - re->farthest_word, null_count))
fml_re = NULL;
}
}
else
{
unsigned int rnull_start;
lrcnt_cache = lrcnt_check(lrcnt_cache, null_count, &rnull_start);
if (lrcnt_cache == &lrcnt_cache_zero) continue;
if (lrcnt_cache != NULL)
{
rcnt_optimize = false;
if (rnull_start <= null_count)
lnull_end -= rnull_start;
}
}
if ((lrcnt_cache == NULL) && (ctxt->sent->null_count == 0))
{
using_cached_match_list = true;
if (le != NULL)
{
mlcl = get_cached_match_list(ctxt, 0, w, le);
}
if (fml_re != NULL && ((le == NULL) || (re->farthest_word <= w)))
{
mlcr = get_cached_match_list(ctxt, 1, w, re);
}
}
}
size_t mlb = form_match_list(mchxt, w, le, lw, fml_re, rw, mlcl, mlcr);
#ifdef VERIFY_MATCH_LIST
Disjunct *od = get_match_list_element(mchxt, mlb);
uint16_t id = od ? od->match_id : 0;
#endif
for (size_t mle = mlb; get_match_list_element(mchxt, mle) != NULL; mle++)
{
COUNT_COST(ctxt->count_cost[1]++;)
Disjunct *d = get_match_list_element(mchxt, mle);
#ifdef VERIFY_MATCH_LIST
assert(id == d->match_id, "Modified id (%u!=%u)", id, d->match_id);
#endif
bool Lmatch = d->match_left;
bool Rmatch = d->match_right;
w_Count_bin leftcount = NO_COUNT;
w_Count_bin rightcount = NO_COUNT;
bool leftpcount = false;
bool rightpcount = false;
d->match_left = d->match_right = false;
if (using_cached_match_list)
{
if (Lmatch && (mlcl != NULL))
{
leftpcount = true;
leftcount = mlcl[lcount_index++].count;
}
if (Rmatch && (mlcr != NULL))
{
rightpcount = true;
rightcount = mlcr[d->rcount_index].count;
}
}
for (unsigned int lnull_cnt = lnull_start; lnull_cnt <= lnull_end; lnull_cnt++)
{
COUNT_COST(ctxt->count_cost[2]++;)
int rnull_cnt = null_count - lnull_cnt;
if (!using_cached_match_list)
{
leftcount = NO_COUNT;
rightcount = NO_COUNT;
leftpcount = false;
rightpcount = false;
}
Count_bin l_bnr = INIT_NO_COUNT;
Count_bin r_bnl = (le == NULL) ? INIT_NO_COUNT : hist_zero();
Count_bin lcount[4] = { NO_COUNT, NO_COUNT, NO_COUNT, NO_COUNT };
Count_bin rcount[4] = { NO_COUNT, NO_COUNT, NO_COUNT, NO_COUNT };
#ifdef USE_PSEUDOCOUNT
if (Lmatch && !leftpcount)
{
leftpcount =
pseudocount(ctxt, lcount, lw, w, le, d->left, lnull_cnt);
}
if (Rmatch && !rightpcount && (leftpcount || (le == NULL)))
{
rightpcount =
pseudocount(ctxt, rcount, w, rw, d->right, re, rnull_cnt);
}
#else
leftpcount = Lmatch;
rightpcount = Rmatch;
#endif
if (leftpcount)
{
l_bnr = table_count(ctxt, w, rw, d->right, re, rnull_cnt);
}
else
{
if (!rightpcount) continue;
if (le == NULL)
{
r_bnl = table_count(ctxt, lw, w, le, d->left, lnull_cnt);
}
}
if (leftpcount &&
(!lcnt_optimize || rightpcount || (0 != hist_total(&l_bnr))))
{
if (hist_total(&leftcount) == NO_COUNT)
{
leftcount =
scount("L", ctxt, lcount, lw, w, le, d->left, lnull_cnt);
}
if (0 < hist_total(&leftcount))
{
parse_count_clamp(&leftcount);
lrcnt_found = true;
d->match_left = true;
CACHE_COUNT(l_bnr, hist_muladdv(&total, &leftcount, d->cost, count),
do_count("C", ctxt, w, rw, d->right, re, rnull_cnt));
}
}
if (rightpcount &&
(!rcnt_optimize || (0 < hist_total(&leftcount)) || (0 != hist_total(&r_bnl))))
{
if (hist_total(&rightcount) == NO_COUNT)
{
rightcount =
scount("R", ctxt, rcount, w, rw, d->right, re, rnull_cnt);
}
if (0 < hist_total(&rightcount))
{
parse_count_clamp(&rightcount);
if (le == NULL)
{
lrcnt_found = true;
d->match_right = true;
CACHE_COUNT(r_bnl, hist_muladdv(&total, &rightcount, d->cost, count),
do_count("C", ctxt, lw, w, le, d->left, lnull_cnt));
}
else
{
hist_muladd(&total, &leftcount, 0.0, &rightcount);
}
}
}
parse_count_clamp(&total);
}
if ((lrcnt_cache != NULL) && (d->match_left || d->match_right) &&
(ctxt->sent->null_count == 0))
{
lrcnt_keep_count(lrcnt_cache, le == NULL, d, leftcount,
rightcount);
}
}
if (lrcnt_cache != NULL)
{
bool match_list = (get_match_list_element(mchxt, mlb) != NULL);
if (lrcnt_expectation_update(lrcnt_cache, lrcnt_found, match_list,
null_count))
{
lrcnt_cache_changed = true;
}
if (lrcnt_found && (ctxt->sent->null_count == 0))
lrcnt_cache_match_list(lrcnt_cache, ctxt, mlb, le == NULL);
}
pop_match_list(mchxt, mlb);
}
if (lrcnt_cache_changed)
generate_word_skip_vector(ctxt, wvp, le, re, start_word, end_word, lw, rw);
return table_store(ctxt, lw, rw, le, re, null_count, h, total);
}
int do_parse(Sentence sent, fast_matcher_t *mchxt, count_context_t *ctxt,
Parse_Options opts)
{
Count_bin hist;
ctxt->current_resources = opts->resources;
ctxt->exhausted = false;
ctxt->checktimer = 0;
ctxt->islands_ok = opts->islands_ok;
ctxt->mchxt = mchxt;
hist = do_count("E", ctxt, -1, sent->length, NULL, NULL, sent->null_count+1);
table_stat(ctxt);
return (int)hist_total(&hist);
}
count_context_t * alloc_count_context(Sentence sent, Tracon_sharing *ts)
{
count_context_t *ctxt = malloc (sizeof(count_context_t));
memset(ctxt, 0, sizeof(count_context_t));
ctxt->sent = sent;
ctxt->is_short = !ENABLE_TABLE_LRCNT ||
((sent->length <= min_len_word_vector) && !IS_GENERATION(ctxt->sent->dict));
if (!ctxt->is_short)
{
for (unsigned int dir = 0; dir < 2; dir++)
ctxt->table_lrcnt[dir].num_tracon_id = ts->next_id[!dir] + 1;
init_table_lrcnt(ctxt);
}
if (NULL != sent->Table_tracon_pool)
{
pool_reuse(sent->Table_tracon_pool);
}
else
{
sent->Table_tracon_pool =
pool_new(__func__, "Table_tracon",
16382 , sizeof(Table_tracon),
false, false, false);
}
init_table(ctxt);
return ctxt;
}
void free_count_context(count_context_t *ctxt, Sentence sent)
{
if (NULL == ctxt) return;
COUNT_COST(lgdebug(+D_COUNT,
"Count cost per: word %"PRIu64", "
"disjunct %"PRIu64", null_count %"PRIu64"\n",
ctxt->count_cost[0], ctxt->count_cost[1], ctxt->count_cost[2]);)
free_table_lrcnt(ctxt);
free(ctxt);
}