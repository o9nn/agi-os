#include <string.h>
#include "api-structures.h"
#include "connectors.h"
#include "dict-common/dict-api.h"
#include "dict-common/dict-structures.h"
#include "dict-common/regex-morph.h"
#include "disjunct-utils.h"
#include "memory-pool.h"
#include "prepare/build-disjuncts.h"
#include "print/print-util.h"
#include "string-set.h"
#include "tokenize/tok-structures.h"
#include "tokenize/word-structures.h"
#include "tracon-set.h"
#include "utilities.h"
static char *connector_list_to_expression(const char *connector_list)
{
dyn_str *e = dyn_str_new();
for (const char *p = connector_list; *p != '\0'; p++)
{
if (*p != ' ')
{
dyn_strcat(e, (char []){ *p, '\0' });
continue;
}
if (p[1] != '\0') dyn_strcat(e, " & ");
}
return dyn_str_take(e);
}
char * disjunct_expression(Disjunct *d)
{
char *ls = print_connector_list_str(d->left, "-");
char *rs = print_connector_list_str(d->right, "+");
size_t lrs_sz = strlen(ls) + 1 + strlen(rs);
char *lrs = alloca(lrs_sz + 1);
size_t n = lg_strlcpy(lrs, ls, lrs_sz);
if ((ls[0] != '\0') && (rs[0] != '\0'))
n += lg_strlcpy(lrs + n, " ", lrs_sz);
lg_strlcpy(lrs + n, rs, lrs_sz);
lrs[lrs_sz] = '\0';
free(ls);
free(rs);
return connector_list_to_expression(lrs);
}
const Category_cost * disjunct_categories(Disjunct *d)
{
if (d->is_category == 0) return NULL;
return d->category;
}
Disjunct ** sentence_unused_disjuncts(Sentence sent)
{
if ((sent == NULL) || (sent->disjunct_used == NULL)) return NULL;
unsigned int n = 0;
for (unsigned int i = 0; i < sent->wildcard_word_num_disjuncts; i++)
{
if (!sent->disjunct_used[i]) n++;
}
const size_t unused_d_sz = sizeof(Disjunct *) * (n + 1);
Disjunct **unused_d = malloc(unused_d_sz);
n = 0;
for (unsigned int i = 0; i < sent->wildcard_word_num_disjuncts; i++)
{
if (!sent->disjunct_used[i])
unused_d[n++] = &((Disjunct *)sent->wildcard_word_dc_memblock)[i];
}
unused_d[n] = NULL;
return unused_d;
}
#define D_DISJ 5
#ifdef USE_SAT_SOLVER
void free_disjuncts(Disjunct *c)
{
Disjunct *c1;
for (;c != NULL; c = c1) {
c1 = c->next;
free_connectors(c->left);
free_connectors(c->right);
xfree((char *)c, sizeof(Disjunct));
}
}
#endif
void free_categories_from_disjunct_array(Disjunct *dbase,
unsigned int num_disjuncts)
{
for (Disjunct *d = dbase; d < &dbase[num_disjuncts]; d++)
{
if (d->is_category != 0)
free(d->category);
}
}
void free_categories(Sentence sent)
{
if (NULL != sent->dc_memblock)
{
free_categories_from_disjunct_array(sent->dc_memblock,
sent->num_disjuncts);
}
else
{
for (WordIdx w = 0; w < sent->length; w++)
{
for (Disjunct *d = sent->word[w].d; d != NULL; d = d->next)
{
if (d->is_category != 0)
free(d->category);
}
}
}
}
void free_sentence_disjuncts(Sentence sent, bool category_too)
{
if (NULL != sent->dc_memblock)
{
if (category_too) free_categories(sent);
free(sent->dc_memblock);
sent->dc_memblock = NULL;
}
else if (NULL != sent->Disjunct_pool)
{
pool_delete(sent->Disjunct_pool);
pool_delete(sent->Connector_pool);
sent->Disjunct_pool = NULL;
sent->Connector_pool = NULL;
}
}
Disjunct * catenate_disjuncts(Disjunct *d1, Disjunct *d2)
{
Disjunct * dis = d1;
if (d1 == NULL) return d2;
if (d2 == NULL) return d1;
while (dis->next != NULL) dis = dis->next;
dis->next = d2;
return d1;
}
unsigned int count_disjuncts(Disjunct * d)
{
unsigned int count = 0;
for (; d != NULL; d = d->next) count++;
return count;
}
static unsigned int count_connectors(Sentence sent)
{
unsigned int ccnt = 0;
for (WordIdx w = 0; w < sent->length; w++)
{
for (Disjunct *d = sent->word[w].d; d != NULL; d = d->next)
{
for (Connector *c = d->left; c != NULL; c = c->next) ccnt++;
for (Connector *c = d->right; c !=NULL; c = c->next) ccnt++;
}
}
return ccnt;
}
typedef struct disjunct_dup_table_s disjunct_dup_table;
struct disjunct_dup_table_s
{
unsigned int table_size_minus_1;
unsigned int log2_divisor;
Disjunct *dup_table[];
};
static inline connector_hash_t old_hash_disjunct(disjunct_dup_table *dt,
Disjunct * d, bool string_too)
{
connector_hash_t i = 0;
if (NULL != d->left)
i = connector_list_hash(d->left);
if (NULL != d->right)
i += 19 * connector_list_hash(d->right);
if (string_too)
i += string_hash(d->word_string);
d->dup_hash = i;
i *= FIBONACCI_MULT;
return ((i ^ (i>>dt->log2_divisor)) & (dt->table_size_minus_1));
}
static bool connectors_equal_prune(Connector *c1, Connector *c2)
{
return c1->desc == c2->desc && (c1->multi == c2->multi);
}
static bool disjuncts_equal(Disjunct * d1, Disjunct * d2, bool ignore_string)
{
Connector *e1, *e2;
if (d1->left == d2->right) return false;
e1 = d1->left;
e2 = d2->left;
while ((e1 != NULL) && (e2 != NULL)) {
if (!connectors_equal_prune(e1, e2)) return false;
e1 = e1->next;
e2 = e2->next;
}
if ((e1 != NULL) || (e2 != NULL)) return false;
e1 = d1->right;
e2 = d2->right;
while ((e1 != NULL) && (e2 != NULL)) {
if (!connectors_equal_prune(e1, e2)) return false;
e1 = e1->next;
e2 = e2->next;
}
if ((e1 != NULL) || (e2 != NULL)) return false;
if (ignore_string) return true;
if (d1->word_string == d2->word_string) return true;
return (strcmp(d1->word_string, d2->word_string) == 0);
}
#if 0
int de_fp = 0;
int de_total = 0;
static void disjuncts_equal_stat(void)
{
fprintf(stderr, "disjuncts_equal FP %d/%d\n", de_fp, de_total);
}
static bool disjuncts_equal(Disjunct * d1, Disjunct * d2, bool ignore_string)
{
if (de_total == 0) atexit(disjuncts_equal_stat);
de_total++;
bool rc = disjuncts_equal1(d1, d2, bool ignore_string);
if (!rc) de_fp++;
return rc;
}
#endif
static disjunct_dup_table * disjunct_dup_table_new(size_t sz)
{
disjunct_dup_table *dt;
dt = malloc(sz * sizeof(Disjunct *) + sizeof(disjunct_dup_table));
dt->table_size_minus_1 = sz - 1;
dt->log2_divisor = (sizeof(connector_hash_t)*CHAR_BIT) - power_of_2_log2(sz);
memset(dt->dup_table, 0, sz * sizeof(Disjunct *));
return dt;
}
static void disjunct_dup_table_delete(disjunct_dup_table *dt)
{
free(dt);
}
#define DEDUP_DEBUG 0
unsigned int eliminate_duplicate_disjuncts(Disjunct *dw, bool multi_string)
{
unsigned int count = 0;
disjunct_dup_table *dt;
Disjunct *prev = dw;
dt = disjunct_dup_table_new(next_power_of_two_up(2 * count_disjuncts(dw)));
#if DEDUP_DEBUG
unsigned int coll = 0;
#endif
for (Disjunct *d = dw; d != NULL; d = d->next)
{
Disjunct *dx;
connector_hash_t h = old_hash_disjunct(dt, d, !multi_string);
for (dx = dt->dup_table[h]; dx != NULL; dx = dx->dup_table_next)
{
if (d->dup_hash != dx->dup_hash) continue;
if (disjuncts_equal(dx, d, multi_string)) break;
}
if (dx != NULL)
{
if (multi_string)
{
if (dx->num_categories == dx->num_categories_alloced - 1)
{
dx->num_categories_alloced *= 2;
dx->category = realloc(dx->category,
sizeof(*(dx->category)) * dx->num_categories_alloced);
}
dassert((d->category[0].num > 0) && (d->category[0].num < 64*1024),
"Insane category %u", d->category[0].num);
dx->category[dx->num_categories].num = d->category[0].num;
dx->category[dx->num_categories].cost = d->cost;
dx->num_categories++;
dx->category[dx->num_categories].num = 0;
}
else
{
if (d->cost < dx->cost) dx->cost = d->cost;
dx->originating_gword =
gword_set_union(dx->originating_gword, d->originating_gword);
}
count++;
prev->next = d->next;
if (d->is_category != 0)
{
free(d->category);
d->is_category = 0;
}
}
else
{
#if DEDUP_DEBUG
if (dt->dup_table[h]) coll++;
#endif
d->dup_table_next = dt->dup_table[h];
dt->dup_table[h] = d;
prev = d;
}
}
#if DEDUP_DEBUG
#if 1
unsigned int pw[] = { 2, 7, 12, 22, 34, 46 , 0};
for (int i = 0; pw[i] != 0; i++)
if (dw->originating_gword->o_gword->sent_wordidx == pw[i])
#endif
{
fprintf(stderr, "edd: %.2f%% coll %u/%u\n",
100.f * coll / count_disjuncts(dw), coll, count_disjuncts(dw));
}
#endif
lgdebug(+D_DISJ+(0==count)*1024, "w%zu: Killed %u duplicates%s\n",
dw->originating_gword == NULL ? 0 :
dw->originating_gword->o_gword->sent_wordidx, count,
multi_string ? " (different word-strings)" : "");
disjunct_dup_table_delete(dt);
return count;
}
static void prt_con(Connector *c, dyn_str * p, char dir)
{
if (NULL == c) return;
prt_con (c->next, p, dir);
if (c->multi)
{
append_string(p, "@%s%c ", connector_string(c), dir);
}
else
{
append_string(p, "%s%c ", connector_string(c), dir);
}
}
char *print_one_disjunct_str(const Disjunct *dj)
{
dyn_str *p = dyn_str_new();
prt_con(dj->left, p, '-');
prt_con(dj->right, p, '+');
return dyn_str_take(p);
}
int left_connector_count(Disjunct * d)
{
int i=0;
for (;d!=NULL; d=d->next) {
for (Connector *c = d->left; c!=NULL; c = c->next) i++;
}
return i;
}
int right_connector_count(Disjunct * d)
{
int i=0;
for (;d!=NULL; d=d->next) {
for (Connector *c = d->right; c!=NULL; c = c->next) i++;
}
return i;
}
void count_disjuncts_and_connectors(Sentence sent, unsigned int *dca,
unsigned int *cca)
{
unsigned int ccnt = 0, dcnt = 0;
for (WordIdx w = 0; w < sent->length; w++)
{
unsigned int ndw = 0;
for (Disjunct *d = sent->word[w].d; d != NULL; d = d->next)
{
ndw++;
for (Connector *c = d->left; c != NULL; c = c->next) ccnt++;
for (Connector *c = d->right; c !=NULL; c = c->next) ccnt++;
}
sent->word[w].num_disjuncts = ndw;
dcnt += ndw;
}
*cca = ccnt;
*dca = dcnt;
}
static void tlsz_check(Tracon_list *tl, unsigned int index, int dir)
{
if (index >= tl->table_size[dir])
{
size_t new_id_table_size = (0 == tl->table_size[dir]) ?
index : tl->table_size[dir] * 2;
size_t new_bytes = new_id_table_size * sizeof(uint32_t *);
tl->table[dir] = realloc(tl->table[dir], new_bytes);
tl->table_size[dir] = new_id_table_size;
}
}
static Connector *pack_connectors(Tracon_sharing *ts, Connector *origc, int dir,
int w)
{
if (NULL == origc) return NULL;
Connector head;
Connector *prevc = &head;
Connector *newc = &head;
Connector *lcblock = ts->cblock;
Tracon_list *tl = ts->tracon_list;
for (Connector *o = origc; NULL != o;  o = o->next)
{
newc = NULL;
o->shallow = (o == origc);
if (NULL != ts->csid[dir])
{
Connector **tracon = tracon_set_add(o, ts->csid[dir]);
if (NULL == *tracon)
{
*tracon = lcblock;
if (NULL != tl)
{
tlsz_check(tl, tl->entries[dir], dir);
uint32_t cblock_index = (uint32_t)(lcblock - ts->cblock_base);
tl->table[dir][tl->entries[dir]] = cblock_index;
tl->entries[dir]++;
}
}
else
{
newc = *tracon;
if (!ts->is_pruning)
{
if ((o->nearest_word != newc->nearest_word) ||
(o->farthest_word != newc->farthest_word))
{
newc = NULL;
}
}
}
}
if (newc == NULL)
{
newc = lcblock++;
*newc = *o;
if (ts->is_pruning)
{
newc->refcount = 1;
if (ts->uc_seen[dir][connector_uc_num(newc)] != w)
{
ts->uc_seen[dir][connector_uc_num(newc)] = w;
ts->num_cnctrs_per_word[dir][w]++;
}
}
else
{
newc->tracon_id = ts->next_id[dir]++;
}
}
else
{
if (NULL != tl)
{
for (Connector *n = newc; NULL != n; n = n->next)
n->refcount++;
}
prevc->next = newc;
ts->cblock = lcblock;
return head.next;
}
prevc->next = newc;
prevc = newc;
}
newc->next = NULL;
ts->cblock = lcblock;
return head.next;
}
static Disjunct *pack_disjunct(Tracon_sharing *ts, Disjunct *d, int w)
{
Disjunct *newd;
uintptr_t token;
newd = ts->dblock++;
newd->word_string = d->word_string;
newd->cost = d->cost;
newd->is_category = d->is_category;
newd->originating_gword = d->originating_gword;
newd->ordinal = d->ordinal;
if (NULL != ts->csid[0])
{
if (NULL == ts->tracon_list)
token = (uintptr_t)d->originating_gword;
else
token = (uintptr_t)w;
if (token != ts->last_token)
{
ts->last_token = token;
tracon_set_reset(ts->csid[0]);
tracon_set_reset(ts->csid[1]);
}
}
newd->left = pack_connectors(ts, d->left, 0, w);
newd->right = pack_connectors(ts, d->right, 1,  w);
return newd;
}
static Disjunct *pack_disjuncts(Sentence sent, Tracon_sharing *ts,
Disjunct *origd, int w)
{
Disjunct head;
Disjunct *prevd = &head;
for (Disjunct *d = origd; NULL != d; d = d->next)
{
prevd->next = pack_disjunct(ts, d, w);
prevd = prevd->next;
}
prevd->next = NULL;
return head.next;
}
#define TLSZ 8192
static Tracon_sharing *pack_sentence_init(Sentence sent, bool is_pruning)
{
unsigned int dcnt = 0, ccnt = 0;
count_disjuncts_and_connectors(sent, &dcnt, &ccnt);
size_t dsize = dcnt * sizeof(Disjunct);
if (sizeof(Disjunct) != 64)
dsize = ALIGN(dsize, sizeof(Connector));
size_t csize = ccnt * sizeof(Connector);
size_t memblock_sz = dsize + csize;
void *memblock = malloc(memblock_sz);
Disjunct *dblock = memblock;
Connector *cblock = (Connector *)((char *)memblock + dsize);
Tracon_sharing *ts = malloc(sizeof(Tracon_sharing));
memset(ts, 0, sizeof(Tracon_sharing));
ts->memblock = memblock;
ts->memblock_sz = memblock_sz;
ts->cblock_base = cblock;
ts->cblock = cblock;
ts->dblock = dblock;
ts->num_connectors = ccnt;
ts->num_disjuncts = dcnt;
ts->word_offset = is_pruning ? 1 : NULL_TRACON_BLOCK;
ts->is_pruning = is_pruning;
ts->next_id[0] = ts->next_id[1] = ts->word_offset;
ts->last_token = (uintptr_t)-1;
if (is_pruning)
{
unsigned int **ncu = ts->num_cnctrs_per_word;
ncu[0] = malloc(2 * sent->length * sizeof(**ncu));
ncu[1] = ncu[0] + sent->length;
memset(ncu[0], 0, 2 * sent->length * sizeof(**ncu));
size_t uc_num = sent->dict->contable.num_uc;
ts->uc_seen[0] = malloc(2 * uc_num * sizeof(**ts->uc_seen));
ts->uc_seen[1] = ts->uc_seen[0] + uc_num;
memset(ts->uc_seen[0], -1, 2 * uc_num * sizeof(**ts->uc_seen));
}
if (sent->length >= sent->min_len_encoding)
{
ts->csid[0] = tracon_set_create();
ts->csid[1] = tracon_set_create();
if (is_pruning)
{
Tracon_list *tl;
tl = ts->tracon_list = malloc(sizeof(Tracon_list));
memset(tl, 0, sizeof(Tracon_list));
for (int dir = 0; dir < 2; dir++)
{
tracon_set_shallow(true, ts->csid[dir]);
tlsz_check(tl, TLSZ, dir);
}
}
}
if (!is_pruning && (ts->memblock != sent->dc_memblock))
{
if (sent->dc_memblock) free(sent->dc_memblock);
sent->dc_memblock = ts->memblock;
sent->num_disjuncts = ts->num_disjuncts;
}
return ts;
}
void free_tracon_sharing(Tracon_sharing *ts)
{
if (NULL == ts) return;
for (int dir = 0; dir < 2; dir++)
{
if (NULL != ts->tracon_list)
free(ts->tracon_list->table[dir]);
if (NULL != ts->csid[dir])
{
tracon_set_delete(ts->csid[dir]);
ts->csid[dir] = NULL;
}
}
free(ts->uc_seen[0]);
free(ts->num_cnctrs_per_word[0]);
if (NULL != ts->d) free(ts->d);
free(ts->tracon_list);
ts->tracon_list = NULL;
free(ts);
}
void free_tracon_memblock(Tracon_sharing *ts)
{
free(ts->memblock);
free_tracon_sharing(ts);
}
static Tracon_sharing *pack_sentence(Sentence sent, bool is_pruning)
{
Tracon_sharing *ts = pack_sentence_init(sent, is_pruning);
for (WordIdx w = 0; w < sent->length; w++)
{
sent->word[w].d = pack_disjuncts(sent, ts, sent->word[w].d, w);
}
return ts;
}
Tracon_sharing *pack_sentence_for_pruning(Sentence sent)
{
unsigned int ccnt_before = 0;
if (verbosity_level(D_DISJ)) ccnt_before = count_connectors(sent);
Tracon_sharing *ts = pack_sentence(sent, true);
if (NULL == ts->csid[0])
{
lgdebug(D_DISJ, "Debug: Encode for pruning (len %zu): None\n",
sent->length);
}
else
{
lgdebug(D_DISJ, "Debug: Encode for pruning (len %zu): "
"tracon_id %zu (%zu+,%zu-), shared connectors %d\n",
sent->length,
ts->tracon_list->entries[0]+ts->tracon_list->entries[1],
ts->tracon_list->entries[0], ts->tracon_list->entries[1],
(int)(&ts->cblock_base[ccnt_before] - ts->cblock));
}
return ts;
}
Tracon_sharing *pack_sentence_for_parsing(Sentence sent)
{
unsigned int ccnt_before = 0;
if (verbosity_level(D_DISJ)) ccnt_before = count_connectors(sent);
Tracon_sharing *ts = pack_sentence(sent, false);
if (verbosity_level(D_SPEC+2))
{
printf("pack_sentence_for_parsing (null_count %u):\n", sent->null_count);
print_all_disjuncts(sent);
}
if (NULL == ts->csid[0])
{
lgdebug(D_DISJ, "Debug: Encode for parsing (len %zu): None\n",
sent->length);
}
else
{
lgdebug(D_DISJ, "Debug: Encode for parsing (len %zu): "
"tracon_id %d (%d+,%d-), shared connectors %d\n",
sent->length,
(ts->next_id[0]-ts->word_offset)+(ts->next_id[1]-ts->word_offset),
ts->next_id[0]-ts->word_offset, ts->next_id[1]-ts->word_offset,
(int)(&ts->cblock_base[ccnt_before] - ts->cblock));
}
return ts;
}
void *save_disjuncts(Sentence sent, Tracon_sharing *ts)
{
void *saved_memblock = malloc(ts->memblock_sz);
memcpy(saved_memblock, ts->memblock, ts->memblock_sz);
if (NULL == ts->d)
ts->d = malloc(sent->length * sizeof(Disjunct *));
for (WordIdx w = 0; w < sent->length; w++)
ts->d[w] = sent->word[w].d;
return saved_memblock;
}
void restore_disjuncts(Sentence sent, void *saved_memblock, Tracon_sharing *ts)
{
if (NULL == saved_memblock) return;
for (WordIdx w = 0; w < sent->length; w++)
sent->word[w].d = ts->d[w];
memcpy(ts->memblock, saved_memblock, ts->memblock_sz);
}
void free_saved_memblock(void * blk)
{
free(blk);
}