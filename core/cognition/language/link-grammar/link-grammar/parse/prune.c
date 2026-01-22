#include "api-structures.h"
#include "connectors.h"
#include "disjunct-utils.h"
#include "dict-common/dict-common.h"
#include "linkage/analyze-linkage.h"
#include "post-process/post-process.h"
#include "post-process/pp-structures.h"
#include "print/print.h"
#include "prune.h"
#include "resources.h"
#include "string-set.h"
#include "tokenize/word-structures.h"
#include "tokenize/wordgraph.h"
#define D_PRUNE 5
#ifdef DEBUG
#define DEBUG_PP_PRUNE
#endif
#ifdef DEBUG_PP_PRUNE
#define ppdebug(...) lgdebug(+D_PRUNE+1, __VA_ARGS__)
#else
#define ppdebug(...)
#endif
#define PRx(x) fprintf(stderr, ""#x)
#define PR(...) true
#define BAD_WORD (MAX_SENTENCE+1)
typedef uint8_t WordIdx_m;
typedef struct
{
WordIdx_m nw[2];
WordIdx_m nw_perjet[2];
WordIdx_m nw_unidir[2];
WordIdx_m fw[2];
} mlink_table;
typedef struct c_list_s C_list;
struct c_list_s
{
C_list *next;
Connector *c;
};
typedef struct power_table_s power_table;
struct power_table_s
{
unsigned int power_table_size;
unsigned int *table_size[2];
C_list ***table[2];
Pool_desc *memory_pool;
};
typedef struct prune_context_s prune_context;
struct prune_context_s
{
unsigned int null_links;
unsigned int null_words;
bool *is_null_word;
bool islands_ok;
uint8_t pass_number;
bool always_parse;
int N_changed;
int N_deleted[2];
power_table *pt;
mlink_table *ml;
Sentence sent;
int power_cost;
int N_xlink;
};
#ifdef DEBUG
GNUC_UNUSED static void print_power_table_entry(power_table *pt, int w, int dir)
{
C_list **t = pt->table[w][dir];
unsigned int size = pt->table_size[w][dir];
if (size == 1) return;
printf("w%d dir%d size=%u:\n", w, dir, size);
for (unsigned int i = 0; i < size; i++)
{
if (t[i] == NULL) continue;
printf(" [%u]: ", i);
for (C_list *cl = t[i]; cl != NULL; cl = cl->next)
{
char *cstr = print_one_connector_str(cl->c, "lrs");
printf("%s", cstr);
free(cstr);
if (cl->next != NULL) printf(" ");
}
printf("\n");
}
}
GNUC_UNUSED static void print_power_table(Sentence sent, power_table *pt)
{
printf("power table:\n");
for (WordIdx w = 0; w < sent->length; w++)
{
for (int dir = 0; dir < 2; dir++)
print_power_table_entry(pt, w, dir);
}
}
#endif
static void power_table_delete(power_table *pt)
{
pool_delete(pt->memory_pool);
free(pt->table_size[0]);
free(pt->table[0][0]);
free(pt->table[0]);
}
static C_list **get_power_table_entry(unsigned int size, C_list **t,
Connector *c)
{
unsigned int h, s;
h = s = connector_uc_num(c) & (size-1);
while (NULL != t[h])
{
if (connector_uc_eq(t[h]->c, c)) break;
h = (h + 1) & (size-1);
if (h == s) return NULL;
}
return &t[h];
}
static void put_into_power_table(Pool_desc *mp, unsigned int size, C_list **t,
Connector *c)
{
C_list **e = get_power_table_entry(size, t, c);
assert(NULL != e, "Overflow");
assert(c->refcount > 0, "refcount %d", c->refcount);
C_list *m = pool_alloc(mp);
m->next = *e;
*e = m;
m->c = c;
}
static void power_table_alloc(Sentence sent, power_table *pt)
{
pt->power_table_size = sent->length;
pt->table_size[0] = malloc (2 * sent->length * sizeof(unsigned int));
pt->table_size[1] = pt->table_size[0] + sent->length;
pt->table[0] = malloc (2 * sent->length * sizeof(C_list **));
pt->table[1] = pt->table[0] + sent->length;
}
static void power_table_init(Sentence sent, Tracon_sharing *ts, power_table *pt)
{
Tracon_list *tl = ts->tracon_list;
power_table_alloc(sent, pt);
Pool_desc *mp = pt->memory_pool = pool_new(__func__, "C_list",
2048, sizeof(C_list),
false, false, false);
unsigned int num_headers = 0;
C_list **memblock_headers;
C_list **hash_table_header;
unsigned int *ncu[2];
ncu[0] = alloca(sent->length * sizeof(*ncu[0]));
ncu[1] = alloca(sent->length * sizeof(*ncu[1]));
for (WordIdx w = 0; w < sent->length; w++)
{
for (size_t dir = 0; dir < 2; dir++)
{
unsigned int tsize;
unsigned int n = ts->num_cnctrs_per_word[dir][w];
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
memblock_headers = malloc(num_headers * sizeof(C_list *));
memset(memblock_headers, 0, num_headers * sizeof(C_list *));
hash_table_header = memblock_headers;
for (WordIdx w = 0; w < sent->length; w++)
{
for (size_t dir = 0; dir < 2; dir++)
{
unsigned int tsize = ncu[dir][w];
C_list **t = hash_table_header;
hash_table_header += tsize;
pt->table[dir][w] = t;
pt->table_size[dir][w] = tsize;
memset(t, 0, sizeof(C_list *) * tsize);
if (NULL == tl)
{
Connector *c;
for (Disjunct *d = sent->word[w].d; d != NULL; d = d->next)
{
c = (dir == 0) ? d->left : d->right;
if (c == NULL) continue;
for (c = c->next; c != NULL; c = c->next)
put_into_power_table(mp, tsize, t, c);
}
for (Disjunct *d = sent->word[w].d; d != NULL; d = d->next)
{
c = (dir == 0) ? d->left : d->right;
if (c == NULL) continue;
put_into_power_table(mp, tsize, t, c);
}
}
}
}
assert(memblock_headers + num_headers == hash_table_header,
"Mismatch header sizes");
if (NULL != tl)
{
for (size_t dir = 0; dir < 2; dir++)
{
C_list ***tp = pt->table[dir];
unsigned int *sizep = pt->table_size[dir];
unsigned int sid_entries = tl->entries[dir];
for (int shallow = 0; shallow < 2; shallow++)
{
for (unsigned int id = 0; id < sid_entries; id++)
{
Connector *c = get_tracon(ts, dir, id);
if (!!shallow != c->shallow) continue;
int w = get_tracon_word_number(c, dir);
put_into_power_table(mp, sizep[w], tp[w], c);
}
}
}
}
}
static void clean_table(unsigned int size, C_list **t)
{
#define UC_NUM_TOMBSTONE ((connector_uc_hash_t)-1)
static condesc_more_t cm_no_match =
{
.string = "TOMBSTONE",
};
static condesc_t desc_no_match =
{
.uc_num = UC_NUM_TOMBSTONE,
.more = &cm_no_match
};
static Connector con_no_match =
{
.desc = &desc_no_match,
.refcount = 1,
.shallow = false,
};
for (unsigned int i = 0; i < size; i++)
{
C_list **m = &t[i];
while (NULL != *m)
{
assert(0 <= (*m)->c->refcount, "refcount < 0 (%d)",
(*m)->c->refcount);
if (0 == (*m)->c->refcount)
{
if ((*m == t[i]) && (NULL == (*m)->next) &&
(NULL != t[(i+1) & (size-1)]))
{
(*m)->c = &con_no_match;
}
else
{
*m = (*m)->next;
}
}
else
{
m = &(*m)->next;
}
}
}
}
#if TOO_MUCH_OVERHEAD
static bool find_no_xlink_disjunct(prune_context *pc, int w,
Connector *lc, Connector *rc,
int lword, int rword)
{
Sentence sent = pc->sent;
Disjunct *d = sent->word[w].d;
if ((pc->ml[w].nw[0] == w) || (pc->ml[w].nw[1] == w)) return true;
for (d = sent->word[w].d; d != NULL; d = d->next)
{
if (d->left->nearest_word < lword)
continue;
if (d->right->nearest_word > rword)
continue;
break;
}
if (d == NULL)
{
PR(N);
return false;
}
return true;
}
#endif
static bool
left_table_search(prune_context *pc, int w, Connector *c,
bool shallow, int word_c);
static bool
right_table_search(prune_context *pc, int w, Connector *c,
bool shallow, int word_c);
static bool is_match(prune_context *pc,
bool (*table_search)(prune_context *, int, Connector *, bool, int),
int word_c, Connector *c, int w)
{
if (c->next == NULL)
{
if (!c->multi) return false;
}
else
{
c = connector_deepest(c);
}
return table_search(pc, w, c, false, word_c);
}
static bool is_cross_mlink(prune_context *pc,
Connector *lc, Connector *rc,
int lword, int rword)
{
if (rword - lword == 1) return false;
if (pc->ml == NULL) return false;
Sentence sent = pc->sent;
int null_allowed = pc->null_links - pc->null_words;
if (pc->islands_ok)
{
if (null_allowed > 0) return false;
}
else
{
if (null_allowed > rword - lword - 1) return false;
}
for (int w = lword+1; w < rword; w++)
{
if (sent->word[w].optional) continue;
if (pc->is_null_word[w]) continue;
if ((w == lword+1) && (pc->ml[w].nw_perjet[1] > rword) &&
!is_match(pc, left_table_search, lword, lc, w))
{
PR(L);
goto null_word_found;
}
if ((w == rword-1) && (pc->ml[w].nw_perjet[0] < lword) &&
!is_match(pc, right_table_search, rword, rc, w))
{
PR(R);
goto null_word_found;
}
if ((pc->ml[w].nw_perjet[0] < lword) && (pc->ml[w].nw_perjet[1] > rword))
{
PR(P);
goto null_word_found;
}
#if 1
if ((pc->ml[w].nw[0] < lword) && PR(L)) goto null_word_found;
if ((pc->ml[w].nw[1] > rword) && PR(R)) goto null_word_found;
#endif
#if 1
if (lword == pc->ml[w].nw[0])
{
Connector *c = connector_deepest(lc);
if (!c->multi && (c->nearest_word > w) && PR(A)) goto null_word_found;
}
#endif
#if 1
if (rword == pc->ml[w].nw[1])
{
Connector *c = connector_deepest(rc);
if (!c->multi && (c->nearest_word < w) && PR(B)) goto null_word_found;
}
#endif
if ((lc->next != NULL) && (rc->next != NULL))
{
#if VERY_WEAK
if ((pc->ml[w].nw[0] < lc->next->nearest_word) &&
(rc->next->nearest_word < w))
{
PR(C);
goto null_word_found;
}
#endif
#if 1
if ((pc->ml[w].nw[1] > rc->next->nearest_word) &&
(lc->next->nearest_word > w))
{
PR(D);
goto null_word_found;
}
#endif
}
#if TOO_MUCH_OVERHEAD
if (!find_no_xlink_disjunct(pc, w, lc, rc, lword, rword))
goto null_word_found;
#endif
continue;
null_word_found:
if (null_allowed == 0)
{
pc->N_xlink++;
return true;
}
null_allowed--;
continue;
}
return false;
}
bool optional_gap_collapse(Sentence sent, int w1, int w2)
{
for (int w = w1+1; w < w2; w++)
if (!sent->word[w].optional) return false;
return true;
}
static bool more_nulls_than_allowed(prune_context *pc, int w1, int w2)
{
int null_allowed = pc->null_links - pc->null_words;
if (pc->islands_ok)
{
if (null_allowed > 0) return false;
}
else
{
if (null_allowed > w2 - w1 - 1) return false;
}
for (int w = w1+1; w < w2; w++)
{
if (pc->sent->word[w].optional) continue;
if (pc->is_null_word[w]) continue;
if (null_allowed == 0) return true;
null_allowed--;
}
return false;
}
static bool possible_connection(prune_context *pc,
Connector *lc, Connector *rc,
int lword, int rword)
{
int dist = rword - lword;
#ifdef DEBUG
assert(0 < dist, "Bad word order in possible connection.");
#endif
if (!lc_easy_match(lc->desc, rc->desc)) return false;
if ((lc->nearest_word > rword) || (rc->nearest_word < lword)) return false;
if (1 == dist)
{
if ((lc->next != NULL) || (rc->next != NULL))
return false;
return true;
}
if ((rword > lc->farthest_word) || (lword < rc->farthest_word))
return false;
if ((lc->next == NULL) && (rc->next == NULL) &&
(!lc->multi || (lc->nearest_word == rword)) &&
(!rc->multi || (rc->nearest_word == lword)) &&
more_nulls_than_allowed(pc, lword, rword))
{
return false;
}
if ((lc->next != NULL) && (rc->next != NULL))
{
if (lc->next->nearest_word > rc->next->nearest_word)
return false;
}
if (is_cross_mlink(pc, lc, rc, lword, rword))
return false;
return true;
}
static bool
right_table_search(prune_context *pc, int w, Connector *c,
bool shallow, int word_c)
{
power_table *pt = pc->pt;
unsigned int size = pt->table_size[1][w];
C_list **e = get_power_table_entry(size, pt->table[1][w], c);
for (C_list *cl = *e; cl != NULL; cl = cl->next)
{
if (!shallow && !cl->c->shallow) return false;
if (possible_connection(pc, cl->c, c, w, word_c))
return true;
}
return false;
}
static bool
left_table_search(prune_context *pc, int w, Connector *c,
bool shallow, int word_c)
{
power_table *pt = pc->pt;
unsigned int size = pt->table_size[0][w];
C_list **e = get_power_table_entry(size, pt->table[0][w], c);
for (C_list *cl = *e; cl != NULL; cl = cl->next)
{
if (!shallow && !cl->c->shallow) return false;
if (possible_connection(pc, c, cl->c, word_c, w))
return true;
}
return false;
}
static int
left_connector_list_update(prune_context *pc, Connector *c,
int w, bool shallow)
{
int n, lb;
int foundmatch = -1;
if (c == NULL) return w;
if (c->prune_pass == pc->pass_number) return c->nearest_word;
n = left_connector_list_update(pc, c->next, w, false) - 1;
if (0 > n) return -1;
if (((int) c->nearest_word) < n) n = c->nearest_word;
lb = c->farthest_word;
for (; n >= lb; n--)
{
pc->power_cost++;
if (right_table_search(pc, n, c, shallow, w))
{
foundmatch = n;
break;
}
}
if (foundmatch < ((int) c->nearest_word))
{
c->nearest_word = foundmatch;
pc->N_changed++;
}
if (foundmatch != -1)
{
int farthest_word = n;
for (int l = lb; l < n; l++)
{
pc->power_cost++;
if (right_table_search(pc, l, c, shallow, w))
{
farthest_word = l;
break;
}
}
if (farthest_word > (int)c->farthest_word)
{
c->farthest_word = farthest_word;
pc->N_changed++;
}
}
return foundmatch;
}
static size_t
right_connector_list_update(prune_context *pc, Connector *c,
size_t w, bool shallow)
{
int n, ub;
int sent_length = (int)pc->sent->length;
int foundmatch = BAD_WORD;
if (c == NULL) return w;
if (c->prune_pass == pc->pass_number) return c->nearest_word;
n = right_connector_list_update(pc, c->next, w, false) + 1;
if (sent_length <= n) return BAD_WORD;
if (c->nearest_word > n) n = c->nearest_word;
ub = c->farthest_word;
for (; n <= ub; n++)
{
pc->power_cost++;
if (left_table_search(pc, n, c, shallow, w))
{
foundmatch = n;
break;
}
}
if (foundmatch > c->nearest_word) {
c->nearest_word = foundmatch;
pc->N_changed++;
}
if (n <= ub)
{
int farthest_word = n;
for (int l = ub; l > n; l--)
{
pc->power_cost++;
if (left_table_search(pc, l, c, shallow, w))
{
farthest_word = l;
break;
}
}
if (farthest_word < (int)c->farthest_word)
{
c->farthest_word = farthest_word;
pc->N_changed++;
}
}
return foundmatch;
}
static void mark_jet_as_good(Connector *c, int pass_number)
{
for (; NULL != c; c = c->next)
c->prune_pass = pass_number;
}
static void mark_jet_for_dequeue(Connector *c, bool mark_bad_word)
{
if (mark_bad_word) c->nearest_word = BAD_WORD;
for (; NULL != c; c = c->next)
{
c->refcount--;
}
}
static bool is_bad(Connector *c)
{
for (; c != NULL; c = c->next)
if (c->nearest_word == BAD_WORD) return true;
return false;
}
static bool check_null_word(prune_context *pc, int w)
{
if (pc->always_parse) return false;
Word *word = &pc->sent->word[w];
if ((word->d == NULL) && !word->optional && !pc->is_null_word[w])
{
pc->null_words++;
pc->is_null_word[w] = true;
if (pc->null_words > pc->null_links) return true;
}
return false;
}
static bool pruning_pass_end(prune_context *pc, const char *pass_dir,
int *prune_total)
{
int total = pc->N_deleted[0] + pc->N_deleted[1];
char xlink_found[32] = "";
if (pc->N_xlink != 0)
snprintf(xlink_found, sizeof(xlink_found), ", xlink=%d", pc->N_xlink);
lgdebug(D_PRUNE, "Debug: %s pass changed %d and deleted %d (%d+%d)%s\n",
pass_dir, pc->N_changed, total, pc->N_deleted[0], pc->N_deleted[1],
xlink_found);
bool pass_end = ((pc->N_changed == 0) && (total == 0));
pc->N_changed = pc->N_deleted[0] = pc->N_deleted[1] = pc->N_xlink = 0;
*prune_total += total;
return pass_end;
}
static int power_prune(Sentence sent, prune_context *pc, Parse_Options opts)
{
int total_deleted = 0;
bool extra_null_word = false;
power_table *pt = pc->pt;
pc->N_changed = 1;
do
{
pc->pass_number++;
for (WordIdx w = 0; w < sent->length; w++)
{
for (Disjunct **dd = &sent->word[w].d; *dd != NULL; )
{
Disjunct *d = *dd;
if (d->left == NULL)
{
dd = &d->next;
continue;
}
bool bad = is_bad(d->left);
if (bad || left_connector_list_update(pc, d->left, w, true) < 0)
{
mark_jet_for_dequeue(d->left, true);
mark_jet_for_dequeue(d->right, false);
*dd = d->next;
if (d->is_category != 0) free(d->category);
pc->N_deleted[(int)bad]++;
continue;
}
mark_jet_as_good(d->left, pc->pass_number);
dd = &d->next;
}
if (check_null_word(pc, w)) extra_null_word = true;
clean_table(pt->table_size[1][w], pt->table[1][w]);
}
if (pruning_pass_end(pc, "l->r", &total_deleted)) break;
for (WordIdx w = sent->length-1; w != (WordIdx) -1; w--)
{
for (Disjunct **dd = &sent->word[w].d; *dd != NULL; )
{
Disjunct *d = *dd;
if (d->right == NULL)
{
dd = &d->next;
continue;
}
bool bad = is_bad(d->right);
if (bad || right_connector_list_update(pc, d->right, w, true) >= sent->length)
{
mark_jet_for_dequeue(d->right, true);
mark_jet_for_dequeue(d->left, false);
*dd = d->next;
if (d->is_category != 0) free(d->category);
pc->N_deleted[(int)bad]++;
continue;
}
mark_jet_as_good(d->right, pc->pass_number);
dd = &d->next;
}
if (check_null_word(pc, w)) extra_null_word = true;
clean_table(pt->table_size[0][w], pt->table[0][w]);
}
if (pruning_pass_end(pc, "r->l", &total_deleted)) break;
pc->ml = NULL;
}
while (!extra_null_word || pc->always_parse);
char found_nulls[32] = "";
if ((verbosity >= D_USER_TIMES) && !extra_null_word && (pc->null_words > 0))
snprintf(found_nulls, sizeof(found_nulls), ", found %u", pc->null_words);
print_time(opts, "power pruned (for %u null%s%s%s)",
pc->null_links, (pc->null_links != 1) ? "s" : "",
extra_null_word ? ", extra null" : "", found_nulls);
if (verbosity_level(D_PRUNE))
{
prt_error("\n\\");
prt_error("Debug: Power prune cost: %d\n", pc->power_cost);
prt_error("Debug: After power_pruning (for %u null%s, sent->null_count %u):\n\\",
pc->null_links, (pc->null_links != 1) ? "s" : "", pc->sent->null_count);
print_disjunct_counts(pc->sent);
}
#ifdef DEBUG
for (WordIdx w = 0; w < sent->length; w++)
{
for (Disjunct *d = sent->word[w].d; NULL != d; d = d->next)
{
for (int dir = 0; dir < 2; dir++)
{
Connector *c = (dir) ? (d->left) : (d->right);
for (; NULL != c; c = c->next)
{
assert(c->nearest_word != BAD_WORD, "dir %d w %zu", dir, w);
assert(c->refcount > 0, "dir %d w %zu", dir, w);
}
}
}
}
#endif
if (extra_null_word && !pc->always_parse) return -1;
return total_deleted;
}
typedef struct cms_struct Cms;
struct cms_struct
{
Cms *next;
Connector *c;
bool last_criterion;
bool left;
bool right;
};
#define CMS_SIZE (1<<11)
typedef struct multiset_table_s multiset_table;
struct multiset_table_s
{
Cms memblock[CMS_SIZE];
Cms *mb;
Pool_desc *mp;
String_set *sset;
Cms *cms_table[CMS_SIZE];
};
static multiset_table *cms_table_new(Sentence sent)
{
multiset_table *mt = malloc(sizeof(multiset_table));
memset(mt->cms_table, 0, CMS_SIZE * sizeof(Cms *));
mt->mb = mt->memblock;
mt->mp = NULL;
mt->sset = sent->string_set;
return mt;
}
static void cms_table_delete(multiset_table *mt)
{
if (mt->mp != NULL) pool_delete(mt->mp);
free(mt);
}
static unsigned int cms_hash(const char *s)
{
unsigned int i = 5381;
if (islower((unsigned int)*s)) s++;
while (is_connector_name_char(*s))
{
i = ((i << 5) + i) + *s;
s++;
}
return (i & (CMS_SIZE-1));
}
static void reset_last_criterion(multiset_table *cmt, const char *criterion)
{
unsigned int h = cms_hash(criterion);
for (Cms *cms = cmt->cms_table[h]; cms != NULL; cms = cms->next)
cms->last_criterion = false;
}
static bool can_form_link(const char *s, const char *t, const char *e)
{
if (islower((unsigned char)*t)) t++;
while (is_connector_name_char(*s))
{
if (*s != *t) return false;
s++;
t++;
}
if (is_connector_name_char(*t)) return false;
while (*t != '\0')
{
if (*s == '\0') return true;
if (*s != *t && *s != '#' && (s == e || *t != '*')) return false;
s++;
t++;
}
while (*s != '\0')
{
if (*s != '*' && *s != '#' && s == e) return false;
s++;
}
return true;
}
#ifdef DEBUG_PP_PRUNE
static const char *connector_signs(Cms *cms)
{
static char buf[3];
size_t i = 0;
if (cms->left) buf[i++] = '-';
if (cms->right) buf[i++] = '+';
buf[i] = '\0';
return buf;
}
#else
#define connector_signs(x) NULL
#endif
static bool match_in_cms_table(multiset_table *cmt, const char *pp_link,
const char *subscr)
{
unsigned int h = cms_hash(pp_link);
bool found = false;
for (Cms *cms = cmt->cms_table[h]; cms != NULL; cms = cms->next)
{
if (cms->c->nearest_word == BAD_WORD) continue;
if (can_form_link(pp_link, connector_string(cms->c), subscr))
{
ppdebug("MATCHED %s%s\n", connector_string(cms->c),connector_signs(cms));
cms->last_criterion = true;
found = true;
continue;
}
ppdebug("NOT-MATCHED %s%s\n", connector_string(cms->c),connector_signs(cms));
}
return found;
}
static Cms *lookup_in_cms_table(multiset_table *cmt, Connector *c)
{
unsigned int h = cms_hash(connector_string(c));
for (Cms *cms = cmt->cms_table[h]; cms != NULL; cms = cms->next)
{
if (c->desc == cms->c->desc) return cms;
}
return NULL;
}
static Cms *cms_alloc(multiset_table *cmt)
{
if (cmt->mb < &cmt->memblock[CMS_SIZE])
return cmt->mb++;
if (cmt->mp == NULL)
{
cmt->mp = pool_new(__func__, "Cms",
CMS_SIZE, sizeof(Cms),
false, false, false);
}
return pool_alloc(cmt->mp);
}
static void insert_in_cms_table(multiset_table *cmt, Connector *c, int dir)
{
Cms *cms, *prev = NULL;
unsigned int h = cms_hash(connector_string(c));
for (cms = cmt->cms_table[h]; cms != NULL; cms = cms->next)
{
if (c->desc == cms->c->desc) break;
prev = cms;
}
if (cms == NULL)
{
cms = cms_alloc(cmt);
cms->c = c;
cms->next = cmt->cms_table[h];
cmt->cms_table[h] = cms;
cms->left = cms->right = false;
}
else
{
if (prev != NULL)
{
prev->next = cms->next;
cms->next = cmt->cms_table[h];
cmt->cms_table[h] = cms;
}
}
if (dir == 0)
cms->left = true;
else
cms->right = true;
cms->last_criterion = false;
}
#define AtoZ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
static bool all_connectors_exist(multiset_table *cmt, const char *pp_link)
{
ppdebug("check PP-link=%s\n", pp_link);
const char *s;
for (s = pp_link; is_connector_name_char(*s); s++) {}
do
{
ppdebug("subscript at %d\n", (int)(s-pp_link-strspn(pp_link, AtoZ)));
if (*s == '#') continue;
if (!match_in_cms_table(cmt, pp_link, s)) return false;
}
while (*s++ != '\0' && *s != '\0');
return true;
}
static bool connector_has_direction(Cms *cms, int dir)
{
return ((dir == 0) && cms->left) || ((dir == 1) && cms->right);
}
static bool any_possible_connection(multiset_table *cmt, const char *criterion)
{
unsigned int h = cms_hash(criterion);
for (Cms *cms1 = cmt->cms_table[h]; cms1 != NULL; cms1 = cms1->next)
{
if (!cms1->last_criterion) continue;
ppdebug("TRY %s%s\n", connector_string(cms1->c), connector_signs(cms1));
for (int dir = 0; dir < 2; dir++)
{
if (!connector_has_direction(cms1, dir)) continue;
Connector *c = cms1->c;
for (Cms *cms2 = cmt->cms_table[h]; cms2 != NULL; cms2 = cms2->next)
{
if (!connector_has_direction(cms2, 1-dir)) continue;
Connector *cfl = cms2->c;
if (easy_match_desc(cfl->desc, c->desc))
{
const char *link = intersect_strings(cmt->sset, cfl, c);
if (post_process_match(criterion, link))
{
ppdebug("%s+ %s- PPLINK\n", connector_string(cfl), connector_string(c));
reset_last_criterion(cmt, criterion);
return true;
}
ppdebug("%s+ %s- NO PPLINK\n", connector_string(cfl), connector_string(c));
continue;
}
ppdebug("%s+ %s- NOMATCH\n", connector_string(cfl), connector_string(c));
}
}
}
ppdebug(">>>No connection possible\n");
reset_last_criterion(cmt, criterion);
return false;
}
static bool rule_satisfiable(multiset_table *cmt, pp_linkset *ls)
{
for (unsigned int hashval = 0; hashval < ls->hash_table_size; hashval++)
{
for (pp_linkset_node *p = ls->hash_table[hashval]; p != NULL; p = p->next)
{
if (all_connectors_exist(cmt, p->str))
{
ppdebug("TRUE\n");
if (any_possible_connection(cmt, p->str)) return true;
}
reset_last_criterion(cmt, p->str);
}
}
ppdebug("FALSE\n");
return false;
}
static bool mark_bad_connectors(multiset_table *cmt, Connector *c)
{
if (c->nearest_word == BAD_WORD)
return true;
Cms *cms = lookup_in_cms_table(cmt, c);
if (cms->c->nearest_word == BAD_WORD)
{
c->nearest_word = BAD_WORD;
return true;;
}
return false;
}
static bool selector_mismatch_wild(multiset_table *cmt, const char *s,
Cms *cms_t)
{
unsigned int h = cms_hash(s);
ppdebug("Selector %s, trigger %s\n", s, connector_string(cms_t->c));
for (Cms *cms = cmt->cms_table[h]; cms != NULL; cms = cms->next)
{
if ((cms_t->left && !cms->right) || (cms_t->right && !cms->left))
continue;
size_t len_s = strlen(s);
if (easy_match_desc(cms_t->c->desc, cms->c->desc))
{
const char *c = connector_string(cms->c);
size_t len_c = strlen(c);
for (size_t i = 0; i < len_s; i++)
{
if ((s[i] == '*') && ((i < len_c) && c[i] != '*'))
{
ppdebug("MISMATCH: %s\n", c);
return true;
}
}
ppdebug("MATCH: %s\n", c);
}
}
return false;
}
static int pp_prune(Sentence sent, Tracon_sharing *ts, Parse_Options opts)
{
if (sent->postprocessor == NULL) return 0;
if (!opts->perform_pp_prune) return 0;
pp_knowledge *knowledge = sent->postprocessor->knowledge;
multiset_table *cmt = cms_table_new(sent);
Tracon_list *tl = ts->tracon_list;
if (NULL != tl)
{
for (int dir = 0; dir < 2; dir++)
{
for (unsigned int id = 0; id < tl->entries[dir]; id++)
{
Connector *c = get_tracon(ts, dir, id);
if (0 == c->refcount) continue;
insert_in_cms_table(cmt, c, dir);
}
}
}
else
{
for (WordIdx w = 0; w < sent->length; w++)
{
for (Disjunct *d = sent->word[w].d; d != NULL; d = d->next)
{
for (int dir = 0; dir < 2; dir++)
{
Connector *first_c = (dir) ? (d->left) : (d->right);
for (Connector *c = first_c; c != NULL; c = c->next)
{
insert_in_cms_table(cmt, c, dir);
}
}
}
}
}
int D_deleted = 0;
int Cname_deleted = 0;
bool *rule_ok = alloca(knowledge->n_contains_one_rules * sizeof(bool));
memset(rule_ok, true, knowledge->n_contains_one_rules * sizeof(bool));
for (size_t i = 0; i < knowledge->n_contains_one_rules; i++)
{
pp_rule* rule = &knowledge->contains_one_rules[i];
const char *selector = rule->selector;
pp_linkset *link_set = rule->link_set;
unsigned int hash = cms_hash(selector);
for (Cms *cms = cmt->cms_table[hash]; cms != NULL; cms = cms->next)
{
Connector *c = cms->c;
if (c->nearest_word == BAD_WORD) continue;
if (!post_process_match(selector, connector_string(c))) continue;
if (rule->selector_has_wildcard &&
selector_mismatch_wild(cmt, selector, cms)) continue;
ppdebug("Rule %zu: Selector %s, Connector %s\n",
i, selector, connector_string(c));
if (rule_ok[i] && rule_satisfiable(cmt, link_set)) break;
rule_ok[i] = false;
ppdebug("DELETE %s refcount %d\n", connector_string(c), c->refcount);
c->nearest_word = BAD_WORD;
Cname_deleted++;
rule->use_count++;
}
}
if (NULL != tl)
{
for (int dir = 0; dir < 2; dir++)
{
for (unsigned int id = 0; id < tl->entries[dir]; id++)
{
Connector *c = get_tracon(ts, dir, id);
if (0 == c->refcount) continue;
if (mark_bad_connectors(cmt, c))
D_deleted++;
}
}
}
else
{
for (WordIdx w = 0; w < sent->length; w++)
{
for (Disjunct *d = sent->word[w].d; d != NULL; d = d->next)
{
for (int dir = 0; dir < 2; dir++)
{
Connector *first_c = (dir) ? (d->left) : (d->right);
for (Connector *c = first_c; c != NULL; c = c->next)
{
if (mark_bad_connectors(cmt, c))
{
D_deleted++;
break;
}
}
}
}
}
}
lgdebug(+D_PRUNE+1, "Deleted %d (%d connector names)\n",
D_deleted, Cname_deleted);
cms_table_delete(cmt);
print_time(opts, "pp pruning");
return D_deleted;
}
static void get_num_con_uc(Sentence sent,power_table *pt,
unsigned int *num_con_uc[])
{
for (WordIdx w = 0; w < sent->length; w++)
{
for (size_t dir = 0; dir < 2; dir++)
{
C_list **t = pt->table[dir][w];
unsigned int size = pt->table_size[dir][w];
unsigned int count = 0;
for (unsigned int h = 0; h < size; h++)
{
if (NULL == t[h]) continue;
if (!t[h]->c->shallow) continue;
count++;
}
num_con_uc[dir][w] = count;
}
}
}
static void mlink_table_init(Sentence sent, mlink_table *ml)
{
for (WordIdx w = 0; w < sent->length; w++)
{
ml[w] = (mlink_table)
{
.nw[0] = 0, .nw[1] = UNLIMITED_LEN,
.nw_perjet[0] = 0, .nw_perjet[1] = UNLIMITED_LEN,
.nw_unidir[0] = 0, .nw_unidir[1] = UNLIMITED_LEN,
.fw[0] = UNLIMITED_LEN, .fw[1] = 0,
};
}
}
static mlink_table *build_mlink_table(Sentence sent, mlink_table *ml)
{
bool ml_exists = false;
bool *nojet[2];
nojet[0] = alloca(2 * sent->length * sizeof(bool));
nojet[1] = nojet[0] + sent->length;
memset(nojet[0], false, 2 * sent->length * sizeof(bool));
mlink_table_init(sent, ml);
for (WordIdx w = 0; w < sent->length; w++)
{
if (sent->word[w].optional) continue;
for (Disjunct *d = sent->word[w].d; d != NULL; d = d->next)
{
if (NULL == d->left)
{
nojet[0][w] = true;
ml[w].fw[0] = 0;
}
else
{
if (NULL == d->right)
{
if (d->left->nearest_word > ml[w].nw_unidir[0])
ml[w].nw_unidir[0] = d->left->nearest_word;
}
else
{
if (d->left->nearest_word > ml[w].nw[0])
ml[w].nw[0] = d->left->nearest_word;
}
if (d->left->farthest_word < ml[w].fw[0])
ml[w].fw[0] = d->left->farthest_word;
}
if (NULL == d->right)
{
nojet[1][w] = true;;
ml[w].fw[1] = UNLIMITED_LEN;
}
else
{
if (NULL == d->left)
{
if (d->right->nearest_word < ml[w].nw_unidir[1])
ml[w].nw_unidir[1] = d->right->nearest_word;
}
else
{
if (d->right->nearest_word < ml[w].nw[1])
ml[w].nw[1] = d->right->nearest_word;
}
if (d->right->farthest_word > ml[w].fw[1])
ml[w].fw[1] = d->right->farthest_word;
}
}
ml_exists |= (!nojet[0][w] || !nojet[1][w]);
}
if (ml_exists)
{
for (WordIdx w = 0; w < sent->length; w++)
{
if (sent->word[w].optional) continue;
if (ml[w].nw_unidir[0] > ml[w].nw[0])
ml[w].nw[0] = ml[w].nw_unidir[0];
if (ml[w].nw_unidir[1] < ml[w].nw[1])
ml[w].nw[1] = ml[w].nw_unidir[1];
for (int dir = 0; dir < 2; dir++)
{
ml[w].nw_perjet[dir] = ml[w].nw[dir];
if (nojet[dir][w])
ml[w].nw[dir] = w;
}
}
}
if (verbosity_level(+D_PRUNE) && ml_exists)
{
prt_error("\n");
for (WordIdx w = 0; w < sent->length; w++)
{
if (sent->word[w].optional) continue;
if (ml[w].nw[0] != ml[w].nw[1])
{
prt_error("%3zu: nearest_word (%3d %3d)", w,
w==ml[w].nw[0]?-1:ml[w].nw[0],
w==ml[w].nw[1]?-1:ml[w].nw[1]);
prt_error("     farthest_word (%3d %3d)\n\\",
w==ml[w].nw[0]?-1:ml[w].fw[0],
w==ml[w].nw[1]?-1:ml[w].fw[1]);
}
}
lg_error_flush();
}
return ml_exists ? ml : NULL;
}
static unsigned int cross_mlink_prune(Sentence sent, mlink_table *ml)
{
int N_deleted[2] = {0};
static Connector bad_connector = { .nearest_word = BAD_WORD };
for (unsigned int w = 0; w < sent->length; w++)
{
if (sent->word[w].optional) continue;
if (sent->word[w].d == NULL) continue;
WordIdx_m nw0 = ml[w].nw[0];
WordIdx_m nw1 = ml[w].nw[1];
WordIdx_m fw0 = ml[w].fw[0];
WordIdx_m fw1 = ml[w].fw[1];
if ((w > 0) && (nw1 != w))
{
for (Disjunct *d = sent->word[nw1].d; d != NULL; d = d->next)
{
Connector *shallow_c = d->left;
if (shallow_c == NULL)
{
if ((nw1 == fw1) || ((d->right->nearest_word > fw1) && PR(1)))
{
d->left = &bad_connector;
N_deleted[0]++;
}
continue;
}
if (shallow_c->nearest_word == BAD_WORD)
{
N_deleted[1]++;
continue;
}
Connector *c = connector_deepest(shallow_c);
if (c->nearest_word < w)
{
shallow_c->nearest_word = BAD_WORD;
N_deleted[0]++;
continue;
}
if (!c->multi)
c->farthest_word = MAX(w, c->farthest_word);
}
}
if ((w < sent->length-1) && (nw0 != w))
{
for (Disjunct *d = sent->word[nw0].d; d != NULL; d = d->next)
{
Connector *shallow_c = d->right;
if (shallow_c == NULL)
{
if ((nw0 == fw0) || ((d->left->nearest_word < fw0) && PR(0)))
{
d->right = &bad_connector;
N_deleted[0]++;
PR(0);
}
continue;
}
if (shallow_c->nearest_word == BAD_WORD)
{
N_deleted[1]++;
continue;
}
Connector *c = connector_deepest(shallow_c);
if (c->nearest_word > w)
{
shallow_c->nearest_word = BAD_WORD;
N_deleted[0]++;
continue;
}
if (!c->multi)
c->farthest_word = MIN(w, c->farthest_word);
}
}
for (unsigned int rw = w+1; rw < nw1; rw++)
{
for (Disjunct *d = sent->word[rw].d; d != NULL; d = d->next)
{
Connector *shallow_c = d->left;
if (shallow_c == NULL) continue;
if (shallow_c->nearest_word == BAD_WORD)
{
N_deleted[1]++;
continue;
}
if (shallow_c->nearest_word < w)
{
shallow_c->nearest_word = BAD_WORD;
N_deleted[0]++;
continue;
}
shallow_c->farthest_word = MAX(w, shallow_c->farthest_word);
if (d->right != NULL)
d->right->farthest_word = MIN(fw1, d->right->farthest_word);
}
}
for (unsigned int lw = nw0+1; lw < w; lw++)
{
for (Disjunct *d = sent->word[lw].d; d != NULL; d = d->next)
{
Connector *shallow_c = d->right;
if (shallow_c == NULL) continue;
if (shallow_c->nearest_word == BAD_WORD)
{
N_deleted[1]++;
continue;
}
if (shallow_c->nearest_word > w)
{
shallow_c->nearest_word = BAD_WORD;
N_deleted[0]++;
continue;
}
shallow_c->farthest_word = MIN(w, shallow_c->farthest_word);
if (d->left != NULL)
d->left->farthest_word = MAX(fw0, d->left->farthest_word);
}
}
}
lgdebug(+D_PRUNE, "Debug: [nw] detected %d (%d+%d)\n",
N_deleted[0] + N_deleted[1], N_deleted[0], N_deleted[1]);
return N_deleted[0] + N_deleted[1];
}
unsigned int pp_and_power_prune(Sentence sent, Tracon_sharing *ts,
unsigned int null_count, Parse_Options opts,
unsigned int *ncu[2])
{
prune_context pc = {0};
power_table pt;
power_table_init(sent, ts, &pt);
bool no_mlink = !!test_enabled("no-mlink");
mlink_table *ml = alloca(sent->length * sizeof(*pc.ml));
pc.always_parse = test_enabled("always-parse");
pc.sent = sent;
pc.pt = &pt;
pc.null_links = null_count;
pc.islands_ok = opts->islands_ok;
pc.is_null_word = alloca(sent->length * sizeof(*pc.is_null_word));
memset(pc.is_null_word, 0, sent->length * sizeof(*pc.is_null_word));
int num_deleted = power_prune(sent, &pc, opts);
if ((num_deleted > 0) && !no_mlink)
{
pc.ml = build_mlink_table(sent, ml);
print_time(opts, "Built mlink_table%s", pc.ml ? "" : " (empty)");
if (pc.ml != NULL)
{
if (null_count == 0)
cross_mlink_prune(sent, pc.ml);
num_deleted = power_prune(sent, &pc, opts);
}
}
if (num_deleted != -1)
{
if (pp_prune(sent, ts, opts) > 0)
num_deleted = power_prune(sent, &pc, opts);
if ((num_deleted > 0) && !no_mlink)
{
pc.ml = build_mlink_table(sent, ml);
print_time(opts, "Built mlink_table%s", pc.ml ? "" : " (empty)");
if (pc.ml != NULL)
{
if (null_count == 0)
cross_mlink_prune(sent, pc.ml);
power_prune(sent, &pc, opts);
}
}
}
unsigned int min_nulls = sent->null_count;
bool parsing_to_be_done = true;
if (null_count == MAX_SENTENCE)
{
min_nulls = pc.null_words;
}
else if ((pc.null_words > sent->null_count) && !pc.always_parse)
{
min_nulls = sent->null_count + 1;
parsing_to_be_done = false;
}
if (parsing_to_be_done)
get_num_con_uc(sent, &pt, ncu);
power_table_delete(&pt);
return min_nulls;
}