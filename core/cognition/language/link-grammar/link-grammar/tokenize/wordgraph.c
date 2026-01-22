#include <stdlib.h>
#include "api-structures.h"
#include "error.h"
#include "string-set.h"
#include "tok-structures.h"
#include "tokenize.h"
#include "wordgraph.h"
Gword *gword_new(Sentence sent, const char *s)
{
Gword *gword = malloc(sizeof(Gword));
memset(gword, 0, sizeof(Gword));
assert(NULL != s, "Null-string subword");
assert(0 != *s, "Empty-string subword");
gword->subword = string_set_add(s, sent->string_set);
if (NULL != sent->last_word) sent->last_word->chain_next = gword;
sent->last_word = gword;
gword->node_num = sent->gword_node_num++;
gword->gword_set_head = (gword_set){0};
gword->gword_set_head.o_gword = gword;
return gword;
}
static Gword **gwordlist_resize(Gword **arr, size_t len)
{
arr = realloc(arr, (len+2) * sizeof(Gword *));
arr[len+1] = NULL;
return arr;
}
size_t gwordlist_len(const Gword **arr)
{
size_t len = 0;
if (arr)
while (arr[len] != NULL) len++;
return len;
}
void gwordlist_append(Gword ***arrp, Gword *p)
{
size_t n = gwordlist_len((const Gword **)*arrp);
*arrp = gwordlist_resize(*arrp, n);
(*arrp)[n] = p;
}
void gwordlist_free(Gword ** gw)
{
free(gw);
}
void gwordlist_cfree(const Gword ** gw)
{
free(gw);
}
const Gword ** gwordlist_copy(const Gword ** path)
{
if (NULL == path) return NULL;
size_t path_arr_size = (gwordlist_len(path)+1)*sizeof(*path);
const Gword ** pcp = malloc(path_arr_size);
memcpy(pcp, path, path_arr_size);
return pcp;
}
#if 0
void gwordlist_append_list(const Gword ***to_word, const Gword **from_word)
{
size_t to_word_arr_len = gwordlist_len(*to_word);
for (const Gword **f = from_word; NULL != *f; f++)
{
size_t l;
for (l = 0; l < to_word_arr_len; l++)
if (*f == (*to_word)[l]) break;
if (l == to_word_arr_len)
gwordlist_append((Gword ***)to_word, (Gword *)*f);
}
}
static void wordlist_replace(Gword ***arrp, size_t start, size_t count,
const Gword *wnew)
{
size_t n = gwordlist_len((const Gword **)(*arrp+start+count));
memmove(*arrp+start+1, *arrp+start+count, (n+1) * sizeof(Gword *));
(*arrp)[start] = (Gword *)wnew;
}
#endif
size_t wordgraph_pathpos_len(Wordgraph_pathpos *wp)
{
size_t len = 0;
if (wp)
while (wp[len].word != NULL) len++;
return len;
}
Wordgraph_pathpos *wordgraph_pathpos_resize(Wordgraph_pathpos *wp,
size_t len)
{
wp = realloc(wp, (len+1) * sizeof(*wp));
wp[len].word = NULL;
return wp;
}
bool wordgraph_pathpos_add(Wordgraph_pathpos **wp, Gword *p, bool used,
bool same_word, bool diff_alternative)
{
size_t n = wordgraph_pathpos_len(*wp);
Wordgraph_pathpos *wpt;
size_t insert_here = n;
assert(NULL != p, "No Gword to insert");
#ifdef DEBUG
if (verbosity_level(+9)) print_hier_position(p);
#endif
if (NULL != *wp)
{
for (wpt = *wp; NULL != wpt->word; wpt++)
{
if (p == wpt->word)
return false;
if ((n == insert_here) && (p->hier_depth >= wpt->word->hier_depth))
insert_here = wpt - *wp;
if (diff_alternative)
{
assert(same_word||wpt->same_word||!in_same_alternative(p,wpt->word),
"wordgraph_pathpos_add(): "
"Word%zu '%s' is from same alternative of word%zu '%s'",
p->node_num, p->subword,
wpt->word->node_num, wpt->word->subword);
}
}
}
*wp = wordgraph_pathpos_resize(*wp, n+1);
if (insert_here < n)
{
memmove(&(*wp)[insert_here+1], &(*wp)[insert_here],
(n+1 - insert_here) * sizeof (*wpt));
}
(*wp)[insert_here].word = p;
(*wp)[insert_here].same_word = same_word;
(*wp)[insert_here].used = used;
(*wp)[insert_here].next_ok = false;
return true;
}
void wordgraph_pathpos_free(Wordgraph_pathpos *wp)
{
free(wp);
}
void print_lwg_path(Gword **w, const char *title)
{
lgdebug(+0, "%s: ", title);
for (; *w; w++) lgdebug(0, "%s ", (*w)->subword);
lgdebug(0, "\n");
}
#ifdef DEBUG
GNUC_UNUSED static const char *debug_show_subword(const Gword *w)
{
return w->unsplit_word ? w->subword : "S";
}
GNUC_UNUSED void print_hier_position(const Gword *word)
{
const Gword **p;
err_msg(lg_Debug, "[Word %zu:%s hier_position(hier_depth=%zu): ",
word->node_num, word->subword, word->hier_depth);
assert(2*word->hier_depth==gwordlist_len(word->hier_position), "word '%s'",
word->subword);
for (p = word->hier_position; NULL != *p; p += 2)
{
err_msg(lg_Debug, "(%zu:%s/%zu:%s)",
p[0]->node_num, debug_show_subword(p[0]),
p[1]->node_num, debug_show_subword(p[1]));
}
err_msg(lg_Debug, "]\n");
}
GNUC_UNUSED void gword_set_print(const gword_set *gs)
{
printf("Gword list: ");
if (NULL == gs)
{
printf("(null)\n");
return;
}
for (; NULL != gs; gs = gs->next)
{
printf("word %p '%s' unsplit '%s'%s", gs->o_gword, (gs->o_gword)->subword,
(gs->o_gword)->unsplit_word->subword, NULL==gs->next ? "" : ", ");
}
printf("\n");
}
#endif
static Gword *find_alternative(Gword *word)
{
assert(NULL != word, "find_alternative(NULL)");
assert(NULL != word->alternative_id, "find_alternative(%s): NULL id",
word->subword);
#if 0
lgdebug(+0, "find_alternative(%s): '%s'\n",
word->subword, debug_show_subword(word->alternative_id));
#endif
return word->alternative_id;
}
const Gword **wordgraph_hier_position(Gword *word)
{
const Gword **hier_position;
size_t i = 0;
Gword *w;
bool is_leaf = true;
if (NULL != word->hier_position) return word->hier_position;
for (w = find_real_unsplit_word(word, true); NULL != w; w = w->unsplit_word)
i++;
if (0 == i) i = 1;
word->hier_depth = i - 1;
i = (2 * word->hier_depth)+1;
hier_position = malloc(i * sizeof(*hier_position));
hier_position[--i] = NULL;
w = word;
while (0 != i)
{
hier_position[--i] = find_alternative(w);
w = find_real_unsplit_word(w, is_leaf);
hier_position[--i] = w;
is_leaf = false;
}
word->hier_position = hier_position;
return hier_position;
}
bool in_same_alternative(Gword *w1, Gword *w2)
{
const Gword **hp1 = wordgraph_hier_position(w1);
const Gword **hp2 = wordgraph_hier_position(w2);
size_t i;
#if 0
print_hier_position(w1); print_hier_position(w2);
#endif
#if 0
if ((NULL == w1->next) || (NULL == w2->next)) return false;
#endif
for (i = 0; (NULL != hp1[i]) && (NULL != hp2[i]); i++)
{
if (hp1[i] != hp2[i]) break;
}
if (0 == i%2) return true;
return false;
}
Gword *find_real_unsplit_word(Gword *word, bool is_leaf)
{
if (NULL == word->unsplit_word)
return word;
if (is_leaf && (word->status & WS_UNSPLIT))
return word;
return word->unsplit_word;
}
Gword *wg_get_sentence_word(const Sentence sent, Gword *word)
{
if (MT_INFRASTRUCTURE == word->morpheme_type) return NULL;
while (!IS_SENTENCE_WORD(sent, word))
{
word = word->unsplit_word;
assert(NULL != word, "NULL unsplit word");
}
assert(NULL != word->subword, "NULL subword");
return word;
}
const char *gword_status(Sentence sent, const Gword *w)
{
dyn_str *s = dyn_str_new();
const char *r;
size_t len;
if (w->status & WS_UNKNOWN)
dyn_strcat(s, "UNK|");
if (w->status & WS_INDICT)
dyn_strcat(s, "IN|");
if (w->status & WS_REGEX)
dyn_strcat(s, "RE|");
if (w->status & WS_SPELL)
dyn_strcat(s, "SP|");
if (w->status & WS_RUNON)
dyn_strcat(s, "RU|");
if (w->status & WS_HASALT)
dyn_strcat(s, "HA|");
if (w->status & WS_UNSPLIT)
dyn_strcat(s, "UNS|");
if (w->status & WS_PL)
dyn_strcat(s, "PL|");
char *status_str = dyn_str_take(s);
len = strlen(status_str);
if (len > 0) status_str[len-1] = '\0';
r = string_set_add(status_str, sent->string_set);
free(status_str);
return r;
}
#ifdef DEBUG
GNUC_UNUSED static int gword_set_len(const gword_set *gl)
{
int len = 0;
for (; NULL != gl; gl = gl->next) len++;
return len;
}
#endif
static gword_set *gword_set_element_new(gword_set *old_e)
{
gword_set *new_e = malloc(sizeof(gword_set));
*new_e = (gword_set){0};
new_e->o_gword = old_e->o_gword;
gword_set *chain_next = old_e->chain_next;
old_e->chain_next = new_e;
new_e->chain_next = chain_next;
return new_e;
}
static void gword_set_element_free(gword_set * e)
{
free(e);
}
static gword_set *gword_set_add(gword_set *gset, gword_set *ge)
{
gword_set *n = gword_set_element_new(ge);
n->next = gset;
gset = n;
return gset;
}
gword_set *gword_set_union(gword_set *kept, gword_set *eliminated)
{
gword_set *preserved_set = NULL;
for (gword_set *e = eliminated; NULL != e; e = e->next)
{
gword_set *k;
for (k = kept; NULL != k; k = k->next)
if (e->o_gword == k->o_gword) break;
if (NULL != k) continue;
preserved_set = gword_set_add(preserved_set, e);
}
if (preserved_set)
{
for (gword_set *k = kept; NULL != k; k = k->next)
preserved_set = gword_set_add(preserved_set, k);
kept = preserved_set;
}
return kept;
}
static void word_queue_delete(Sentence sent)
{
word_queue_t *wq = sent->word_queue;
while (NULL != wq)
{
word_queue_t *wq_tofree = wq;
wq = wq->next;
free(wq_tofree);
}
sent->word_queue = NULL;
sent->word_queue_last = NULL;
}
static void gword_set_delete(Gword *w)
{
if (NULL == w) return;
for (w = w->chain_next; NULL != w; w = w->chain_next)
{
gword_set *n;
for (gword_set *f = w->gword_set_head.chain_next; NULL != f; f = n)
{
n = f->chain_next;
gword_set_element_free(f);
}
}
}
void wordgraph_delete(Sentence sent)
{
word_queue_delete(sent);
Gword *w = sent->wordgraph;
gword_set_delete(w);
while (NULL != w)
{
Gword *w_tofree = w;
free(w->prev);
free(w->next);
free(w->hier_position);
free(w->null_subwords);
w = w->chain_next;
free(w_tofree);
}
sent->last_word = NULL;
sent->wordgraph = NULL;
}