#include "api-structures.h"
#include "api-types.h"
#include "dict-common/regex-morph.h"
#include "connectors.h"
#include "disjunct-utils.h"
#include "error.h"
#include "linkage.h"
#include "sane.h"
#include "tokenize/tok-structures.h"
#include "tokenize/word-structures.h"
#include "tokenize/wordgraph.h"
#include "utilities.h"
struct Wrdgr_path_s
{
Gword *word;
const Gword **path;
};
typedef struct Wrdgr_path_s Wrdgr_pathpos;
static size_t wrdgr_path_len(Wrdgr_pathpos *wp)
{
size_t len = 0;
if (wp)
while (wp[len].word != NULL) len++;
return len;
}
static Wrdgr_pathpos *wrdgr_path_resize(Wrdgr_pathpos *wp, size_t len)
{
wp = realloc(wp, (len+1) * sizeof(*wp));
wp[len].word = NULL;
return wp;
}
static void wrdgr_path_free(Wrdgr_pathpos *wp)
{
free(wp);
}
#define D_WPA 7
static void wordgraph_path_append(Wrdgr_pathpos **nwp, const Gword **path,
Gword *current_word,
Gword *p)
{
size_t n = wrdgr_path_len(*nwp);
assert(NULL != p, "Tried to add a NULL word to the word queue");
if (current_word == p)
{
lgdebug(D_WPA, "Adding the same word '%s' again\n", p->subword);
}
const Wrdgr_pathpos *wpt = NULL;
if (NULL != *nwp)
{
for (wpt = *nwp; NULL != wpt->word; wpt++)
{
if (p == wpt->word)
{
lgdebug(D_WPA, "Word %s (after %zu) exists (after %d)\n",
p->subword,
wpt->path[gwordlist_len(wpt->path)-1]->sent_wordidx,
path ? (int)path[gwordlist_len(path)-1]->sent_wordidx : -1);
if ((NULL != path) &&
wpt->path[gwordlist_len(wpt->path)-1]->sent_wordidx <=
path[gwordlist_len(path)-1]->sent_wordidx)
{
lgdebug(D_WPA, "Shorter path already queued\n");
return;
}
lgdebug(D_WPA, "Longer path is in the queue\n");
gwordlist_cfree(wpt->path);
break;
}
}
}
if ((NULL == wpt) || (p != wpt->word))
{
*nwp = wrdgr_path_resize(*nwp, n+1);
}
else
{
lgdebug(D_WPA, "Path position to be replaced (len %zu): %d\n", n,
(int)(wpt - *nwp));
n = wpt - *nwp;
}
(*nwp)[n].word = p;
(*nwp)[n].path = gwordlist_copy(path);
if (NULL == current_word) return;
if (p != current_word)
{
gwordlist_append((Gword ***)&(*nwp)[n].path, current_word);
}
}
static void wordgraph_path_free(Wrdgr_pathpos *wp, bool free_final_path)
{
Wrdgr_pathpos *twp;
if (NULL == wp) return;
for (twp = wp; NULL != twp->word; twp++)
{
if (free_final_path || (MT_INFRASTRUCTURE != twp->word->morpheme_type))
gwordlist_cfree(twp->path);
}
wrdgr_path_free(wp);
}
#define NO_WORD (MAX_SENTENCE+1)
static size_t num_islands(const Linkage lkg, const Gword **wg_path)
{
struct word
{
int prev;
int next;
int inum;
};
struct word *word = alloca(lkg->sent->length * sizeof(struct word));
for (WordIdx w = 0; w < lkg->sent->length; w++)
{
word[w].prev = word[w].next = w;
}
for (LinkIdx li = 0; li < lkg->num_links; li++)
{
Link *l = &lkg->link_array[li];
WordIdx iw;
for (iw = word[l->lw].next; (iw != l->rw) && (iw != l->lw); iw = word[iw].next)
;
if (iw != l->rw)
{
int nextl = word[l->lw].next;
int prevr = word[l->rw].prev;
word[l->lw].next = l->rw;
word[l->rw].prev = l->lw;
word[prevr].next = nextl;
word[nextl].prev = prevr;
}
if (verbosity_level(+8))
{
for (WordIdx w = 0; w < lkg->sent->length; w++)
{
err_msg(lg_Debug, "%d<-%zu->%d ", word[w].prev, w, word[w].next);
}
err_msg(lg_Debug, "\n");
}
}
int inum = -1;
Disjunct **cdj = lkg->chosen_disjuncts;
for (WordIdx w = 0; w < lkg->sent->length; w++)
{
if ((NULL == *wg_path) || ((*wg_path)->sent_wordidx != w))
{
assert(word[w].prev == word[w].next,
"A skipped optional word found in an island");
assert((NULL == cdj[w]) && lkg->sent->word[w].optional,
"A matching disjunct found for a skipped optional word");
word[w].prev = NO_WORD;
word[w].inum = -1;
continue;
}
wg_path++;
if (NO_WORD == word[w].prev) continue;
inum++;
for (WordIdx iw = w; NO_WORD != word[iw].prev; iw = word[iw].next)
{
word[iw].prev = NO_WORD;
word[iw].inum = inum;
}
}
if (verbosity_level(8))
{
err_msg(lg_Debug, "Island count %d: ", inum);
for (WordIdx w = 0; w < lkg->sent->length; w++)
{
err_msg(lg_Debug, "%d ", word[w].inum);
}
err_msg(lg_Debug, "\n");
}
return inum;
}
#define AFFIXTYPE_PREFIX 'p'
#define AFFIXTYPE_STEM 't'
#define AFFIXTYPE_SUFFIX 's'
#define AFFIXTYPE_MIDDLE 'm'
#define AFFIXTYPE_WORD 'w'
#ifdef WORD_BOUNDARIES
#define AFFIXTYPE_END 'b'
#endif
#define D_SLM 8
bool sane_linkage_morphism(Sentence sent, Linkage lkg, Parse_Options opts)
{
Wrdgr_pathpos *wp_new = NULL;
Wrdgr_pathpos *wp_old = NULL;
Wrdgr_pathpos *wpp = NULL;
Gword **next;
size_t i;
unsigned int null_count_found = 0;
bool match_found = true;
Gword **lwg_path = NULL;
Dictionary afdict = sent->dict->affix_table;
char *const affix_types = alloca(sent->length*2 + 1);
affix_types[0] = '\0';
lkg->wg_path = NULL;
for (next = sent->wordgraph->next; *next; next++)
{
wordgraph_path_append(&wp_new, NULL, NULL, *next);
}
assert(NULL != wp_new, "Path word queue is empty");
for (i = 0; i < lkg->num_words; i++)
{
Disjunct *cdj;
lgdebug(D_SLM, "lkg=%p Word %zu: ", lkg, i);
if (NULL == wp_new)
{
lgdebug(D_SLM, "- No more words in the wordgraph\n");
match_found = false;
break;
}
if (wp_old != wp_new)
{
wordgraph_path_free(wp_old, true);
wp_old = wp_new;
}
wp_new = NULL;
cdj = lkg->chosen_disjuncts[i];
if (NULL == cdj)
{
lgdebug(D_SLM, "- Null word");
match_found = false;
bool optional_word_found = false;
for (wpp = wp_old; NULL != wpp->word; wpp++)
{
if ((MT_INFRASTRUCTURE == wpp->word->morpheme_type) ||
(wpp->word->sent_wordidx > i))
{
assert(sent->word[i].optional, "wordindex=%zu", i);
lgdebug(D_SLM, " (Optional, index=%zu)\n", i);
wordgraph_path_append(&wp_new, wpp->path, wpp->word, wpp->word);
match_found = true;
optional_word_found = true;
continue;
}
for (next = wpp->word->next; NULL != *next; next++)
{
if (MT_INFRASTRUCTURE != wpp->word->morpheme_type)
match_found = true;
wordgraph_path_append(&wp_new, wpp->path, wpp->word, *next);
}
}
if (!optional_word_found)
{
null_count_found++;
if ((null_count_found > lkg->sent->null_count) &&
(lkg->sent->null_count != sent->length-1))
{
lgdebug(D_SLM, " (Extra, count > %u)\n", lkg->sent->null_count);
match_found = false;
break;
}
lgdebug(D_SLM, "\n");
}
continue;
}
if (!match_found)
{
const char *e = "Internal error: Too many words in the linkage";
lgdebug(D_SLM, "- %s\n", e);
prt_error("Error: %s.\n", e);
break;
}
if (verbosity_level(D_SLM))
{
if (cdj->is_category == 0)
prt_error("%s", cdj->word_string);
else
prt_error("Category[0]:%u", cdj->category[0].num);
}
match_found = false;
for (wpp = wp_old; NULL != wpp->word; wpp++)
{
for (gword_set *gl = cdj->originating_gword; NULL != gl; gl = gl->next)
{
if (gl->o_gword == wpp->word)
{
match_found = true;
for (next = wpp->word->next; NULL != *next; next++)
{
wordgraph_path_append(&wp_new, wpp->path, wpp->word, *next);
}
break;
}
}
}
if (!match_found)
{
lgdebug(D_SLM, "- No Wordgraph match\n");
break;
}
lgdebug(D_SLM, "\n");
}
if (match_found)
{
match_found = false;
if (NULL != wp_new)
{
for (wpp = wp_new; NULL != wpp->word; wpp++)
{
if (MT_INFRASTRUCTURE == wpp->word->morpheme_type) {
match_found = true;
break;
}
}
}
if (!match_found)
lgdebug(D_SLM, "%p Missing word(s) at the end of the linkage.\n", lkg);
}
if (match_found)
{
unsigned int count_found =
opts->islands_ok ? num_islands(lkg, wpp->path) : null_count_found;
if ((count_found != lkg->sent->null_count) &&
(lkg->sent->null_count != sent->length-1) && (count_found != sent->length))
{
lgdebug(D_SLM, "Null count mismatch: Found %u != null_count %u\n",
count_found, lkg->sent->null_count);
match_found = false;
}
}
#define DEBUG_morpheme_type 0
if (match_found && (0 == sent->null_count) &&
(NULL != afdict) && (NULL != afdict->regex_root))
{
const Gword **w;
char *affix_types_p = affix_types;
#if DEBUG_morpheme_type
print_lwg_path(wpp->path, "Linkage");
#endif
i = 0;
for (w = wpp->path; *w; w++)
{
i++;
PRAGMA_START(GCC diagnostic ignored "-Wswitch-enum")
switch ((*w)->morpheme_type)
{
default:
case MT_WORD:
*affix_types_p = AFFIXTYPE_WORD;
break;
case MT_PREFIX:
*affix_types_p = AFFIXTYPE_PREFIX;
break;
case MT_STEM:
*affix_types_p = AFFIXTYPE_STEM;
break;
case MT_MIDDLE:
*affix_types_p = AFFIXTYPE_MIDDLE;
break;
case MT_SUFFIX:
*affix_types_p = AFFIXTYPE_SUFFIX;
break;
}
PRAGMA_END
#if DEBUG_morpheme_type
lgdebug(D_SLM, "Word %zu: %s affixtype=%c\n",
i, (*w)->subword, *affix_types_p);
#endif
affix_types_p++;
}
*affix_types_p = '\0';
#ifdef WORD_BOUNDARIES
{
const Gword *uw;
uw = word_boundary(w);
if (NULL != uw)
{
*affix_types_p++ = AFFIXTYPE_END;
lgdebug(D_SLM, "%p End of Gword %s\n", lkg, uw->subword);
}
}
#endif
if (('\0' != affix_types[0]) &&
(NULL == match_regex(afdict->regex_root, affix_types)))
{
match_found = false;
if (0 < opts->verbosity)
prt_error("Warning: Invalid morpheme type combination '%s'.\n"
"Run with !bad and !verbosity>"STRINGIFY(D_USER_MAX)
" to debug\n", affix_types);
}
}
if (match_found) lwg_path = (Gword **)wpp->path;
wordgraph_path_free(wp_old, true);
wordgraph_path_free(wp_new, !match_found);
if (match_found)
{
if ('\0' != affix_types[0])
{
lgdebug(D_SLM, "%p Morpheme type combination '%s'\n", lkg, affix_types);
}
lgdebug(+D_SLM-1, "%p SUCCEEDED\n", lkg);
lkg->wg_path = lwg_path;
return true;
}
lgdebug(+D_SLM-1, "%p FAILED\n", lkg);
return false;
}
#undef D_SLM