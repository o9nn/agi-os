#include <stdint.h>
#include <stdlib.h>
#include "api-structures.h"
#include "connectors.h"
#include "dict-common/dict-api.h"
#include "dict-common/dict-affix.h"
#include "dict-common/dict-defines.h"
#include "dict-common/idiom.h"
#include "disjunct-utils.h"
#include "link-includes.h"
#include "linkage.h"
#include "lisjuncts.h"
#include "sat-solver/sat-encoder.h"
#include "string-set.h"
#include "tokenize/wordgraph.h"
#include "tokenize/tok-structures.h"
#include "tokenize/word-structures.h"
#define INFIX_MARK_L 1
#define STEM_MARK_L  1
#define NULLWORD_START '['
#define NULLWORD_END   ']'
static void add_morpheme_unmarked(Sentence sent, char *join_buff,
const char *wm, Morpheme_type mt)
{
const char infix_mark = INFIX_MARK(sent->dict->affix_table);
const char *sm =  get_word_subscript(wm);
if (NULL == sm) sm = (char *)wm + strlen(wm);
if ((MT_PREFIX == mt) && (infix_mark == sm[-INFIX_MARK_L]))
strncat(join_buff, wm, sm-wm-INFIX_MARK_L);
else if ((MT_SUFFIX == mt) && (infix_mark == wm[0]))
strncat(join_buff, INFIX_MARK_L+wm, sm-wm-INFIX_MARK_L);
else if ((MT_MIDDLE == mt))
strncat(join_buff, INFIX_MARK_L+wm, sm-wm-2*INFIX_MARK_L);
else
strncat(join_buff, wm, sm-wm);
}
static const char *join_null_word(Sentence sent, Gword **wgp, size_t count)
{
size_t i;
char *join_buff;
const char *s;
size_t join_len = 0;
for (i = 0; i < count; i++)
join_len += strlen(wgp[i]->subword);
join_buff = alloca(join_len+1);
join_buff[0] = 0;
for (i = 0; i < count; i++)
add_morpheme_unmarked(sent, join_buff, wgp[i]->subword,
wgp[i]->morpheme_type);
s = string_set_add(join_buff, sent->string_set);
return s;
}
static Gword *wordgraph_null_join(Sentence sent, Gword **start, Gword **end)
{
Gword *new_word;
Gword **w;
char *usubword;
size_t join_len = 0;
for (w = start; w <= end; w++) join_len += strlen((*w)->subword);
usubword = alloca(join_len+1);
usubword[0] = 0;
for (w = start; w <= end; w++)
add_morpheme_unmarked(sent, usubword, (*w)->subword, (*w)->morpheme_type);
new_word = gword_new(sent, usubword);
new_word->status |= WS_PL;
new_word->label = "NJ";
new_word->null_subwords = NULL;
new_word->start = (*start)->start;
new_word->end = (*end)->end;
for (w = start; w <= end; w++)
gwordlist_append(&new_word->null_subwords, (Gword *)(*w));
return new_word;
}
static Gword *wordgraph_link_placeholder(Sentence sent, Gword *w)
{
Gword *new_word;
new_word = gword_new(sent, "PLACE_HOLDER");
new_word->status |= WS_PL;
new_word->label = "PH";
new_word->start = w->start;
new_word->end = w->end;
return new_word;
}
#define SUBSCRIPT_SEP SUBSCRIPT_DOT
#define PREFIX_SUPPRESS ("PL")
#define PREFIX_SUPPRESS_L 2
#define SUFFIX_SUPPRESS ("LL")
#define SUFFIX_SUPPRESS_L 2
#define HIDE_MORPHO   (('\0' != infix_mark) && !display_morphology)
#define DISPLAY_GUESS_MARKS true
static inline bool is_morphology_link(const char *link_name)
{
if (NULL == link_name) return false;
return (0 == strncmp(link_name, SUFFIX_SUPPRESS, SUFFIX_SUPPRESS_L)) ||
(0 == strncmp(link_name, PREFIX_SUPPRESS, PREFIX_SUPPRESS_L));
}
static void remap_linkages(Linkage lkg, const int *remap)
{
LinkIdx i, j;
for (i = 0, j = 0; i < lkg->num_links; i++)
{
Link *old_lnk = &lkg->link_array[i];
if (NULL != old_lnk->link_name &&
(-1 != remap[old_lnk->rw]) && (-1 != remap[old_lnk->lw]))
{
Link *new_lnk = &lkg->link_array[j];
Connector *ctmp;
new_lnk->lw = remap[old_lnk->lw];
new_lnk->rw = remap[old_lnk->rw];
ctmp = new_lnk->lc;
new_lnk->lc = old_lnk->lc;
old_lnk->lc = ctmp;
ctmp = new_lnk->rc;
new_lnk->rc = old_lnk->rc;
old_lnk->rc = ctmp;
new_lnk->link_name = old_lnk->link_name;
j++;
}
}
lkg->num_links = j;
}
#define D_REE 7
void remove_empty_words(Linkage lkg)
{
size_t i, j;
Disjunct **cdj = lkg->chosen_disjuncts;
int *remap = alloca(lkg->num_words * sizeof(*remap));
Gword **wgp = lkg->wg_path;
for (i = 0, j = 0; i < lkg->num_words; i++)
{
if ((NULL == *wgp) || ((*wgp)->sent_wordidx != i))
{
assert((NULL == cdj[i]) && lkg->sent->word[i].optional,
"A matching disjunct found for a skipped optional word");
remap[i] = -1;
continue;
}
#if USE_SAT_SOLVER
Disjunct *cdtmp = cdj[j];
#endif
cdj[j] = cdj[i];
#if USE_SAT_SOLVER
cdj[i] = cdtmp;
#endif
remap[i] = j;
j++;
wgp++;
}
if (lkg->num_words != j)
{
lkg->num_words = j;
remap_linkages(lkg, remap);
}
}
#undef D_REE
#define D_CCW 8
static void compute_chosen_words(Sentence sent, Linkage linkage,
Parse_Options opts)
{
WordIdx i;
WordIdx j;
Disjunct **cdjp = linkage->chosen_disjuncts;
const char **chosen_words = alloca(linkage->num_words * sizeof(*chosen_words));
int *remap = alloca(linkage->num_words * sizeof(*remap));
bool *show_word = alloca(linkage->num_words * sizeof(*show_word));
bool display_morphology = opts->display_morphology;
const char infix_mark = INFIX_MARK(sent->dict->affix_table);
Gword **lwg_path = linkage->wg_path;
Gword **n_lwg_path = NULL;
Gword **nullblock_start = NULL;
size_t nbsize = 0;
Gword *sentence_word;
memset(show_word, 0, linkage->num_words * sizeof(*show_word));
if (verbosity_level(D_CCW))
print_lwg_path(lwg_path, "Linkage");
if (HIDE_MORPHO)
{
for (i=0; i<linkage->num_links; i++)
{
Link * lnk = &linkage->link_array[i];
if (is_morphology_link(lnk->link_name))
{
lnk->link_name = NULL;
}
else
{
show_word[lnk->rw] = true;
show_word[lnk->lw] = true;
}
}
}
for (i = 0; i < linkage->num_words; i++)
{
Disjunct *cdj = cdjp[i];
Gword *w;
const Gword *nw;
Gword **wgp;
const char *t = NULL;
bool at_nullblock_end;
bool join_alt = false;
char *s;
size_t l;
size_t m;
lgdebug(D_CCW, "Loop start, word%zu: cdj %s, path %s\n",
i, cdj ? cdj->word_string : "NULL",
lwg_path[i] ? lwg_path[i]->subword : "NULL");
w = lwg_path[i];
nw = lwg_path[i+1];
wgp = &lwg_path[i];
sentence_word = wg_get_sentence_word(sent, w);
if (NULL == cdj)
{
chosen_words[i] = NULL;
nbsize++;
if (NULL == nullblock_start)
nullblock_start = wgp;
at_nullblock_end = (NULL == nw) ||
(wg_get_sentence_word(sent, nw->unsplit_word) != sentence_word);
if (!at_nullblock_end && (NULL == cdjp[i+1]) &&
((w->morpheme_type == MT_PUNC) == (nw->morpheme_type == MT_PUNC)))
{
lgdebug(D_CCW, "Skipping word%zu cdjp=NULL#%zu, path %s\n",
i, nbsize, w->subword);
chosen_words[i] = NULL;
continue;
}
if (NULL != nullblock_start)
{
lgdebug(+D_CCW, "Handling %zu null words at %zu: ", nbsize, i);
if (1 == nbsize)
{
lgdebug(D_CCW, "A single null subword.\n");
t = join_null_word(sent, wgp, nbsize);
gwordlist_append(&n_lwg_path, w);
}
else
{
lgdebug(D_CCW, "Combining null subwords");
if (((*nullblock_start)->alternative_id == *nullblock_start)
&& at_nullblock_end)
{
lgdebug(D_CCW, " (null alternative)\n");
t = sentence_word->subword;
gwordlist_append(&n_lwg_path, sentence_word);
}
else
{
Gword *wgnull;
lgdebug(D_CCW, " (null partial word)\n");
wgnull = wordgraph_null_join(sent, wgp-nbsize+1, wgp);
gwordlist_append(&n_lwg_path, wgnull);
t = wgnull->subword;
}
}
nullblock_start = NULL;
nbsize = 0;
show_word[i] = true;
if (MT_WALL != w->morpheme_type)
{
l = strlen(t) + 2;
s = (char *) alloca(l+1);
s[0] = NULLWORD_START;
strcpy(&s[1], t);
s[l-1] = NULLWORD_END;
s[l] = '\0';
t = string_set_add(s, sent->string_set);
lgdebug(D_CCW, " %s\n", t);
}
}
}
else
{
t = cdj->word_string;
if (0)
{
}
else
{
if (is_idiom_word(t))
{
s = strdupa(t);
char *sm = (char *)get_word_subscript(s);
UNREACHABLE(NULL == sm);
*sm = '\0';
t = string_set_add(s, sent->string_set);
}
else if (HIDE_MORPHO)
{
Gword **wgaltp;
size_t join_len = 0;
size_t mcnt = 0;
const Gword *unsplit_word = w->unsplit_word;
for (wgaltp = wgp, j = i; NULL != *wgaltp; wgaltp++, j++)
{
if ((*wgaltp)->unsplit_word != unsplit_word) break;
if (MT_INFRASTRUCTURE ==
(*wgaltp)->unsplit_word->morpheme_type) break;
mcnt++;
if (NULL == cdjp[j])
{
join_alt = false;
break;
}
join_len += strlen(cdjp[j]->word_string) + 1;
if ((*wgaltp)->morpheme_type & IS_REG_MORPHEME)
join_alt = true;
}
if (join_alt)
{
const char subscript_sep_str[] = { SUBSCRIPT_SEP, '\0'};
char *join = alloca(join_len + 1);
join[0] = '\0';
for (wgaltp = wgp, m = 0; m < mcnt; wgaltp++, m++)
{
add_morpheme_unmarked(sent, join, cdjp[i+m]->word_string,
(*wgaltp)->morpheme_type);
}
strcat(join, subscript_mark_str());
for (wgaltp = wgp, m = 0; m < mcnt; wgaltp++, m++)
{
Gword *wg_placeholder;
if (m != mcnt-1)
{
chosen_words[i+m] = "";
if (show_word[i+m])
{
wg_placeholder = wordgraph_link_placeholder(sent, *wgaltp);
gwordlist_append(&n_lwg_path, wg_placeholder);
}
}
const char *sm =
get_word_subscript(cdjp[i+m]->word_string);
if (NULL != sm)
{
if (MT_STEM == (*wgaltp)->morpheme_type)
{
sm += 1 + STEM_MARK_L;
if ('\0' == *sm) sm = NULL;
#if 0
if ((cnt-1) == m)
{
move_combined_word = i+m-1;
}
else
{
move_combined_word = -1;
}
#endif
}
}
if (NULL != sm)
{
strcat(join, sm+1);
strcat(join, subscript_sep_str);
}
}
join_len = strlen(join);
if ((SUBSCRIPT_SEP == join[join_len-1]) ||
(SUBSCRIPT_MARK == join[join_len-1]))
join[join_len-1] = '\0';
gwordlist_append(&n_lwg_path, w->unsplit_word);
t = string_set_add(join, sent->string_set);
i += mcnt-1;
}
}
}
if (!join_alt) gwordlist_append(&n_lwg_path, *wgp);
if (t)
{
s = strdupa(t);
char *sm = get_word_subscript(s);
if (sm) *sm = SUBSCRIPT_DOT;
if ((!(w->status & WS_GUESS) && (w->status & WS_INDICT))
|| !DISPLAY_GUESS_MARKS)
{
t = string_set_add(s, sent->string_set);
}
else
{
const char *regex_name = w->regex_name;
int baselen = NULL == sm ? strlen(t) : (size_t)(sm-s);
char guess_mark = 0;
switch (w->status & WS_GUESS)
{
case WS_SPELL:
guess_mark = GM_SPELL;
break;
case WS_RUNON:
guess_mark = GM_RUNON;
break;
case WS_REGEX:
guess_mark = GM_REGEX;
break;
case 0:
guess_mark = GM_UNKNOWN;
break;
default:
assert(0, "Missing 'case: %2x'", w->status & WS_GUESS);
}
if ((NULL == regex_name) || !display_morphology) regex_name = "";
s = alloca(strlen(t) + strlen(regex_name) + 4);
strncpy(s, t, baselen);
s[baselen] = '[';
s[baselen + 1] = guess_mark;
strcpy(s + baselen + 2, regex_name);
strcat(s, "]");
if (NULL != sm) strcat(s, sm);
t = string_set_add(s, sent->string_set);
}
}
}
assert(t != NULL, "Word %zu: NULL", i);
chosen_words[i] = t;
}
if (test_enabled("removeZZZ"))
{
if (sent->dict->zzz_connector)
{
for (i=0; i<linkage->num_links; i++)
{
Link *lnk = &(linkage->link_array[i]);
if (0 == strcmp(sent->dict->zzz_connector, lnk->link_name))
chosen_words[lnk->rw] = NULL;
}
}
}
linkage->word = (const char **) exalloc(linkage->num_words*sizeof(char *));
Disjunct **cdj = linkage->chosen_disjuncts;
for (i=0, j=0; i<linkage->num_words; ++i)
{
if (chosen_words[i] &&
(chosen_words[i][0] || (!HIDE_MORPHO || show_word[i])))
{
linkage->word[j] = chosen_words[i];
#if USE_SAT_SOLVER
Disjunct *cdtmp = cdj[j];
#endif
cdj[j] = cdj[i];
#if USE_SAT_SOLVER
cdj[i] = cdtmp;
#endif
remap[i] = j;
j++;
}
else
{
remap[i] = -1;
}
}
linkage->num_words = j;
remap_linkages(linkage, remap);
linkage->wg_path_display = n_lwg_path;
if (verbosity_level(D_CCW))
print_lwg_path(n_lwg_path, "Display");
}
#undef D_CCW
#define D_CGW 5
void compute_generated_words(Sentence sent, Linkage linkage)
{
Disjunct **cdjp = linkage->chosen_disjuncts;
linkage->word = malloc(linkage->num_words * sizeof(char *));
for (WordIdx i = 0; i < linkage->num_words; i++)
{
assert(cdjp[i] != NULL, "NULL disjunct in generated sentence");
const char *word;
Disjunct *cdj = cdjp[i];
if (cdj->is_category == 0)
{
word = cdj->word_string;
}
else
{
assert(cdj->num_categories > 0, "zero categories in disjunct");
word = linkage_get_disjunct_str(linkage, i);
size_t len = strlen(word) + sizeof("<>");
char *disjunct_string = alloca(len);
snprintf(disjunct_string, len, "<%s>", word);
word = string_set_add(disjunct_string, sent->string_set);
}
linkage->word[i] = word;
}
}
#undef D_CGW
Linkage linkage_create(LinkageIdx k, Sentence sent, Parse_Options opts)
{
Linkage linkage;
#if USE_SAT_SOLVER
if (opts->use_sat_solver)
{
linkage = sat_create_linkage(k, sent, opts);
if (!linkage) return NULL;
}
else
#endif
{
if (sent->num_linkages_post_processed <= k) return NULL;
linkage = &sent->lnkages[k];
}
if (!IS_GENERATION(sent->dict))
compute_chosen_words(sent, linkage, opts);
linkage->is_sent_long = (linkage->num_words >= opts->twopass_length);
return linkage;
}
void linkage_delete(Linkage linkage)
{
}
size_t linkage_get_num_words(const Linkage linkage)
{
if (!linkage) return 0;
return linkage->num_words;
}
size_t linkage_get_num_links(const Linkage linkage)
{
if (!linkage) return 0;
return linkage->num_links;
}
static inline bool verify_link_index(const Linkage linkage, LinkIdx index)
{
if (!linkage) return false;
if (index >= linkage->num_links) return false;
return true;
}
int linkage_get_link_length(const Linkage linkage, LinkIdx index)
{
Link *link;
if (!verify_link_index(linkage, index)) return -1;
link = &(linkage->link_array[index]);
return link->rw - link->lw;
}
WordIdx linkage_get_link_lword(const Linkage linkage, LinkIdx index)
{
if (!verify_link_index(linkage, index)) return SIZE_MAX;
return linkage->link_array[index].lw;
}
WordIdx linkage_get_link_rword(const Linkage linkage, LinkIdx index)
{
if (!verify_link_index(linkage, index)) return SIZE_MAX;
return linkage->link_array[index].rw;
}
const char * linkage_get_link_label(const Linkage linkage, LinkIdx index)
{
if (!verify_link_index(linkage, index)) return NULL;
return linkage->link_array[index].link_name;
}
const char * linkage_get_link_llabel(const Linkage linkage, LinkIdx index)
{
if (!verify_link_index(linkage, index)) return NULL;
return connector_string(linkage->link_array[index].lc);
}
const char * linkage_get_link_rlabel(const Linkage linkage, LinkIdx index)
{
if (!verify_link_index(linkage, index)) return NULL;
return connector_string(linkage->link_array[index].rc);
}
const char ** linkage_get_words(const Linkage linkage)
{
return linkage->word;
}
const char * linkage_get_disjunct_str(const Linkage linkage, WordIdx w)
{
if (NULL == linkage) return "";
if (linkage->num_words <= w) return NULL;
Disjunct *dj = linkage->chosen_disjuncts[w];
if (NULL == dj) return "";
if (NULL == linkage->disjunct_list_str)
lg_compute_disjunct_strings(linkage);
return linkage->disjunct_list_str[w];
}
float linkage_get_disjunct_cost(const Linkage linkage, WordIdx w)
{
Disjunct *dj;
if (linkage->num_words <= w) return 0.0;
dj = linkage->chosen_disjuncts[w];
if (NULL == dj)
return 0.0;
if (dj->is_category)
return dj->category[0].cost;
return dj->cost;
}
const char * linkage_get_word(const Linkage linkage, WordIdx w)
{
if (!linkage) return NULL;
if (linkage->num_words <= w) return NULL;
return linkage->word[w];
}
int linkage_unused_word_cost(const Linkage linkage)
{
if (!linkage) return 0;
return linkage->lifo.unused_word_cost;
}
float linkage_disjunct_cost(const Linkage linkage)
{
if (!linkage) return 0.0;
return linkage->lifo.disjunct_cost;
}
int linkage_link_cost(const Linkage linkage)
{
if (!linkage) return 0;
return linkage->lifo.link_cost;
}
WordIdx linkage_get_word_byte_start(const Linkage linkage, WordIdx w)
{
if (linkage->num_words <= w) return (WordIdx)-1;
return linkage->wg_path_display[w]->start - linkage->sent->orig_sentence;
}
WordIdx linkage_get_word_byte_end(const Linkage linkage, WordIdx w)
{
if (linkage->num_words <= w) return (WordIdx)-1;
return linkage->wg_path_display[w]->end - linkage->sent->orig_sentence;
}
WordIdx linkage_get_word_char_start(const Linkage linkage, WordIdx w)
{
if (linkage->num_words <= w) return (WordIdx)-1;
int pos = (int)(linkage->wg_path_display[w]->start - linkage->sent->orig_sentence);
char *sentchunk = strndupa(linkage->sent->orig_sentence, pos);
return utf8_strlen(sentchunk);
}
WordIdx linkage_get_word_char_end(const Linkage linkage, WordIdx w)
{
if (linkage->num_words <= w) return (WordIdx)-1;
int pos = (int)(linkage->wg_path_display[w]->end - linkage->sent->orig_sentence);
char *sentchunk = strndupa(linkage->sent->orig_sentence, pos);
return utf8_strlen(sentchunk);
}
const Category_cost *linkage_get_categories(const Linkage linkage, WordIdx w)
{
if (NULL == linkage) return NULL;
if (linkage->num_words <= w) return NULL;
Disjunct *dj = linkage->chosen_disjuncts[w];
if (dj->is_category == 0) return NULL;
return dj->category;
}