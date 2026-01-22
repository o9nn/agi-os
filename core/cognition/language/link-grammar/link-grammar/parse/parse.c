#include <limits.h>
#include "api-structures.h"
#include "count.h"
#include "dict-common/dict-common.h"
#include "disjunct-utils.h"
#include "extract-links.h"
#include "fast-match.h"
#include "linkage/analyze-linkage.h"
#include "linkage/linkage.h"
#include "linkage/sane.h"
#include "parse.h"
#include "post-process/post-process.h"
#include "preparation.h"
#include "prune.h"
#include "resources.h"
#include "tokenize/word-structures.h"
#define D_PARSE 5
static Linkage linkage_array_new(int num_to_alloc)
{
Linkage lkgs = (Linkage) malloc(num_to_alloc * sizeof(struct Linkage_s));
memset(lkgs, 0, num_to_alloc * sizeof(struct Linkage_s));
return lkgs;
}
void linkage_array_free(Linkage lkgs)
{
free(lkgs);
}
static void find_unused_disjuncts(Sentence sent, extractor_t *pex)
{
const size_t disjunct_used_sz =
sizeof(bool) * sent->wildcard_word_num_disjuncts;
sent->disjunct_used = malloc(disjunct_used_sz);
memset(sent->disjunct_used, 0, disjunct_used_sz);
if (pex != NULL)
mark_used_disjuncts(pex, sent->disjunct_used);
if (verbosity_level(+D_PARSE))
{
unsigned int num_unused = 0;
for (unsigned int i = 0; i < sent->wildcard_word_num_disjuncts; i++)
if (!sent->disjunct_used[i]) num_unused++;
prt_error("Info: Unused disjuncts %u/%u\n", num_unused,
sent->wildcard_word_num_disjuncts);
}
}
static void setup_linkages(Sentence sent, extractor_t* pex,
fast_matcher_t* mchxt,
count_context_t* ctxt,
Parse_Options opts)
{
sent->overflowed = build_parse_set(pex, sent, mchxt, ctxt, sent->null_count, opts);
print_time(opts, "Built parse set");
if (sent->overflowed && (1 < opts->verbosity) && !IS_GENERATION(sent->dict))
{
err_ctxt ec = { sent };
err_msgc(&ec, lg_Warn, "Count overflow.\n"
"Considering a random subset of %zu of an unknown and large number of linkages\n",
opts->linkage_limit);
}
if (sent->num_linkages_found == 0)
{
sent->num_linkages_alloced = 0;
sent->num_linkages_post_processed = 0;
sent->num_valid_linkages = 0;
sent->lnkages = NULL;
return;
}
sent->num_linkages_alloced =
MIN(sent->num_linkages_found, (int) opts->linkage_limit);
if (sent->lnkages) free_linkages(sent);
sent->lnkages = linkage_array_new(sent->num_linkages_alloced);
}
static void print_chosen_disjuncts_words(const Linkage lkg, bool prt_optword)
{
size_t i;
dyn_str *djwbuf = dyn_str_new();
err_msg(lg_Debug, "Linkage %p (%zu words): ", lkg, lkg->num_words);
for (i = 0; i < lkg->num_words; i++)
{
Disjunct *cdj = lkg->chosen_disjuncts[i];
const char *djw;
if (NULL == cdj)
{
djw = (prt_optword && lkg->sent->word[i].optional) ? "{}" : "[]";
}
else if (0 == cdj->is_category)
{
if ('\0' == cdj->word_string[0])
djw = "\\0";
else
djw = cdj->word_string;
}
else
{
if ((NULL == cdj->category))
{
djw = "\\0";
}
else
{
char *cbuf = alloca(32);
snprintf(cbuf, 32, "Category[0]:%u", cdj->category[0].num);
djw = cbuf;
}
}
dyn_strcat(djwbuf, djw);
dyn_strcat(djwbuf, " ");
}
err_msg(lg_Debug, "%s\n", djwbuf->str);
dyn_str_delete(djwbuf);
}
static bool optional_word_exists(Sentence sent)
{
for (WordIdx w = 0; w < sent->length; w++)
if (sent->word[w].optional) return true;
return false;
}
#define D_PL 7
static void process_linkages(Sentence sent, extractor_t* pex,
Parse_Options opts)
{
if (0 == sent->num_linkages_found) return;
if (0 == sent->num_linkages_alloced) return;
bool pick_randomly = sent->overflowed ||
(sent->num_linkages_found > (int) opts->linkage_limit);
sent->num_valid_linkages = 0;
size_t N_invalid_morphism = 0;
int itry = 0;
size_t in = 0;
int maxtries;
#define MAX_TRIES 250000
if (pick_randomly)
{
maxtries = MIN((int) sent->num_linkages_alloced + MAX_TRIES,
sent->num_linkages_found);
}
else
{
maxtries = sent->num_linkages_alloced;
}
bool need_sane_morphism = !IS_GENERATION(sent->dict) ||
optional_word_exists(sent);
bool need_init = true;
for (itry=0; itry<maxtries; itry++)
{
Linkage lkg = &sent->lnkages[in];
Linkage_info * lifo = &lkg->lifo;
lifo->index = pick_randomly ? -(itry+1) : itry;
if (need_init)
{
partial_init_linkage(sent, lkg, sent->length);
need_init = false;
}
extract_links(pex, lkg);
compute_link_names(lkg, sent->string_set);
if (verbosity_level(+D_PL))
{
err_msg(lg_Debug, "chosen_disjuncts before:\n\\");
print_chosen_disjuncts_words(lkg, true);
}
if (need_sane_morphism)
{
if (sane_linkage_morphism(sent, lkg, opts))
{
remove_empty_words(lkg);
if (verbosity_level(+D_PL))
{
err_msg(lg_Debug, "chosen_disjuncts after:\n\\");
print_chosen_disjuncts_words(lkg, false);
}
}
else
{
N_invalid_morphism++;
lkg->num_links = 0;
lkg->num_words = sent->length;
memset(lkg->chosen_disjuncts, 0, sent->length * sizeof(Disjunct *));
continue;
}
}
if (IS_GENERATION(sent->dict))
compute_generated_words(sent, lkg);
need_init = true;
in++;
if (in >= sent->num_linkages_alloced) break;
}
if (!need_init) free_linkage(&sent->lnkages[in]);
sent->num_valid_linkages = in;
sent->num_linkages_alloced = sent->num_valid_linkages;
if (verbosity >= D_USER_INFO)
{
lgdebug(0, "Info: sane_morphism(): %zu of %d linkages had "
"invalid morphology construction\n", N_invalid_morphism,
itry + (itry != maxtries));
}
}
static int linkage_equiv_p(Linkage lpv, Linkage lnx)
{
for (uint32_t li=0; li<lpv->num_links; li++)
{
Link * plk = &lpv->link_array[li];
Link * nlk = &lnx->link_array[li];
int lwd = plk->lw - nlk->lw;
if (lwd) return lwd;
int rwd = plk->rw - nlk->rw;
if (rwd) return rwd;
}
for (uint32_t li=0; li<lpv->num_links; li++)
{
Link * plk = &lpv->link_array[li];
Link * nlk = &lnx->link_array[li];
if (plk->link_name == nlk->link_name) continue;
int lncmp = strcmp(plk->link_name, nlk->link_name);
if (lncmp) return lncmp;
}
for (uint32_t wi=0; wi<lpv->num_words; wi++)
{
Disjunct * pdj = lpv->chosen_disjuncts[wi];
Disjunct * ndj = lnx->chosen_disjuncts[wi];
if (NULL == pdj)
{
if (NULL == ndj) continue;
return 1;
}
if (pdj->word_string == ndj->word_string) continue;
int wscmp = strcmp(pdj->word_string, ndj->word_string);
if (wscmp) return wscmp;
}
for (uint32_t li=0; li<lpv->num_links; li++)
{
Link * plk = &lpv->link_array[li];
Link * nlk = &lnx->link_array[li];
if (plk->lc != nlk->lc)
{
if (plk->lc->desc != nlk->lc->desc)
return strcmp(connector_string(plk->lc), connector_string(nlk->lc));
int md = plk->lc->multi - nlk->lc->multi;
if (md) return md;
}
if (plk->rc != nlk->rc)
{
if (plk->rc->desc != nlk->rc->desc)
return strcmp(connector_string(plk->rc), connector_string(nlk->rc));
int md = plk->rc->multi - nlk->rc->multi;
if (md) return md;
}
}
#if DOUBLE_CHECK
for (uint32_t wi=0; wi<lpv->num_words; wi++)
{
if (lpv->chosen_disjuncts[wi] != lnx->chosen_disjuncts[wi])
return strcmp(
linkage_get_disjunct_str(lpv, wi),
linkage_get_disjunct_str(lnx, wi));
}
#endif
lnx->dupe = true;
return 0;
}
int VDAL_compare_linkages(Linkage l1, Linkage l2)
{
Linkage_info * p1 = &l1->lifo;
Linkage_info * p2 = &l2->lifo;
if (p1->N_violations != p2->N_violations)
return (p1->N_violations - p2->N_violations);
if (p1->unused_word_cost != p2->unused_word_cost)
return (p1->unused_word_cost - p2->unused_word_cost);
float diff = p1->disjunct_cost - p2->disjunct_cost;
#define COST_EPSILON 1.0e-6
if (COST_EPSILON < diff) return 1;
if (diff < -COST_EPSILON) return -1;
if (p1->link_cost != p2->link_cost)
return (p1->link_cost - p2->link_cost);
if (l1->num_words != l2->num_words)
return l1->num_words - l2->num_words;
if (0 < p1->N_violations) return 0;
return linkage_equiv_p(l1, l2);
}
static void deduplicate_linkages(Sentence sent, int linkage_limit)
{
int linkage_dedup = -1;
const char *test_linkage_dedup = test_enabled("linkage-dedup");
if (test_linkage_dedup != NULL)
{
if ((test_linkage_dedup[0] != ':') || (test_linkage_dedup[1] == '\0'))
linkage_dedup = 1;
else
linkage_dedup = atoi(test_linkage_dedup + 1);
}
if ((linkage_dedup == 0) || ((linkage_dedup < 0) &&
!sent->overflowed && (sent->num_linkages_found <= linkage_limit)))
return;
uint32_t nl = sent->num_valid_linkages;
if (2 > nl) return;
uint32_t tgt = 0;
uint32_t blkstart = 0;
uint32_t blklen = 1;
uint32_t num_dupes = 0;
for (uint32_t i=1; i<nl; i++)
{
Linkage lnx = &sent->lnkages[i];
if (false == lnx->dupe) { blklen++; continue; }
free_linkage(lnx);
num_dupes ++;
if (0 < blklen)
{
if (0 < tgt)
{
Linkage ltgt = &sent->lnkages[tgt];
Linkage lsrc = &sent->lnkages[blkstart];
memmove(ltgt, lsrc, blklen * sizeof(struct Linkage_s));
}
tgt += blklen;
blklen = 0;
}
blkstart = i+1;
}
if (0 < tgt)
{
Linkage ltgt = &sent->lnkages[tgt];
Linkage lsrc = &sent->lnkages[blkstart];
blklen += sent->num_linkages_alloced - sent->num_valid_linkages;
memmove(ltgt, lsrc, blklen * sizeof(struct Linkage_s));
}
assert(num_dupes < sent->num_valid_linkages, "Too many duplicates found!");
sent->num_linkages_alloced -= num_dupes;
sent->num_valid_linkages -= num_dupes;
sent->num_linkages_post_processed -= num_dupes;
}
static void sort_linkages(Sentence sent, Parse_Options opts)
{
if (0 == sent->num_linkages_found) return;
if (0 != sent->rand_state && sent->dict->shuffle_linkages) return;
for (uint32_t i=0; i<sent->num_linkages_alloced; i++)
sent->lnkages[i].dupe = false;
qsort((void *)sent->lnkages, sent->num_linkages_alloced,
sizeof(struct Linkage_s),
(int (*)(const void *, const void *))opts->cost_model.compare_fn);
deduplicate_linkages(sent, opts->linkage_limit);
print_time(opts, "Sorted all linkages");
}
static void notify_no_complete_linkages(unsigned int null_count,
unsigned int max_null_count)
{
if ((0 == null_count) && (0 < max_null_count) && verbosity > 0)
prt_error("No complete linkages found.\n");
}
void classic_parse(Sentence sent, Parse_Options opts)
{
fast_matcher_t * mchxt = NULL;
count_context_t * ctxt = NULL;
Tracon_sharing *ts_parsing = NULL;
void *saved_memblock = NULL;
int current_prune_level = -1;
int needed_prune_level = opts->min_null_count;
bool more_pruning_possible = false;
unsigned int max_null_count = opts->max_null_count;
max_null_count = (unsigned int)MIN(max_null_count, sent->length);
bool one_step_parse = (unsigned int)opts->min_null_count != max_null_count;
int max_prune_level = (int)max_null_count;
bool optimize_pruning = true;
unsigned int *ncu[2];
ncu[0] = alloca(sent->length * sizeof(*ncu[0]));
ncu[1] = alloca(sent->length * sizeof(*ncu[1]));
if (opts->islands_ok)
optimize_pruning = false;
if (sent->length < sent->min_len_multi_pruning)
optimize_pruning = false;
if (!optimize_pruning)
{
if (opts->min_null_count == 0)
max_prune_level = 0;
else
{
needed_prune_level = MAX_SENTENCE;
one_step_parse = false;
}
}
prepare_to_parse(sent, opts);
if (resources_exhausted(opts->resources)) return;
Tracon_sharing *ts_pruning = pack_sentence_for_pruning(sent);
free_sentence_disjuncts(sent, false);
if (one_step_parse)
{
saved_memblock = save_disjuncts(sent, ts_pruning);
}
print_time(opts, "Encoded for pruning%s%s",
(NULL == ts_pruning->tracon_list) ? " (skipped)" : "",
(one_step_parse) ? " (one-step)" : "");
for (unsigned int nl = opts->min_null_count; nl <= max_null_count; nl++)
{
sent->null_count = nl;
sent->num_linkages_found = 0;
sent->overflowed = false;
sent->num_valid_linkages = 0;
sent->num_linkages_post_processed = 0;
if (needed_prune_level > current_prune_level)
{
current_prune_level = needed_prune_level;
if (needed_prune_level < max_prune_level)
needed_prune_level++;
else
needed_prune_level = MAX_SENTENCE;
if (more_pruning_possible)
restore_disjuncts(sent, saved_memblock, ts_pruning);
more_pruning_possible =
one_step_parse && (current_prune_level != MAX_SENTENCE);
unsigned int expected_null_count =
pp_and_power_prune(sent, ts_pruning, current_prune_level, opts,
ncu);
if (expected_null_count > nl)
{
if (opts->verbosity >= D_USER_TIMES)
{
prt_error("#### Skip parsing (w/%u ", nl);
if (expected_null_count-1 > nl)
prt_error("to %u nulls)\n", expected_null_count-1);
else
prt_error("null%s)\n", (nl != 1) ? "s" : "");
}
notify_no_complete_linkages(nl, max_null_count);
nl = expected_null_count-1;
if (nl == sent->length-1) nl--;
continue;
}
}
if (NULL != ts_pruning)
{
free_tracon_sharing(ts_parsing);
ts_parsing = pack_sentence_for_parsing(sent);
print_time(opts, "Encoded for parsing");
if (!more_pruning_possible)
{
free_tracon_memblock(ts_pruning);
ts_pruning = NULL;
if (NULL != saved_memblock)
free_saved_memblock(saved_memblock);
}
gword_record_in_connector(sent);
free_fast_matcher(sent, mchxt);
mchxt = alloc_fast_matcher(sent, ncu);
print_time(opts, "Initialized fast matcher");
if (resources_exhausted(opts->resources)) goto parse_end_cleanup;
}
free_linkages(sent);
free_count_context(ctxt, sent);
ctxt = alloc_count_context(sent, ts_parsing);
sent->num_linkages_found = do_parse(sent, mchxt, ctxt, opts);
print_time(opts, "Counted parses (%d w/%u null%s)",
sent->num_linkages_found, sent->null_count,
(sent->null_count != 1) ? "s" : "");
if (resources_exhausted(opts->resources))
{
sent->num_linkages_found = 0;
goto parse_end_cleanup;
}
if (sent->num_linkages_found > 0)
{
extractor_t * pex = extractor_new(sent);
setup_linkages(sent, pex, mchxt, ctxt, opts);
process_linkages(sent, pex, opts);
if (IS_GENERATION(sent->dict))
find_unused_disjuncts(sent, pex);
#ifdef PC_DISPLAY
display_parse_choice(pex);
#endif
free_extractor(pex);
post_process_lkgs(sent, opts);
if (resources_exhausted(opts->resources))
{
sent->num_linkages_found = 0;
sent->num_valid_linkages = 0;
sent->num_linkages_post_processed = 0;
goto parse_end_cleanup;
}
if (sent->num_valid_linkages > 0) break;
if (verbosity >= D_USER_INFO)
{
if ((sent->num_linkages_post_processed > 0) &&
(sent->num_linkages_post_processed == sent->num_linkages_alloced) &&
((int)opts->linkage_limit < sent->num_linkages_found) &&
!IS_GENERATION(sent->dict))
prt_error("Info: All examined linkages (%zu) had P.P. violations.\n"
"Consider increasing the linkage limit.\n"
"At the command line, use !limit\n",
sent->num_linkages_post_processed);
}
}
notify_no_complete_linkages(nl, max_null_count);
}
if ((sent->num_linkages_found == 0) && IS_GENERATION(sent->dict))
find_unused_disjuncts(sent, NULL);
sort_linkages(sent, opts);
parse_end_cleanup:
if (NULL != ts_pruning)
{
free_categories(sent);
free_tracon_memblock(ts_pruning);
free_saved_memblock(saved_memblock);
}
free_tracon_sharing(ts_parsing);
free_count_context(ctxt, sent);
free_fast_matcher(sent, mchxt);
}