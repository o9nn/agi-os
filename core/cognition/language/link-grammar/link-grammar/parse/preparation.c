#include "api-structures.h"
#include "prepare/build-disjuncts.h"
#include "connectors.h"
#include "dict-common/dict-common.h"
#include "disjunct-utils.h"
#include "externs.h"
#include "preparation.h"
#include "print/print.h"
#include "prune.h"
#include "resources.h"
#include "string-set.h"
#include "utilities.h"
#include "tokenize/word-structures.h"
#include "tokenize/tokenize.h"
#include "tokenize/tok-structures.h"
#define D_PREP 5
static int set_dist_fields(Connector * c, size_t w, int delta)
{
if (c == NULL) return (int) w;
c->nearest_word = set_dist_fields(c->next, w, delta) + delta;
return c->nearest_word;
}
static void setup_connectors(Sentence sent)
{
for (WordIdx w = 0; w < sent->length; w++)
{
Disjunct *head = NULL;
Disjunct *xd;
for (Disjunct *d = sent->word[w].d; d != NULL; d = xd)
{
xd = d->next;
if ((set_dist_fields(d->left, w, -1) < 0) ||
(set_dist_fields(d->right, w, 1) >= (int)sent->length))
{
if (d->is_category != 0) free(d->category);
}
else
{
d->next = head;
head = d;
}
}
sent->word[w].d = head;
}
}
void gword_record_in_connector(Sentence sent)
{
for (Disjunct *d = sent->dc_memblock;
d < &((Disjunct *)sent->dc_memblock)[sent->num_disjuncts]; d++)
{
for (Connector *c = d->right; NULL != c; c = c->next)
c->originating_gword = d->originating_gword;
for (Connector *c = d->left; NULL != c; c = c->next)
c->originating_gword = d->originating_gword;
}
}
static void build_sentence_disjuncts(Sentence sent, float cost_cutoff,
Parse_Options opts)
{
sent->Disjunct_pool = pool_new(__func__, "Disjunct",
2048, sizeof(Disjunct),
false, false, false);
sent->Connector_pool = pool_new(__func__, "Connector",
8192, sizeof(Connector),
true, false, false);
#ifdef DEBUG
size_t num_con_alloced = pool_num_elements_issued(sent->Connector_pool);
#endif
for (size_t w = 0; w < sent->length; w++)
{
Disjunct * d = NULL;
for (X_node * x = sent->word[w].x; x != NULL; x = x->next)
{
Disjunct *dx = build_disjuncts_for_exp(sent, x->exp, x->string,
&x->word->gword_set_head, cost_cutoff, opts);
d = catenate_disjuncts(dx, d);
}
sent->word[w].d = d;
}
#ifdef DEBUG
unsigned int dcnt, ccnt;
count_disjuncts_and_connectors(sent, &dcnt, &ccnt);
lgdebug(+D_PREP, "%u disjuncts, %u connectors (%zu allocated)\n",
dcnt, ccnt,
pool_num_elements_issued(sent->Connector_pool) - num_con_alloced);
#endif
pool_delete(sent->Clause_pool);
pool_delete(sent->Tconnector_pool);
sent->Clause_pool = NULL;
sent->Tconnector_pool = NULL;
}
static void create_wildcard_word_disjunct_list(Sentence sent,
Parse_Options opts)
{
if (opts->verbosity >= D_USER_TIMES)
prt_error("#### Creating a wild-card word disjunct list\n");
int spell_option = parse_options_get_spell_guess(opts);
parse_options_set_spell_guess(opts, 0);
Sentence wc_word_list = sentence_create(WILDCARD_WORD, sent->dict);
if (0 != sentence_split(wc_word_list, opts)) goto error;
WordIdx w = 1;
if (0 == strcmp(wc_word_list->word[0].unsplit_word, LEFT_WALL_WORD))
{
Word tmp = wc_word_list->word[0];
wc_word_list->word[0] = wc_word_list->word[1];
wc_word_list->word[1] = tmp;
wc_word_list->word[1].x = NULL;
w = 2;
}
if ((wc_word_list->length == w + 1) &&
(0 == strcmp(wc_word_list->word[w].unsplit_word, RIGHT_WALL_WORD)))
{
wc_word_list->word[w].x = NULL;
}
build_sentence_disjuncts(wc_word_list, opts->disjunct_cost, opts);
Word *word0 = &wc_word_list->word[0];
unsigned int Ndeleted;
Ndeleted = eliminate_duplicate_disjuncts(word0->d, false);
Ndeleted += eliminate_duplicate_disjuncts(word0->d, true);
print_time(opts, "Eliminated duplicate disjuncts (%u deleted)", Ndeleted);
wc_word_list->min_len_encoding = 2;
Tracon_sharing *t = pack_sentence_for_pruning(wc_word_list);
for (unsigned int n = 0; n < t->num_disjuncts; n++)
t->dblock_base[n].ordinal = (int)n;
sent->wildcard_word_dc_memblock = t->memblock;
sent->wildcard_word_dc_memblock_sz = t->memblock_sz;
sent->wildcard_word_num_disjuncts = t->num_disjuncts;
if (opts->verbosity >= D_USER_TIMES)
print_time(opts, "Finished creating list: %u disjuncts", t->num_disjuncts);
t->memblock = NULL;
free_tracon_sharing(t);
error:
parse_options_set_spell_guess(opts, spell_option);
sentence_delete(wc_word_list);
}
void prepare_to_parse(Sentence sent, Parse_Options opts)
{
size_t i;
if (IS_GENERATION(sent->dict))
create_wildcard_word_disjunct_list(sent, opts);
build_sentence_disjuncts(sent, opts->disjunct_cost, opts);
if (verbosity_level(D_PREP))
{
prt_error("Debug: After expanding expressions into disjuncts:\n\\");
print_disjunct_counts(sent);
}
print_time(opts, "Built disjuncts");
unsigned int Ndeleted = 0;
for (i=0; i<sent->length; i++)
{
Ndeleted += eliminate_duplicate_disjuncts(sent->word[i].d, false);
if (IS_GENERATION(sent->dict))
{
if ((sent->word[i].d != NULL) && (sent->word[i].d->is_category != 0))
{
Ndeleted += eliminate_duplicate_disjuncts(sent->word[i].d, true);
int nord = 0;
for (Disjunct *d = sent->word[i].d; d != NULL; d = d->next)
d->ordinal = nord++;
}
else
{
for (Disjunct *d = sent->word[i].d; d != NULL; d = d->next)
d->ordinal = -1;
}
}
#if 0
if (resources_exhausted(opts->resources))
return;
#endif
}
print_time(opts, "Eliminated duplicate disjuncts (%u deleted)", Ndeleted);
if (verbosity_level(D_PREP))
{
prt_error("Debug: After duplicate elimination:\n");
print_disjunct_counts(sent);
}
setup_connectors(sent);
if (verbosity_level(D_PREP))
{
prt_error("Debug: After setting connectors:\n");
print_disjunct_counts(sent);
}
if (verbosity_level(D_SPEC+2))
{
printf("prepare_to_parse:\n");
print_all_disjuncts(sent);
}
}