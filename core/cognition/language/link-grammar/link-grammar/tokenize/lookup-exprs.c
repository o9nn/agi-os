#include "api-structures.h"
#include "dict-common/dict-api.h"
#include "dict-common/dict-common.h"
#include "dict-common/dict-defines.h"
#include "dict-common/dict-utils.h"
#include "error.h"
#include "lookup-exprs.h"
#include "print/print.h"
#include "tokenize.h"
#include "tok-structures.h"
#include "wordgraph.h"
#include "word-structures.h"
static Dict_node *dictionary_all_categories(Dictionary dict)
{
assert(0 != dict->num_categories, "No categories in dict!");
Dict_node * dn = malloc(sizeof(*dn) * dict->num_categories);
for (size_t i = 0; i < dict->num_categories; i++)
{
dn[i].exp = dict->category[i + 1].exp;
char category_string[16];
snprintf(category_string, sizeof(category_string), " %x",
(unsigned int)i + 1);
dn[i].string = string_set_add(category_string, dict->string_set);
dn[i].right = &dn[i + 1];
}
dn[dict->num_categories-1].right = NULL;
return dn;
}
static X_node * build_word_expressions(Sentence sent, const Gword *w,
const char *s, Parse_Options opts)
{
const Dictionary dict = sent->dict;
Dict_node * dn_head = NULL;
if (IS_GENERATION(dict) && (NULL != strstr(w->subword, WILDCARD_WORD)))
{
if (0 == strcmp(w->subword, WILDCARD_WORD))
{
dn_head = dictionary_all_categories(dict);
}
else
{
char *t = alloca(strlen(w->subword) + 1);
const char *backslash = strchr(w->subword, '\\');
strcpy(t, w->subword);
strcpy(t+(backslash - w->subword), backslash+1);
dn_head = dictionary_lookup_wild(dict, t);
}
}
else
{
dn_head = dictionary_lookup_list(dict, NULL == s ? w->subword : s);
}
X_node * x = NULL;
Dict_node * dn = dn_head;
while (dn != NULL)
{
X_node * y = (X_node *) pool_alloc(sent->X_node_pool);
y->next = x;
x = y;
x->exp = copy_Exp(dn->exp, sent->Exp_pool, opts);
if (NULL == s)
{
x->string = dn->string;
}
else
{
dyn_str *xs = dyn_str_new();
const char *sm = get_word_subscript(dn->string);
dyn_strcat(xs, w->subword);
if (NULL != sm) dyn_strcat(xs, sm);
x->string = string_set_add(xs->str, sent->string_set);
dyn_str_delete(xs);
}
x->word = w;
dn = dn->right;
}
if (!IS_GENERATION(dict) || (0 != strcmp(w->subword, WILDCARD_WORD)))
free_lookup_list (dict, dn_head);
else
free(dn_head);
if (IS_GENERATION(dict) && (NULL == dn_head) &&
(NULL != strstr(w->subword, WILDCARD_WORD)))
{
X_node * y = pool_alloc(sent->X_node_pool);
y->next = NULL;
y->exp = make_zeroary_node(sent->Exp_pool);
}
assert(NULL != x, "Word '%s': NULL X-node", w->subword);
return x;
}
static X_node * catenate_X_nodes(X_node *d1, X_node *d2)
{
X_node * dis = d1;
if (d1 == NULL) return d2;
if (d2 == NULL) return d1;
while (dis->next != NULL) dis = dis->next;
dis->next = d2;
return d1;
}
#ifdef DEBUG
GNUC_UNUSED static void print_x_node(X_node *x)
{
if (x == NULL) printf("NULL X_node\n");
for (; x != NULL; x = x->next)
{
printf("%p: exp=%p next=%p\n", x, x->exp, x->next);
}
}
#endif
static void add_empty_word(Sentence sent, X_node *x)
{
if (MT_WALL == x->word->morpheme_type) return;
for(; NULL != x; x = x->next)
{
if (is_stem(x->string)) continue;
Exp *zn = make_connector_node(sent->dict,
sent->Exp_pool, sent->dict->zzz_connector, '+', false);
zn = make_optional_node(sent->Exp_pool, zn);
Exp *an = make_and_node(sent->Exp_pool, zn, x->exp);
x->exp = an;
}
}
#define D_X_NODE 9
#define D_DWE 8
static bool determine_word_expressions(Sentence sent, Gword *w,
unsigned int *ZZZ_added,
Parse_Options opts)
{
Dictionary dict = sent->dict;
const size_t wordpos = w->sent_wordidx;
const char *s = w->subword;
lgdebug(+D_DWE, "Word %zu subword %zu:'%s' status %s",
wordpos, w->node_num, s, gword_status(sent, w));
if (NULL != sent->word[wordpos].unsplit_word)
lgdebug(D_DWE, " (unsplit '%s')", sent->word[wordpos].unsplit_word);
X_node * we = NULL;
if (w->status & WS_INDICT)
{
we = build_word_expressions(sent, w, NULL, opts);
}
else if (w->status & WS_REGEX)
{
we = build_word_expressions(sent, w, w->regex_name, opts);
}
else if (IS_GENERATION(dict) && (NULL != strstr(s, WILDCARD_WORD)))
{
lgdebug(+D_DWE, "Wildcard word %s\n", s);
we = build_word_expressions(sent, w, NULL, opts);
w->status = WS_INDICT;
}
else if (dict->unknown_word_defined && dict->use_unknown_word)
{
we = build_word_expressions(sent, w, UNKNOWN_WORD, opts);
assert(we, UNKNOWN_WORD " has no expressions in the dictionary!");
w->status |= WS_UNKNOWN;
}
else
{
if (dictionary_word_is_known(dict, s))
{
prt_error("Internal Error: Word '%s' has bad status\n", s);
return false;
}
prt_error("Error: Word '%s': word is unknown\n", s);
return false;
}
if ((wordpos != *ZZZ_added) && is_exp_like_empty_word(dict, we->exp))
{
lgdebug(D_DWE, " (has ZZZ-)");
add_empty_word(sent, sent->word[wordpos-1].x);
*ZZZ_added = wordpos;
}
lgdebug(D_DWE, "\n");
sent->word[wordpos].x = catenate_X_nodes(sent->word[wordpos].x, we);
if (verbosity_level(D_X_NODE))
{
prt_error("Debug: Tokenize word/alt=%zu/%zu '%s' re=%s\n\\",
wordpos, altlen(sent->word[wordpos].alternatives),
s, w->regex_name ? w->regex_name : "");
while (we)
{
prt_error("Debug:  string='%s' status=%s expr=%s\n",
we->string, gword_status(sent, w), exp_stringify(we->exp));
we = we->next;
}
}
return true;
}
#undef D_DWE
#define D_BSE 8
bool build_sentence_expressions(Sentence sent, Parse_Options opts)
{
Dictionary dict = sent->dict;
for (size_t i=0; i<sent->length; i++)
{
Gword *gw = sent->word[i].gwords[0];
int igw = 0;
while (gw)
{
altappend(sent, &sent->word[i].alternatives, gw->subword);
igw ++;
gw = sent->word[i].gwords[igw];
}
}
bool have_unknown_words = false;
dict->start_lookup(dict, sent);
for (size_t i=0; i<sent->length; i++)
{
unsigned int ZZZ_added = 0;
Gword *gw = sent->word[i].gwords[0];
int igw = 0;
while (gw)
{
have_unknown_words |=
!determine_word_expressions(sent, gw, &ZZZ_added, opts);
igw ++;
gw = sent->word[i].gwords[igw];
}
}
dict->end_lookup(dict, sent);
lgdebug(+D_BSE, "sent->length %zu\n", sent->length);
if (verbosity_level(D_BSE))
{
dyn_str *s = dyn_str_new();
print_sentence_word_alternatives(s, sent, true, NULL, NULL, NULL);
char *out = dyn_str_take(s);
prt_error("Debug: Sentence words and alternatives:\n%s", out);
free(out);
}
return !have_unknown_words;
}
#undef D_BSE