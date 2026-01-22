#include "connectors.h"
#include "dialect.h"
#include "dict-affix.h"
#include "dict-api.h"
#include "dict-common.h"
#include "dict-defines.h"
#include "disjunct-utils.h"
#include "file-utils.h"
#include "post-process/pp_knowledge.h"
#include "regex-morph.h"
#include "string-set.h"
#include "tokenize/anysplit.h"
#include "tokenize/spellcheck.h"
#include "dict-sql/read-sql.h"
#include "dict-file/read-dict.h"
#include "dict-file/word-file.h"
#include "dict-atomese/read-atomese.h"
#define STEM_MARK '='
bool is_stem(const char* w)
{
const char *subscrmark = get_word_subscript(w);
if (NULL == subscrmark) return false;
if (subscrmark == w) return false;
if (STEM_MARK != subscrmark[1]) return false;
return true;
}
bool is_macro(const char *w)
{
if (w[0] == '<')
{
char *end = strchr(w, '>');
if (end == NULL) return false;
if ((end[1] == '\0') || (end[1] == SUBSCRIPT_MARK)) return true;
}
return false;
}
bool is_wall(const char *s)
{
if (0 == strncmp(s, LEFT_WALL_WORD, sizeof(LEFT_WALL_WORD)-1))
{
if (s[sizeof(LEFT_WALL_WORD)-1] == '\0' ||
(s[sizeof(LEFT_WALL_WORD)-1] == SUBSCRIPT_MARK)) return true;
}
if (0 == strncmp(s, RIGHT_WALL_WORD, sizeof(RIGHT_WALL_WORD)-1))
{
if (s[sizeof(RIGHT_WALL_WORD)-1] == '\0' ||
(s[sizeof(RIGHT_WALL_WORD)-1] == SUBSCRIPT_MARK)) return true;
}
return false;
}
Dictionary dictionary_create_default_lang(void)
{
Dictionary dictionary = NULL;
char * lang = get_default_locale();
if (lang && *lang)
{
lang[strcspn(lang, "_-")] = '\0';
dictionary = dictionary_create_lang(lang);
}
if ((NULL == dictionary) && ((lang == NULL) || (0 != strcmp(lang, "en"))))
{
dictionary = dictionary_create_lang("en");
}
free(lang);
return dictionary;
}
Dictionary dictionary_create_lang(const char * lang)
{
Dictionary dictionary = NULL;
object_open(NULL, NULL, NULL);
if (check_db(lang))
{
#if HAVE_SQLITE3
dictionary = dictionary_create_from_db(lang);
#else
return NULL;
#endif
}
else if (check_atomspace(lang))
{
#if HAVE_ATOMESE
dictionary = dictionary_create_from_atomese(lang);
#else
return NULL;
#endif
}
if (NULL == dictionary)
{
dictionary = dictionary_create_from_file(lang);
}
return dictionary;
}
const char * dictionary_get_lang(Dictionary dict)
{
if (!dict) return "";
return dict->lang;
}
Dict_node * dictionary_lookup_list(const Dictionary dict, const char *s)
{
return dict->lookup_list(dict, s);
}
Dict_node * dictionary_lookup_wild(const Dictionary dict, const char *s)
{
return dict->lookup_wild(dict, s);
}
void free_lookup_list(const Dictionary dict, Dict_node *llist)
{
dict->free_lookup(dict, llist);
}
bool dict_has_word(const Dictionary dict, const char *s)
{
return dict->exists_lookup(dict, s);
}
bool dictionary_word_is_known(const Dictionary dict, const char * word)
{
const char * regex_name;
if (dict_has_word(dict, word)) return true;
regex_name = match_regex(dict->regex_root, word);
if (NULL == regex_name) return false;
return dict_has_word(dict, regex_name);
}
const Category *dictionary_get_categories(const Dictionary dict)
{
if (dict->category == NULL) return NULL;
return dict->category + 1;
}
void dictionary_clear_cache(const Dictionary dict)
{
if (dict) dict->clear_cache(dict);
}
#ifdef USEFUL_BUT_NOT_CURRENTLY_USED
static bool find_one_non_idiom_node(Dict_node * p, Dict_node * dn,
const char * s,
Dict_node **parent, Dict_node **to_be_deleted)
{
int m;
if (dn == NULL) return false;
m = dict_order_bare(s, dn);
if (m <= 0) {
if (find_one_non_idiom_node(dn, dn->left, s, parent, to_be_deleted)) return true;
}
if ((m == 0) && (!is_idiom_word(dn->string))) {
*to_be_deleted = dn;
*parent = p;
return true;
}
if (m >= 0) {
if (find_one_non_idiom_node(dn, dn->right, s, parent, to_be_deleted)) return true;
}
return false;
}
static void set_parent_of_node(Dictionary dict,
Dict_node *p,
Dict_node * del,
Dict_node * newnode)
{
if (p == NULL) {
dict->root = newnode;
} else {
if (p->left == del) {
p->left = newnode;
} else if (p->right == del) {
p->right = newnode;
} else {
assert(false, "Dictionary broken?");
}
}
}
int delete_dictionary_words(Dictionary dict, const char * s)
{
Dict_node *pred, *pred_parent;
Dict_node *parent, *to_be_deleted;
if (!find_one_non_idiom_node(NULL, dict->root, s, &parent, &to_be_deleted)) return false;
for(;;) {
if (to_be_deleted->file != NULL) {
to_be_deleted->file->changed = true;
}
if (to_be_deleted->left == NULL) {
set_parent_of_node(dict, parent, to_be_deleted, to_be_deleted->right);
free(to_be_deleted);
} else {
pred_parent = to_be_deleted;
pred = to_be_deleted->left;
while(pred->right != NULL) {
pred_parent = pred;
pred = pred->right;
}
to_be_deleted->string = pred->string;
to_be_deleted->file = pred->file;
to_be_deleted->exp = pred->exp;
set_parent_of_node(dict, pred_parent, pred, pred->left);
free(pred);
}
if (!find_one_non_idiom_node(NULL, dict->root, s, &parent, &to_be_deleted)) return true;
}
}
#endif
static void affix_list_delete(Dictionary dict)
{
if (NULL == dict->afdict_class) return;
Afdict_class * atc = dict->afdict_class;
for (size_t i = 0; i < AFDICT_NUM_ENTRIES; i++)
{
if (atc[i].length > 0) free(atc[i].string);
if (atc[i].Nregexes > 0)
{
for (size_t r = 0; r < atc[i].Nregexes; r++)
free_regexs(atc[i].regex[r]);
free(atc[i].regex);
}
}
free(dict->afdict_class);
dict->afdict_class = NULL;
}
void dictionary_delete(Dictionary dict)
{
if (!dict) return;
if (verbosity >= D_USER_INFO) {
prt_error("Info: Freeing dictionary %s\n", dict->name);
}
if (dict->affix_table != NULL) {
affix_list_delete(dict->affix_table);
dictionary_delete(dict->affix_table);
}
affix_list_delete(dict);
spellcheck_destroy(dict->spell_checker);
if ((locale_t) 0 != dict->lctype) {
freelocale(dict->lctype);
}
condesc_delete(dict);
if (dict->close) dict->close(dict);
pp_knowledge_close(dict->base_knowledge);
pp_knowledge_close(dict->hpsg_knowledge);
string_set_delete(dict->string_set);
free_dialect(dict->dialect);
free(dict->dialect_tag.name);
string_id_delete(dict->dialect_tag.set);
if (dict->macro_tag != NULL) free(dict->macro_tag->name);
free(dict->macro_tag);
string_id_delete(dict->dfine.set);
free(dict->dfine.name);
free(dict->dfine.value);
free_regexs(dict->regex_root);
free_anysplit(dict);
free_Word_file(dict->word_file_header);
free_dictionary_root(dict);
for (unsigned int i = 1; i <= dict->num_categories; i++)
free(dict->category[i].word);
free(dict->category);
free(dict);
object_open(NULL, NULL, NULL);
}
bool dictionary_generation_request(const Dictionary dict)
{
const char *generation_mode = test_enabled("generate");
if (generation_mode != NULL)
{
dict->generate_walls =
feature_enabled(generation_mode, "walls", NULL) != NULL;
dict->spell_checker = NULL;
return true;
}
return false;
}