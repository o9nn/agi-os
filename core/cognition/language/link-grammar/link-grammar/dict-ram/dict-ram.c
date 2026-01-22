#include <string.h>
#include "dict-common/dict-common.h"
#include "dict-common/dict-internals.h"
#include "dict-common/dict-utils.h"
#include "dict-common/idiom.h"
#include "string-id.h"
#include "string-set.h"
#include "dict-ram.h"
void free_dict_node_recursive(Dict_node * dn)
{
while (dn != NULL)
{
free_dict_node_recursive(dn->left);
Dict_node * rn = dn->right;
free(dn);
dn = rn;
}
}
void free_dictionary_root(Dictionary dict)
{
free_dict_node_recursive(dict->root);
pool_delete(dict->Exp_pool);
dict->root = NULL;
dict->Exp_pool = NULL;
}
NO_SAN_DICT
static inline int dict_order_strict(const char *s, const Dict_node * dn)
{
const char * t = dn->string;
while ((*s == *t) && (*s != '\0')) { s++; t++; }
return (*s - *t);
}
NO_SAN_DICT
static inline int dict_order_bare(const char *s, const Dict_node * dn)
{
const char * t = dn->string;
while ((*s == *t) && (*s != '\0')) { s++; t++; }
return (*s) - ((*t == SUBSCRIPT_MARK)?(0):(*t));
}
#define WILD_TYPE '*'
#define D_DOW 6
static inline int dict_order_wild(const char * s, const Dict_node * dn)
{
const char * t = dn->string;
lgdebug(+D_DOW, "search-word='%s' dict-word='%s'\n", s, t);
while((*s == *t) && (*s != SUBSCRIPT_MARK) && (*s != '\0')) { s++; t++; }
if (*s == WILD_TYPE) return 0;
lgdebug(D_DOW, "Result: '%s'-'%s'=%d\n",
s, t, ((*s == SUBSCRIPT_MARK)?(0):(*s)) - ((*t == SUBSCRIPT_MARK)?(0):(*t)));
return ((*s == SUBSCRIPT_MARK)?(0):(*s)) - ((*t == SUBSCRIPT_MARK)?(0):(*t));
}
#undef D_DOW
static bool subscr_match(const char *s, const Dict_node * dn)
{
const char * s_sub = get_word_subscript(s);
const char * t_sub = get_word_subscript(dn->string);
if (NULL == s_sub)
{
if (NULL == t_sub) return true;
return !is_idiom_word(t_sub);
}
if (NULL == t_sub) return false;
if (0 == strcmp(s_sub, t_sub)) return true;
return false;
}
static Dict_node *
rdictionary_lookup(Dict_node * restrict llist,
Dict_node * restrict dn,
const char * restrict s,
bool boolean_lookup,
int (*dict_order)(const char *, const Dict_node *))
{
if (dn == NULL) return llist;
int m = dict_order(s, dn);
while (m != 0)
{
if (m > 0)
dn = dn->right;
else if (m < 0)
dn = dn->left;
if (dn == NULL) return llist;
m = dict_order(s, dn);
}
if (dn->right)
llist = rdictionary_lookup(llist, dn->right, s, boolean_lookup, dict_order);
if (dict_order != dict_order_wild || subscr_match(s, dn))
{
dn->use_count++;
if (boolean_lookup) return dn;
Dict_node * dn_new = dict_node_new();
*dn_new = *dn;
dn_new->right = llist;
dn_new->left = dn;
llist = dn_new;
}
if (dn->left)
llist = rdictionary_lookup(llist, dn->left, s, boolean_lookup, dict_order);
return llist;
}
Dict_node * dict_node_lookup(const Dictionary dict, const char *s)
{
return rdictionary_lookup(NULL, dict->root, s, false, dict_order_bare);
}
bool dict_node_exists_lookup(Dictionary dict, const char *s)
{
return !!rdictionary_lookup(NULL, dict->root, s, true, dict_order_bare);
}
Dict_node * strict_lookup_list(const Dictionary dict, const char *s)
{
return rdictionary_lookup(NULL, dict->root, s, false, dict_order_strict);
}
Dict_node * dict_node_wild_lookup(Dictionary dict, const char *s)
{
char * ds = strrchr(s, SUBSCRIPT_DOT);
char * ws = strrchr(s, WILD_TYPE);
Dict_node * result;
char * stmp = strdupa(s);
if ((NULL != ds) && ('\0' != ds[1]) && ((NULL == ws) || (ds > ws)))
stmp[ds-s] = SUBSCRIPT_MARK;
result = rdictionary_lookup(NULL, dict->root, stmp, false, dict_order_wild);
return result;
}
Exp *Exp_create(Pool_desc *mp)
{
Exp *e = pool_alloc(mp);
e->tag_type = Exptag_none;
e->operand_next = NULL;
e->cost = 0.0;
return e;
}
Exp *Exp_create_dup(Pool_desc *mp, Exp *old_e)
{
Exp *new_e = pool_alloc(mp);
*new_e = *old_e;
return new_e;
}
Exp * make_zeroary_node(Pool_desc *mp)
{
Exp * n = Exp_create(mp);
n->type = AND_type;
n->operand_first = NULL;
return n;
}
Exp *make_unary_node(Pool_desc *mp, Exp * e)
{
Exp * n;
n = Exp_create(mp);
n->type = AND_type;
n->operand_first = e;
return n;
}
Exp * make_join_node(Pool_desc *mp, Exp* nl, Exp* nr, Exp_type t)
{
Exp* n;
n = Exp_create(mp);
n->type = t;
n->operand_first = nl;
nl->operand_next = nr;
return n;
}
Exp * make_and_node(Pool_desc *mp, Exp* nl, Exp* nr)
{
return make_join_node(mp, nl, nr, AND_type);
}
Exp * make_or_node(Pool_desc *mp, Exp* nl, Exp* nr)
{
return make_join_node(mp, nl, nr, OR_type);
}
Exp * make_connector_node(Dictionary dict, Pool_desc *mp,
const char* linktype, char dir, bool multi)
{
Exp* n = Exp_create(mp);
n->type = CONNECTOR_type;
n->condesc = condesc_add(&dict->contable,
string_set_add(linktype, dict->string_set));
n->dir = dir;
n->multi = multi;
return n;
}
Exp *make_optional_node(Pool_desc *mp, Exp *e)
{
return make_or_node(mp, make_zeroary_node(mp), e);
}
static Dict_node *rotate_right(Dict_node *root)
{
Dict_node *pivot = root->left;
root->left = pivot->right;
pivot->right = root;
return pivot;
}
Dict_node * dsw_tree_to_vine (Dict_node *root)
{
Dict_node *vine_tail, *vine_head, *rest;
Dict_node vh;
vine_head = &vh;
vine_head->left = NULL;
vine_head->right = root;
vine_tail = vine_head;
rest = root;
while (NULL != rest)
{
if (NULL == rest->left)
{
vine_tail = rest;
rest = rest->right;
}
else
{
rest = rotate_right(rest);
vine_tail->right = rest;
}
}
return vh.right;
}
NO_SAN_DICT
static void dsw_compression (Dict_node *root, unsigned int count)
{
unsigned int j;
for (j = 0; j < count; j++)
{
Dict_node * pivot = root->right;
root->right = pivot->right;
root = pivot->right;
pivot->right = root->left;
root->left = pivot;
}
}
static inline unsigned int full_tree_size (unsigned int size)
{
unsigned int pk = 1;
while (pk < size) pk = 2*pk + 1;
return pk/2;
}
Dict_node * dsw_vine_to_tree (Dict_node *root, int size)
{
Dict_node vine_head;
unsigned int full_count = full_tree_size(size +1);
vine_head.left = NULL;
vine_head.right = root;
dsw_compression(&vine_head, size - full_count);
for (size = full_count; size > 1; size /= 2)
{
dsw_compression(&vine_head, size / 2);
}
return vine_head.right;
}
static int dup_word_status(Dictionary dict, const Dict_node *newnode)
{
if (dict->allow_duplicate_words == dict->allow_duplicate_idioms)
return dict->allow_duplicate_words;
if (contains_underbar(newnode->string))
{
return dict->allow_duplicate_idioms;
}
else
{
return dict->allow_duplicate_words;
}
}
static bool dup_word_error(Dictionary dict, Dict_node *newnode)
{
if (dup_word_status(dict, newnode) == 1) return false;
if (dict->allow_duplicate_words == 0)
{
const char *s = linkgrammar_get_dict_define(dict, "allow-duplicate-words");
dict->allow_duplicate_words =
((s != NULL) && (0 == strcasecmp(s, "true"))) ? 1 : -1;
bool disallow_dup_idioms = !!test_enabled("disallow-dup-idioms");
dict->allow_duplicate_idioms = disallow_dup_idioms ? -1 : 1;
if (dup_word_status(dict, newnode) == 1) return false;
}
if (IS_DYNAMIC_DICT(dict))
{
prt_error("Error: While handling storage-node\n  \"%s\":\n"
"Ignoring word which has been multiply defined: \"%s\"\n",
dict->name, newnode->string);
} else {
prt_error("Error: While parsing dictionary \"%s\":\n"
"Ignoring word which has been multiply defined: \"%s\"\n"
"\t Line %d\n",
dict->name, newnode->string, dict->line_number);
}
newnode->exp = make_zeroary_node(dict->Exp_pool);
return true;
}
NO_SAN_DICT
Dict_node *dict_node_insert(Dictionary dict, Dict_node *n, Dict_node *newnode)
{
if (NULL == n) return newnode;
int comp = dict_order_strict(newnode->string, n);
if ((0 == comp) && dup_word_error(dict, newnode))
comp = -1;
if (comp < 0)
{
if (NULL == n->left)
{
n->left = newnode;
return n;
}
n->left = dict_node_insert(dict, n->left, newnode);
}
else
{
if (NULL == n->right)
{
n->right = newnode;
return n;
}
n->right = dict_node_insert(dict, n->right, newnode);
}
return n;
}
void add_define(Dictionary dict, const char *name, const char *value)
{
int id = string_id_add(name, dict->dfine.set);
if (dict->dfine.size >= (unsigned int)id)
{
prt_error("Warning: Redefinition of \"%s\", "
"found near line %d of \"%s\"\n",
name, dict->line_number, dict->name);
}
else
{
dict->dfine.size++;
dict->dfine.value =
realloc(dict->dfine.value, dict->dfine.size * sizeof(char *));
dict->dfine.name =
realloc(dict->dfine.name, dict->dfine.size * sizeof(char *));
assert(dict->dfine.size == (unsigned int)id,
"\"dfine\" array size inconsistency");
dict->dfine.name[id - 1] = string_set_add(name, dict->string_set);
}
dict->dfine.value[id - 1] = string_set_add(value, dict->string_set);
}
static bool is_directive(const char *s)
{
return
(strcmp(s, UNLIMITED_CONNECTORS_WORD) == 0) ||
(strncmp(s, LIMITED_CONNECTORS_WORD, sizeof(LIMITED_CONNECTORS_WORD)-1) == 0);
}
static bool is_correction(const char *s)
{
static const char correction_mark[] = { SUBSCRIPT_MARK, '#' , '\0'};
return strstr(s, correction_mark) != 0;
}
void add_category(Dictionary dict, Exp *e, Dict_node *dn, int n)
{
if (n == 1)
{
if (is_macro(dn->string)) return;
if (!dict->generate_walls && is_wall(dn->string)) return;
if (is_correction(dn->string)) return;
if (is_directive(dn->string)) return;
}
dict->num_categories++;
if (dict->num_categories >= dict->num_categories_alloced)
{
dict->num_categories_alloced *= 2;
dict->category =
realloc(dict->category,
sizeof(*dict->category) * dict->num_categories_alloced);
}
dict->category[dict->num_categories].word =
malloc(sizeof(*dict->category[0].word) * n);
n = 0;
for (Dict_node *dnx = dn; dnx != NULL; dnx = dnx->left)
{
if (is_macro(dnx->string)) continue;
if (!dict->generate_walls && is_wall(dnx->string)) continue;
if (is_correction(dnx->string)) continue;
if (is_directive(dnx->string)) return;
dict->category[dict->num_categories].word[n] = dnx->string;
n++;
}
if (n == 0)
{
free(dict->category[dict->num_categories].word);
--dict->num_categories;
}
else
{
assert(dict->num_categories < 1024 * 1024, "Insane number of categories");
char category_string[16];
snprintf(category_string, sizeof(category_string), " %x",
dict->num_categories);
string_set_add(category_string, dict->string_set);
dict->category[dict->num_categories].exp = e;
dict->category[dict->num_categories].num_words = n;
dict->category[dict->num_categories].name = "";
}
}
void print_dictionary_defines(Dictionary dict)
{
#define SPECIAL "(){};[]&^|:"
for (size_t i = 0; i < dict->dfine.size; i++)
{
const char *value = dict->dfine.value[i];
int q = (int)(strcspn(value, SPECIAL) == strlen(value));
printf("#define %s %s%s%s\n",
dict->dfine.name[i], &"\""[q], value, &"\""[q]);
}
}
static void rprint_dictionary_data(Dict_node * n)
{
if (n == NULL) return;
rprint_dictionary_data(n->left);
printf("%s: %s\n", n->string, exp_stringify(n->exp));
rprint_dictionary_data(n->right);
}
void print_dictionary_data(Dictionary dict)
{
rprint_dictionary_data(dict->root);
}