#include "api-structures.h"
#include "api-types.h"
#include "dict-api.h"
#include "dict-common.h"
#include "dict-internals.h"
#include "error.h"
#include "idiom.h"
#include "string-set.h"
bool contains_underbar(const char * s)
{
if ((s[0] == '_') || (s[0] == '\0')) return false;
while (*++s != '\0')
{
if (*s == SUBSCRIPT_MARK) return false;
if ((*s == '_') && (s[-1] != '\\')) return true;
}
return false;
}
static bool is_idiom_string(const char * s)
{
size_t len;
const char * t;
len = strlen(s);
if ((s[0] == '_') || (s[len-1] == '_'))
{
return false;
}
for (t = s; *t != '\0'; t++)
{
if (*s == SUBSCRIPT_MARK) return true;
if ((*t == '_') && (*(t+1) == '_')) return false;
}
return true;
}
static const char * build_idiom_word_name(Dictionary dict, const char * s)
{
size_t n = strlen(s);
char *buff = alloca(n+5);
strcpy(buff, s);
buff[n] = SUBSCRIPT_MARK;
buff[n + 1] = '_';
buff[n + 2] = 'I';
buff[n + 3] = '\0';
return string_set_add(buff, dict->string_set);
}
static Dict_node * make_idiom_Dict_nodes(Dictionary dict, const char * string)
{
Dict_node * dn = NULL;
char * s = strdupa(string);
const char * t;
const char *sm = get_word_subscript(s);
for (t = s; NULL != s; t = s)
{
s = strchr(s, '_');
if ((NULL != sm) && (s > sm)) s = NULL;
if (NULL != s) *s++ = '\0';
Dict_node *dn_new = dict_node_new();
dn_new->right = dn;
dn = dn_new;
dn->string = string_set_add(t, dict->string_set);
dn->file = NULL;
}
return dn;
}
static void increment_current_name(Dictionary dict)
{
short i = IDIOM_LINK_SZ-2;
do
{
dict->current_idiom[i]++;
if (dict->current_idiom[i] <= 'Z') return;
dict->current_idiom[i] = 'A';
} while (i-- > 0);
assert(0, "Overflow");
}
static const char * generate_id_connector(Dictionary dict)
{
char buff[IDIOM_LINK_SZ+4];
short i;
char * t;
for (i=0; dict->current_idiom[i] == 'A'; i++)
;
t = buff;
*t++ = '_';
*t++ = 'I';
for (; i < IDIOM_LINK_SZ; i++)
{
*t++ = dict->current_idiom[i];
}
*t++ = '\0';
return string_set_add(buff, dict->string_set);
}
void insert_idiom(Dictionary dict, Dict_node * dn)
{
Dict_node * dn_list, * xdn, * start_dn_list;
const char * s = dn->string;
if (!is_idiom_string(s))
{
prt_error("Warning: Word \"%s\" on line %d "
"is not a correctly formed idiom string.\n"
"\tThis word will be ignored\n",
s, dict->line_number);
return;
}
dn_list = start_dn_list = make_idiom_Dict_nodes(dict, s);
assert(dn_list->right != NULL, "Idiom string with only one connector");
Exp* nc = make_connector_node(dict, dict->Exp_pool,
generate_id_connector(dict), '-', false);
dn_list->exp = make_and_node(dict->Exp_pool, nc, dn->exp);
dn_list = dn_list->right;
while(dn_list->right != NULL)
{
nc = make_connector_node(dict, dict->Exp_pool,
generate_id_connector(dict), '+', false);
increment_current_name(dict);
Exp* no = make_connector_node(dict, dict->Exp_pool,
generate_id_connector(dict), '-', false);
dn_list->exp = make_and_node(dict->Exp_pool, nc, no);
dn_list = dn_list->right;
}
dn_list->exp = make_connector_node(dict, dict->Exp_pool,
generate_id_connector(dict), '+', false);
increment_current_name(dict);
dn_list = start_dn_list;
while (dn_list != NULL)
{
xdn = dn_list->right;
const char *word_name = build_idiom_word_name(dict, dn_list->string);
Dict_node *t = dictionary_lookup_list(dict, word_name);
if (NULL == t)
{
dn_list->left = dn_list->right = NULL;
dn_list->string = word_name;
dict->root = dict_node_insert(dict, dict->root, dn_list);
dict->num_entries++;
}
else
{
if (t->exp->type != OR_type)
{
t->exp = make_or_node(dict->Exp_pool, t->exp, NULL);
}
dn_list->exp = Exp_create_dup(dict->Exp_pool, dn_list->exp);
dn_list->exp->operand_next = t->exp->operand_first;
t->exp->operand_first = dn_list->exp;
t->left->exp = t->exp;
free_lookup_list(dict, t);
free(dn_list);
}
dn_list = xdn;
}
}
bool is_idiom_word(const char * s)
{
const char *sm = get_word_subscript(s);
if (NULL == sm) return false;
if ((sm[1] == '_') && (sm[2] == 'I') && (sm[3] == '\0')) return true;
return false;
}